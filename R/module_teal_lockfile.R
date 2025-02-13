#' Generate lockfile for application's environment reproducibility
#'
#' @inheritParams module_teal
#' @param lockfile_path (`character`) path to the lockfile.
#'
#' @section Different ways of creating lockfile:
#' `teal` leverages [renv::snapshot()], which offers multiple methods for lockfile creation.
#'
#' - **Working directory lockfile**: `teal`, by default, will create an `implicit` type lockfile that uses
#' `renv::dependencies()` to detect all R packages in the current project's working directory.
#' - **`DESCRIPTION`-based lockfile**: To generate a lockfile based on a `DESCRIPTION` file in your working
#' directory, set `renv::settings$snapshot.type("explicit")`. The naming convention for `type` follows
#' `renv::snapshot()`. For the `"explicit"` type, refer to `renv::settings$package.dependency.fields()` for the
#' `DESCRIPTION` fields included in the lockfile.
#' - **Custom files-based lockfile**: To specify custom files as the basis for the lockfile, set
#' `renv::settings$snapshot.type("custom")` and configure the `renv.snapshot.filter` option.
#'
#' @section lockfile usage:
#' After creating the lockfile, you can restore the application's environment using `renv::restore()`.
#'
#' @seealso [renv::snapshot()], [renv::restore()].
#'
#' @return `NULL`
#'
#' @name module_teal_lockfile
#' @rdname module_teal_lockfile
#'
#' @keywords internal
NULL

#' @rdname module_teal_lockfile
ui_teal_lockfile <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    tags$span("", id = ns("lockFileStatus")),
    shinyjs::disabled(downloadLink(ns("lockFileLink"), "Download lockfile"))
  )
}

#' @rdname module_teal_lockfile
srv_teal_lockfile <- function(id) {
  moduleServer(id, function(input, output, session) {
    logger::log_debug("Initialize srv_teal_lockfile.")
    enable_lockfile_download <- function() {
      shinyjs::html("lockFileStatus", "Application lockfile ready.")
      shinyjs::hide("lockFileStatus", anim = TRUE)
      shinyjs::enable("lockFileLink")
      output$lockFileLink <- shiny::downloadHandler(
        filename = function() {
          "renv.lock"
        },
        content = function(file) {
          file.copy(lockfile_path, file)
          file
        },
        contentType = "application/json"
      )
    }
    disable_lockfile_download <- function() {
      warning("Lockfile creation failed.", call. = FALSE)
      shinyjs::html("lockFileStatus", "Lockfile creation failed.")
      shinyjs::hide("lockFileLink")
    }

    shiny::onStop(function() {
      if (file.exists(lockfile_path) && !shiny::isRunning()) {
        logger::log_debug("Removing lockfile after shutting down the app")
        file.remove(lockfile_path)
      }
    })

    lockfile_path <- "teal_app.lock"
    mode <- getOption("teal.lockfile.mode", default = "")

    if (!(mode %in% c("auto", "enabled", "disabled"))) {
      stop("'teal.lockfile.mode' option can only be one of \"auto\", \"disabled\" or \"disabled\". ")
    }

    if (mode == "disabled") {
      logger::log_debug("'teal.lockfile.mode' option is set to 'disabled'. Hiding lockfile download button.")
      shinyjs::hide("lockFileLink")
      return(NULL)
    }

    if (file.exists(lockfile_path)) {
      logger::log_debug("Lockfile has already been created for this app - skipping automatic creation.")
      enable_lockfile_download()
      return(NULL)
    }

    if (mode == "auto" && .is_disabled_lockfile_scenario()) {
      logger::log_debug(
        "Automatic lockfile creation disabled. Execution scenario satisfies teal:::.is_disabled_lockfile_scenario()."
      )
      shinyjs::hide("lockFileLink")
      return(NULL)
    }

    if (!.is_lockfile_deps_installed()) {
      warning("Automatic lockfile creation disabled. `mirai` and `renv` packages must be installed.")
      shinyjs::hide("lockFileLink")
      return(NULL)
    }

    # - Will be run only if the lockfile doesn't exist (see the if-s above)
    # - We render to the tempfile because the process might last after session is closed and we don't
    #   want to make a "teal_app.renv" then. This is why we copy only during active session.
    process <- .teal_lockfile_process_invoke(lockfile_path)
    observeEvent(process$status(), {
      if (process$status() %in% c("initial", "running")) {
        shinyjs::html("lockFileStatus", "Creating lockfile...")
      } else if (process$status() == "success") {
        result <- process$result()
        if (any(grepl("Lockfile written to", result$out))) {
          logger::log_debug("Lockfile containing { length(result$res$Packages) } packages created.")
          if (any(grepl("(WARNING|ERROR):", result$out))) {
            warning("Lockfile created with warning(s) or error(s):", call. = FALSE)
            for (i in result$out) {
              warning(i, call. = FALSE)
            }
          }
          enable_lockfile_download()
        } else {
          disable_lockfile_download()
        }
      } else if (process$status() == "error") {
        disable_lockfile_download()
      }
    })

    NULL
  })
}

utils::globalVariables(c("opts", "sysenv", "libpaths", "wd", "lockfilepath", "run")) # needed for mirai call
#' @rdname module_teal_lockfile
.teal_lockfile_process_invoke <- function(lockfile_path) {
  mirai_obj <- NULL
  process <- shiny::ExtendedTask$new(function() {
    m <- mirai::mirai(
      {
        options(opts)
        do.call(Sys.setenv, sysenv)
        .libPaths(libpaths)
        setwd(wd)
        run(lockfile_path = lockfile_path)
      },
      run = .renv_snapshot,
      lockfile_path = lockfile_path,
      opts = options(),
      libpaths = .libPaths(),
      sysenv = as.list(Sys.getenv()),
      wd = getwd()
    )
    mirai_obj <<- m
    m
  })

  shiny::onStop(function() {
    if (mirai::unresolved(mirai_obj)) {
      logger::log_debug("Terminating a running lockfile process...")
      mirai::stop_mirai(mirai_obj) # this doesn't stop running - renv will be created even if session is closed
    }
  })

  suppressWarnings({ # 'package:stats' may not be available when loading
    process$invoke()
  })

  logger::log_debug("Lockfile creation started based on { getwd() }.")

  process
}

#' @rdname module_teal_lockfile
.renv_snapshot <- function(lockfile_path) {
  out <- utils::capture.output(
    res <- renv::snapshot(
      lockfile = lockfile_path,
      prompt = FALSE,
      force = TRUE,
      type = renv::settings$snapshot.type() # see the section "Different ways of creating lockfile" above here
    )
  )

  list(out = out, res = res)
}

#' @rdname module_teal_lockfile
.is_lockfile_deps_installed <- function() {
  requireNamespace("mirai", quietly = TRUE) && requireNamespace("renv", quietly = TRUE)
}

#' @rdname module_teal_lockfile
.is_disabled_lockfile_scenario <- function() {
  identical(Sys.getenv("CALLR_IS_RUNNING"), "true") || # inside callr process
    identical(Sys.getenv("TESTTHAT"), "true") || # inside devtools::test
    !identical(Sys.getenv("QUARTO_PROJECT_ROOT"), "") || # inside Quarto process
    (
      ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))
    ) # inside R CMD CHECK
}
