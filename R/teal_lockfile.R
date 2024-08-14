#' Generate lockfile for application's environment reproducibility
#'
#' @param lockfile_path (`character`) path to the lockfile (`"teal_app.lock"`).
#'
#' @section Different ways of creating lockfile:
#' `teal` leverages [renv::snapshot()], which offers multiple methods for lockfile creation.
#'
#' - User-specified:
#'     - **Pre-computed lockfile**: Users can provide their own pre-computed lockfile by specifying the path via
#'     `teal.renv.lockfile` option. Automatic lockfile computation is skipped in such case.
#' - Automatically computed:
#'     - **Working directory lockfile**: If `teal.renv.lockfile` is not set, `teal` will, by default, create an
#'      `implicit` type lockfile that uses `renv::dependencies()` to detect all R packages in the current project's
#'      working directory.
#'     - **`DESCRIPTION`-based lockfile**: To generate a lockfile based on a `DESCRIPTION` file in your working
#'     directory, set `renv::settings$snapshot.type("explicit")`. The naming convention for `type` follows
#'     `renv::snapshot()`. For the `"explicit"` type, refer to `renv::settings$package.dependency.fields()` for the
#'     `DESCRIPTION` fields included in the lockfile.
#'     - **Custom files-based lockfile**: To specify custom files as the basis for the lockfile, set
#'     `renv::settings$snapshot.type("custom")` and configure the `renv.snapshot.filter` option.
#'
#' @section lockfile usage:
#' After creating the lockfile, you can restore the application environment using `renv::restore()`.
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
    }
    disable_lockfile_download <- function() {
      warning("Lockfile creation failed.", call. = FALSE)
      shinyjs::html("lockFileStatus", "Lockfile creation failed.")
      shinyjs::disable("lockFileLink")
    }
    shiny::onStop(function() {
      if (file.exists(lockfile_path) && !shiny::isRunning()) {
        logger::log_debug("Removing lockfile after shutting down the app")
        file.remove(lockfile_path)
      }
    })

    # run renv::snapshot only once per app. Once calculated available for all users
    lockfile_path <- "teal_app.lock"
    if (file.exists(lockfile_path)) {
      logger::log_debug("Lockfile have been already created - skipping automatic creation.")
      enable_lockfile_download()
      return(NULL)
    }

    # don't run renv::snapshot when option is set
    user_lockfile <- getOption("teal.renv.lockfile", "")
    if (!identical(user_lockfile, "")) {
      if (file.exists(user_lockfile)) {
        file.copy(user_lockfile, lockfile_path)
        logger::log_debug('Lockfile set using option "teal.renv.lockfile" - skipping automatic creation.')
        enable_lockfile_download()
        return(NULL)
      } else {
        warning("lockfile provided through options('teal.renv.lockfile') does not exist.", call. = FALSE)
      }
    }

    # - Will be run only if the lockfile doesn't exist (see the if-s above)
    # - We render to the tempfile because the process might last after session is closed and we don't
    #   want to make a "teal_app.renv" then. This is why we copy only during active session.
    temp_lockfile_path <- tempfile()
    process <- .teal_lockfile_process_invoke(temp_lockfile_path)
    observeEvent(process$status(), {
      if (process$status() %in% c("initial", "running")) {
        shinyjs::html("lockFileStatus", "Creating lockfile...")
      } else if (process$status() == "success") {
        result <- process$result()
        if (any(grepl("Lockfile written to", result$out))) {
          logger::log_debug("Lockfile {result$path} containing { length(result$res$Packages) } packages created.")
          if (any(grepl("(WARNING|ERROR):", result$out))) {
            warning("Lockfile created with warning(s) or error(s):", call. = FALSE)
            for (i in result$out) {
              warning(i, call. = FALSE)
            }
          }
          file.copy(temp_lockfile_path, lockfile_path)
          enable_lockfile_download()
        } else {
          disable_lockfile_download()
        }
      } else if (process$status() == "error") {
        disable_lockfile_download()
      }
    })

    output$lockFileLink <- downloadHandler(
      filename = function() {
        "renv.lock"
      },
      content = function(file) {
        file.copy(lockfile_path, file)
        file
      },
      contentType = "application/json"
    )

    NULL
  })
}

utils::globalVariables(c("opts", "sysenv", "libpaths", "wd", "lockfilepath", "run")) # needed for mirai call
#' @rdname module_teal_lockfile
.teal_lockfile_process_invoke <- function(lockfile_path) {
  # to ignore running mirai process after app is closed
  mirai_obj <- mirai::mirai(
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
  shiny::onStop(function() {
    if (mirai::unresolved(mirai_obj)) {
      logger::log_debug("Terminating a running lockfile process...")
      mirai::stop_mirai(mirai_obj) # this doesn't stop running - renv will be created even if session is closed
    }
  })
  process <- ExtendedTask$new(function() mirai_obj)

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

  list(
    out = out,
    res = res,
    path = lockfile_path
  )
}
