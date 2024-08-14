#' Generate lockfile for application's environment reproducibility
#'
#' @param process (`mirai`) process to track the status of the lockfile creation.
#' @param lockfile_path (`character`) path to the lockfile (`"teal_app.lock"`).
#' @param opts (`list`) options to be set in the [mirai::daemon()].
#' @param sysenv (`list`) system environment variables to be set in the [mirai::daemon()].
#' @param libpaths (`character`) library paths to be set in the [mirai::daemon()].
#' @param wd (`character(1)`) working directory to be set in the [mirai::daemon()].
#'
#' @section lockfile creation steps:
#' Process is split into multiple steps.
#'
#' 1. `teal_lockfile_invoke` is executed in [init] before application starts. It is better when task starts in
#' [init] as it is a one-time process and does not need to be repeated when each `shiny` session starts.
#' Function invokes `renv_snapshot` (via `ExtendedTask`) to begin creation of the lockfile.
#' 2. `ExtendedTask` with background `mirai` process is passed to the `teal_lockfile_handler` to track the
#' status of the task.
#' 3. Once `ExtendedTask` is completed, `teal_lockfile_handler` is triggered to log the status of the lockfile,
#' send a notification to UI and show the download button.
#' 4. `teal_lockfile_downloadhandler` is used to download the lockfile (when button is shown).
#'
#' Please note that if pre-computed lockfile file path has been provided through `teal.renv.lockfile` option, then
#' whole process is skipped and download lockfile button becomes available.
#'
#' @section Different ways of creating lockfile:
#' The function leverages [renv::snapshot()], which offers multiple methods for lockfile creation.
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
#' @return
#'  `ExtendedTask` processing `renv` lockfile or `NULL` if lockfile has been provided through options
#'  (skipping asynchronous process).
#'
#' @name teal_lockfile
#' @rdname teal_lockfile
#'
#' @keywords internal
NULL

#' @rdname teal_lockfile
#' @keywords internal
teal_lockfile <- function() {
  lockfile_path <- "teal_app.lock"
  shiny::onStop(function() {
    if (file.exists(lockfile_path)) {
      file.remove(lockfile_path)
    }
  })

  user_lockfile <- getOption("teal.renv.lockfile", "")
  if (!identical(user_lockfile, "")) {
    teal_lockfile_external()
  } else {
    teal_lockfile_process_invoke()
  }
}

#' @rdname teal_lockfile
#' @keywords internal
teal_lockfile_external <- function() {
  lockfile_path <- "teal_app.lock"
  user_lockfile <- getOption("teal.renv.lockfile", "")

  if (file.exists(user_lockfile)) {
    file.copy(user_lockfile, lockfile_path)
    shinyjs::enable("teal-lockFileLink")
    logger::log_trace('Lockfile set using option "teal.renv.lockfile" - skipping automatic creation.')
    return(NULL)
  } else {
    stop("lockfile provided through options('teal.renv.lockfile') does not exist.")
  }
}

#' @rdname teal_lockfile
#' @keywords internal
teal_lockfile_process_invoke <- function() {
  lockfile_path <- "teal_app.lock"

  process <- ExtendedTask$new(
    function() {
      mirai::mirai(
        {
          options(opts)
          do.call(Sys.setenv, sysenv)
          .libPaths(libpaths)
          setwd(wd)

          run(lockfile_path = lockfile_path)
        },
        run = renv_snapshot,
        lockfile_path = lockfile_path,
        opts = options(),
        libpaths = .libPaths(),
        sysenv = as.list(Sys.getenv()),
        wd = getwd()
      )
    }
  )

  suppressWarnings({ # 'package:stats' may not be available when loading
    process$invoke()
  })

  logger::log_trace("Lockfile creation started based on { getwd() }.")

  process
}

#' @rdname teal_lockfile
#' @keywords internal
renv_snapshot <- function(lockfile_path) {
  checkmate::assert_string(lockfile_path)

  out <- utils::capture.output(
    res <- renv::snapshot(
      lockfile = lockfile_path,
      prompt = FALSE,
      force = TRUE
      # type is taken from renv::settings$snapshot.type()
    )
  )

  list(
    out = out,
    res = res,
    path = lockfile_path
  )
}

#' @rdname teal_lockfile
#' @keywords internal
teal_lockfile_process_tracker <- function(process) {
  checkmate::assert_class(process, "ExtendedTask")

  observeEvent(process$status(), {
    if (process$status() %in% c("initial", "running")) {
      shinyjs::html("teal-lockFileStatus", "Creating lockfile...")
    } else if (process$status() == "success") {
      result <- process$result()
      if (any(grepl("Lockfile written to", result$out))) {
        logger::log_trace("Lockfile {result$path} containing { length(result$res$Packages) } packages created.")
        if (any(grepl("(WARNING|ERROR):", result$out))) {
          logger::log_warn("Lockfile created with warning(s) or error(s):")
          for (i in result$out) {
            logger::log_warn(i)
          }
        }

        shinyjs::html("teal-lockFileStatus", "Application lockfile ready.")
        shinyjs::hide("teal-lockFileStatus", anim = TRUE)

        shinyjs::enable("teal-lockFileLink")
      } else {
        warning("Lockfile creation failed.")
        shinyjs::html("teal-lockFileStatus", "Lockfile creation failed.")
        shinyjs::disable("teal-lockFileLink")
      }
    } else if (process$status() == "error") {
      warning("Lockfile creation failed.")
      shinyjs::html("teal-lockFileStatus", "Lockfile creation failed.")
      shinyjs::disable("teal-lockFileLink")
    }
  })
}

#' @rdname teal_lockfile
#' @keywords internal
teal_lockfile_downloadhandler <- function() {
  downloadHandler(
    filename = function() {
      "renv.lock"
    },
    content = function(file) {
      teal_lockfile <- "teal_app.lock"
      file.copy(teal_lockfile, file)
      file
    },
    contentType = "application/json"
  )
}
