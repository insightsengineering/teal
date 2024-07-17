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
teal_lockfile_invoke <- function() {
  lockfile_path <- "teal_app.lock"
  shiny::onStop(function() file.remove(lockfile_path))

  # If user has setup the file, there is no need to compute a new one.
  user_lockfile <- getOption("teal.renv.lockfile", "")
  if (!identical(user_lockfile, "")) {
    if (file.exists(user_lockfile)) {
      file.copy(user_lockfile, lockfile_path)
      logger::log_trace('Lockfile set using option "teal.renv.lockfile" - skipping automatic creation.')
      return(NULL)
    } else {
      stop("lockfile provided through options('teal.renv.lockfile') does not exist.")
    }
  }


  run_renv_mirai <- function(lockfile_path, opts, sysenv, libpaths, wd) {
    mirai::mirai(
      renv_snapshot(
        lockfile_path = lockfile_path,
        opts = opts,
        sysenv = sysenv,
        libpaths = libpaths,
        wd = wd
      ),
    )
  }

  process <- ExtendedTask$new(run_renv_mirai)

  process$invoke(
    lockfile_path = lockfile_path,
    opts = options(),
    libpaths = .libPaths(),
    sysenv = as.list(Sys.getenv()), # normally output is a class of "Dlist"
    wd = getwd()
  )
  logger::log_trace("Lockfile creation started based on { getwd() }.")

  process
}

#' @rdname teal_lockfile
#' @keywords internal
renv_snapshot <- function(lockfile_path = NULL, opts, sysenv, libpaths, wd) {
  checkmate::assert_string(lockfile_path)
  checkmate::assert_list(opts)
  checkmate::assert_class(sysenv, "list")
  checkmate::assert_character(libpaths, min.len = 1)
  checkmate::assert_directory(wd)

  # mirai starts in vanilla session in the R.home directory. We need to pass all session related info
  options(opts)
  do.call(Sys.setenv, sysenv)
  .libPaths(libpaths)
  setwd(wd)

  out <- utils::capture.output(
    renv <- renv::snapshot(
      lockfile = lockfile_path,
      prompt = FALSE,
      force = TRUE
      # type is taken from renv::settings$snapshot.type()
    )
  )

  list(
    out = out,
    lockfile_path = lockfile_path,
    length = length(renv$Packages)
  )
}

#' @rdname teal_lockfile
#' @keywords internal
teal_lockfile_tracker <- function(process) {
  checkmate::assert_class(process, "ExtendedTask", null.ok = TRUE)
  if (inherits(process, "ExtendedTask")) {
    tracker <- observeEvent(process$status(), {
      if (process$status() != "running") {
        teal_lockfile_handler(process)
      }
    })
  } else {
    # If it is not mirai proces, then lockfile was provided through options. Skip the process - file is ready.
    shinyjs::show("teal-lockFile")
  }
}

#' @rdname teal_lockfile
#' @keywords internal
teal_lockfile_handler <- function(process) {
  renv_logs <- process$result()
  if (any(grepl("Lockfile written to", renv_logs$out))) {
    with <- if (any(grepl("WARNING:", renv_logs$out))) {
      " with warning(s)"
    } else if (any(grepl("ERROR:", renv_logs$out))) {
      " with error(s)"
    } else {
      ""
    }

    logger::log_trace("Lockfile {renv_logs$lockfile_path} containing { renv_logs$length } packages created{ with }.")
    shiny::showNotification(paste0("Lockfile created", with, " and available to download."))
    shinyjs::show("teal-lockFile")
  } else {
    warning("Lockfile creation failed.")
    shiny::showNotification("Lockfile creation failed.", type = "warning")
  }
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
