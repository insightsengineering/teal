#' Generate lockfile for application's environment reproducibility
#'
#' @section lockfile creation steps:
#' Process is split into multiple steps.
#'
#' 1. `teal_lockfile` is executed in [init] before application starts. It is better when process starts in
#' [init] as it is a one-time process and does not need to be repeated when each `shiny` session starts.
#' Function invokes `ExtendedTask` with `create_renv_lockfile` to begin creation of the lockfile.
#' 2. `ExtendedTask` with background `mirai` process is passed to the `lockfile_status_handler` to track the
#' status of the task.
#' 3. Once `ExtendedTask` is completed, `lockfile_status_handler` is triggered to log the status of the lockfile,
#' send a notification to UI and show the download button.
#' 4. `teal_lockfile_downloadhandler` is used to download the lockfile (when button is shown).
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
#' @return Nothing. This function is executed for its side effect of creating a lockfile used in the `teal` application.
#'
#' @keywords internal
teal_lockfile <- function() {
  lockfile_path <- "teal_app.lock"
  # If user has setup the file, there is no need to compute a new one.
  user_lockfile <- getOption("teal.renv.lockfile", "")
  if (!identical(user_lockfile, "")) {
    if (file.exists(user_lockfile)) {
      file.copy(user_lockfile, lockfile_path)
      logger::log_trace('Lockfile set using option "teal.renv.lockfile" - skipping automatic creation.')
      return(invisible(NULL))
    } else {
      stop("lockfile provided through options('teal.renv.lockfile') does not exist.")
    }
  }

  shiny::onStop(function() file.remove(lockfile_path))
  process <- ExtendedTask$new(function(...) {
    mirai::mirai(
      run(lockfile_path = lockfile_path, opts = opts, sysenv = sysenv, libpaths = libpaths, wd = wd),
      ...
    )
  })

  process$invoke(
    lockfile_path = lockfile_path,
    run = create_renv_lockfile,
    opts = options(),
    libpaths = .libPaths(),
    sysenv = as.list(Sys.getenv()) # normally output is a class of "Dlist",
    wd = getwd()
  )
  logger::log_trace("Lockfile creation started based on { getwd() }.")

  process
}

#' @rdname teal_lockfile
#' @keywords internal
create_renv_lockfile <- function(lockfile_path = NULL, opts, sysenv, libpaths, wd) {
  # todo: need to setwd() to
  checkmate::assert_string(lockfile_path)
  checkmate::assert_list(opts)
  checkmate::assert_class(sysenv, "list")
  checkmate::assert_character(libpaths, min.len = 1)
  checkmate::assert_directory(wd)
  options(opts)
  lapply(names(sysenv), function(sysvar) do.call(Sys.setenv, sysenv[sysvar]))
  .libPaths(libpaths)
  setwd(wd)

  out <- capture.output(
    renv <- renv::snapshot(
      lockfile = lockfile_path,
      prompt = FALSE,
      force = TRUE
      # type = is taken from renv::settings$snapshot.type()
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

#' @rdname teal_lockfile
#' @keywords internal
lockfile_status_handler <- function(process) {
  renv_logs <- process$result()
  message <- if (any(grepl("Lockfile written to", renv_logs$out))) {
    with <- if (any(grepl("WARNING:", renv_logs$out))) {
      "with warning(s)"
    } else if (any(grepl("ERROR:", renv_logs$out))) {
      "with error(s)"
    } else {
      ""
    }

    message <- sprintf(
      "Lockfile %s containing %s packages saved %s.",
      renv_logs$lockfile_path,
      renv_logs$length,
      with
    )
    shinyjs::show(selector = "#teal-lockFile")
  } else {
    "Lockfile creation failed."
  }
  logger::log_trace(message)
  shiny::showNotification(message)
}

#' @rdname teal_lockfile
#' @keywords internal
lockfile_status_tracker <- function(process) {
  if (!is.null(isolate(process))) {
    tracker <- observeEvent(process$status(), {
      if (process$status() != "running") {
        lockfile_status_handler(process)
      }
    })
  }
}
