#' Generate lockfile for application reproducibility
#'
#' This function is invoked during [teal::init] to create `renv`-compatible lockfile for use within the application.
#'
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
      return(invisible(NULL))
    } else {
      stop("lockfile provided through options('teal.renv.lockfile') does not exist.")
    }
  }

  shiny::onStop(function() file.remove(lockfile_path))
  callr::r_bg(create_renv_lockfile, args = list(lockfile_path = lockfile_path))
  logger::log_trace("lockfile creation started.")
}

create_renv_lockfile <- function(lockfile_path = NULL) {
  checkmate::assert_string(lockfile_path, na.ok = TRUE)

  renv_logs <- utils::capture.output(
    renv::snapshot(
      lockfile = lockfile_path,
      prompt = FALSE,
      force = TRUE
      # type = is taken from renv::settings$snapshot.type()
    )
  )
  lockfile_path
}

teal_lockfile_downloadhandler <- function() {
  downloadHandler(
    filename = function() {
      "renv.lock"
    },
    content = function(file) {
      teal_lockfile <- "teal_app.lock"
      iter <- 1
      while (!file.exists(teal_lockfile) && iter <= 100) {
        logger::log_trace("lockfile not created yet, retrying...")
        Sys.sleep(0.25)
        iter <- iter + 1 # max wait time is 25 seconds
      }
      file.copy(teal_lockfile, file)
      file
    },
    contentType = "application/json"
  )
}
