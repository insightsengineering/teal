#' Create application `.lockfile`
#'
#' This function is used during `teal::init` and enables to create `renv` `.lockfile` to be shared in the `teal` app.
#'
#' The process uses [renv::snapshot()], which allows multiple ways of `.lockfile` creation.
#'
#' - **Pre-computed `.lockfile`**: User is able to provide his own pre-computed `.lockfile` by setting the path to the
#' `.lockfile` through `options(teal.renv.lockfile = "")`. Then this function is not used.
#' - **Working directory `.lockfile`**: If `options(teal.renv.lockfile)` is empty, by default `teal` will
#' create an `implicit` type of the `.lockfile`, that uses `renv::dependencies()` to detect all R packages in the
#' current project working directory.
#' - **`DESCRIPTION` based `.lockfile`**: You can always include a `DESCRIPTION` file in your working directory and
#' enable `.lockfile` creation based on this file. To do this, run `renv::settings$snapshot.type("explicit")`.
#' Naming of `type` is the same as in `renv::snapshot()`. For the `"explicit"` type refer to
#' `renv::settings$package.dependency.fields()`
#' to see what `DESCRIPTION` fields are included in the `.lockfile`.
#' - **Custom files based `.lockfile`**: If you want to specify custom files as a base for the `.lockfile`, then run
#' `renv::settings$snapshot.type("custom")` and set `renv.snapshot.filter` option.
#'
#' @note
#' This function computes the `.lockfile` as a `promises::future_promise` promise, while
#' running the evaluation of the process on a separate worker. If `future::plan()` was set to something different than
#' `future::sequential`, it reuses the parallel backed. If not, it sets `future::multisession` as a parallel plan with
#' 2 workers. Then, `shiny::ExtendedTask()` is used to run asynchronous computations. If the `teal` app started with
#' `future::sequential` plan, is it set back once the task is finished.
#'
#' @section `.lock` file usage:
#' Once you have the `.lock` file, you can restore the application environment with `renv::restore()`.
#'
#' @seealso [renv::snapshot()], [renv::restore()].
#'
#' @return Nothing is returned. The side effect of this function is the creation of
#' `getOption("teal.internal.renv.lockfile")` file, which will be shared through the `teal` app.
#'
#' @keywords internal
teal_lockfile <- function() {
  # If user has setup the file, there is no need to compute a new one.
  user_lockfile <- getOption("teal.renv.lockfile", "")
  if (!identical(user_lockfile, "") && !file.exists(user_lockfile)) {
    stop("lockfile provided through options('teal.renv.lockfile') does not exist.")
  }

  if (!(file.exists(user_lockfile) || is_in_test() || is_r_cmd_check())) {
    old_plan <- future::plan()
    # If there is already a parallel backend, reuse it.
    if (inherits(old_plan, "sequential")) {
      future::plan(future::multisession, workers = 2)
    }

    lockfile_task <- ExtendedTask$new(create_renv_lockfile)
    lockfile_task$invoke(inherits(old_plan, "sequential"))
    logger::log_trace("lockfile creation invoked.")
  }
}

create_renv_lockfile <- function(close) {
  checkmate::assert_flag(close)
  promise <- promises::future_promise({
    # Below is not a file in tempdir() directory.
    # If a file is created in tempdir() it gets deleted on 'then(onFulfilled' part.
    lockfile_path <- getOption("teal.internal.renv.lockfile")
    shiny::onStop(function() file.remove(lockfile_path))

    renv_logs <- utils::capture.output(
      renv::snapshot(
        lockfile = lockfile_path,
        prompt = FALSE,
        force = TRUE
        # type = is taken from renv::settings$snapshot.type()
      )
    )
    if (any(grepl("Lockfile written", renv_logs))) {
      logger::log_trace("lockfile created successfully.")
    } else {
      logger::log_trace("lockfile created with issues.")
    }

    lockfile_path
  })
  if (close) {
    promises::then(promise, onFulfilled = function() {
      future::plan(future::sequential)
    })
  }
  promise
}

is_r_cmd_check <- function() {
  ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))
}

is_in_test <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}
