#' Returns testing depth set by an environmental variable.
#'
#' @details Looks for the option `TESTING_DEPTH` first, if not set,
#' takes the system environmental variable `TESTING_DEPTH`. If neither
#' is set, then returns 3 by default. If the value of `TESTING_DEPTH`
#' is not a scalar numeric, then returns 3.
#'
#' @return `numeric(1)` the testing depth.
#'
testing_depth <- function() { # nolint # nousage
  testing_depth <- getOption("TESTING_DEPTH")
  if (is.null(testing_depth)) testing_depth <- Sys.getenv("TESTING_DEPTH")

  testing_depth <- tryCatch(
    as.numeric(testing_depth),
    error = function(error) 3,
    warning = function(warning) 3
  )

  if (length(testing_depth) != 1 || is.na(testing_depth)) testing_depth <- 3

  testing_depth
}

#' Skipping tests in the testthat pipeline under specific scope
#'
#' @description This function should be used per each `testthat::test_that` call.
#'   Each of the call should specify an appropriate depth value.
#'   The depth value will set the appropriate scope so more/less time consuming tests could be recognized.
#'   The environment variable `TESTING_DEPTH` is used for changing the scope of `testthat` pipeline.
#' `TESTING_DEPTH` interpretation for each possible value:
#' - no tests at all}
#' - fast - small scope - executed on every commit
#' - medium - medium scope - daily integration pipeline
#' - slow - all tests - daily package tests
#' }
#' @param depth `numeric` the depth of the testing evaluation,
#'   has opposite interpretation to environment variable `TESTING_DEPTH`.
#'   So e.g. `0` means run it always and `5` means a heavy test which should be run rarely.
#'   If the `depth` argument is larger than `TESTING_DEPTH` then the test is skipped.
#' @importFrom testthat skip
#' @return `NULL` or invoke an error produced by `testthat::skip`.
#' @note By default `TESTING_DEPTH` is equal to 3 if there is no environment variable for it.
#' By default `depth` argument lower or equal to 3 will not be skipped because by default `TESTING_DEPTH`
#' is equal to 3. To skip <= 3 depth tests then the environment variable has to be lower than 3 respectively.
skip_if_too_deep <- function(depth) { # nolintr
  checkmate::assert_number(depth, lower = 0, upper = 5)
  test_to_depth <- testing_depth() # by default 3 if there are no env variable
  if (test_to_depth < depth) {
    testthat::skip(paste("testing depth", test_to_depth, "is below current testing specification", depth))
  }
}
