testthat::test_that("register_logger accepts a scalar character", {
  withr::with_options(
    new = list(teal.log_layout = NULL),
    code = testthat::expect_error(register_logger(namespace = "test"), NA)
  )
})

testthat::test_that("register_logger throws an error when passed an invalid namespace argument", {
  testthat::expect_error(
    register_logger(namespace = 7),
    regexp = "namespace argument to register_logger must be a scalar character or NA."
  )
})

testthat::test_that("register_logger throws an error when setting the logger layout fails", {
  testthat::expect_error(
    withr::with_options(new = list(teal.log_layout = stop), code = register_logger("test")),
    regexp = "Error setting the layout of the logger."
  )
})

testthat::test_that("register_logger throws an error if the passed log level is invalid", {
  testthat::expect_error(
    register_logger("test", level = "WRONG level"),
    regexp = "The log level passed to logger::log_threshold was invalid"
  )
})

testthat::test_that("An additional logging namespace is created after register_logger", {
  withr::with_options(
    new = list(teal.log_layout = NULL),
    code = {
      register_logger("test")
      testthat::expect_true("test" %in% logger::log_namespaces())
    }
  )
})

testthat::test_that("register_logger does not throw if passed a correct logger layout", {
  withr::with_options(
    new = list(teal.log_level = logger::INFO),
    code = testthat::expect_error(register_logger(namespace = "test", layout = "{msg}"), NA)
  )
})

testthat::test_that("register_logger does not throw if passed a correct logger level", {
  withr::with_options(
    new = list(teal.log_layout = "{msg}"),
    code = testthat::expect_error(register_logger(namespace = "test", level = logger::INFO), NA)
  )
})

testthat::test_that("register_logger does not throw if passed NULL to either log_layout or log_level
  and valid options are set", {
  withr::with_options(
    new = list(teal.log_layout = "{msg}", teal.log_level = logger::INFO),
    code = testthat::expect_error(
      register_logger(namespace = "test", level = NULL, layout = NULL),
      NA
    )
  )
})

testthat::test_that("register_logger does not throw if passed NULL to arguments, options are not set and
  the system variables are set to valid value", {
  withr::local_envvar(.new = list(TEAL.LOG_LAYOUT = "{msg}", TEAL.LOG_LEVEL = "INFO"))
  withr::local_options(.new = list(teal.log_layout = NULL, teal.log_level = NULL))
  testthat::expect_error(register_logger(namespace = "test", layout = NULL, level = NULL), NA)
})

testthat::test_that("log_system_info does not throw an error", {
  testthat::expect_error(log_system_info(), NA)
})
