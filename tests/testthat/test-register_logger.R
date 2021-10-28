testthat::test_that("register_logger accepts a scalar character", {
  withr::with_options(
    new = list(teal_logging_layout = NULL),
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
    withr::with_options(new = list(teal_logging_layout = stop), code = register_logger("test")),
    regexp = "Error setting the layout of the logger."
  )
})

testthat::test_that("An additional logging namespace is created after register_logger", {
  withr::with_options(
    new = list(teal_logging_layout = NULL),
    code = {
      register_logger("test")
      testthat::expect_true("test" %in% logger::log_namespaces())
    }
  )
})
