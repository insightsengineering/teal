testthat::test_that("log_app_usage throws a depreciation warning", {
  lifecycle::expect_deprecated(log_app_usage("test", "test", "test", "test"))
})
