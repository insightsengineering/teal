testthat::test_that("init_filter_state accepts a string or name as varname", {
  testthat::expect_error(init_filter_state(7, varname = "test"), NA)
  testthat::expect_error(init_filter_state(7, varname = quote(test)), NA)
})
