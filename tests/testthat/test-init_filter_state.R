testthat::test_that("init_filter_state accepts a string or name as varname", {
  testthat::expect_error(init_filter_state(7, varname = "test"), NA)
  testthat::expect_error(init_filter_state(7, varname = quote(test)), NA)
})

testthat::test_that("init_filter_state accepts a character vector of length 0 or 1 as varlabel", {
  testthat::expect_error(init_filter_state(7, varname = "test", varlabel = "test"), NA)
  testthat::expect_error(init_filter_state(7, varname = "test", varlabel = character(0)), NA)
})

testthat::test_that("init_filter_state accepts NULL, name or call as input_dataname", {
  testthat::expect_error(init_filter_state(7, varname = "test", input_dataname = NULL), NA)
  testthat::expect_error(init_filter_state(7, varname = "test", input_dataname = quote(test)), NA)
  testthat::expect_error(init_filter_state(7, varname = "test", input_dataname = call("test")), NA)
})

testthat::test_that("init_filter_state accepts logicalas use_dataname", {
  testthat::expect_error(init_filter_state(7, varname = "test", use_dataname = TRUE), NA)
  testthat::expect_error(init_filter_state(7, varname = "test", use_dataname = FALSE), NA)
})

testthat::test_that("init_filter_state provides default values for varlabel, input_dataname, use_datname", {
  filter_state <- init_filter_state(7, varname = "test")
  testthat::expect_equal(filter_state$get_varlabel(), character(0))
  testthat::expect_equal(filter_state$get_dataname(deparse = TRUE), "NULL")
  testthat::expect_equal(filter_state$get_dataname(deparse = FALSE), NULL)
})

testthat::test_that("init_filter_state returns an EmptyFilterState if all values provided are NA", {
  testthat::expect_true(is(init_filter_state(NA, varname = "test"), "EmptyFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed a numeric array of length < 5", {
  testthat::expect_true(is(init_filter_state(c(1, 2, 3, 4), varname = "test"), "ChoicesFilterState"))
  testthat::expect_true(is(init_filter_state(c(1, 2, 3, 4, 5), varname = "test"), "RangeFilterState"))
})
