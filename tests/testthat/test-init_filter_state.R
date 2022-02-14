testthat::test_that("init_filter_state accepts a string or name as varname", {
  testthat::expect_error(init_filter_state(7, varname = "test"), NA)
  testthat::expect_error(init_filter_state(7, varname = quote(test)), NA)
})

testthat::test_that("init_filter_state accepts a character vector of length 0 or 1 as varlabel", {
  testthat::expect_error(init_filter_state(7, varname = "test", varlabel = "test"), NA)
  testthat::expect_error(init_filter_state(7, varname = "test", varlabel = character(0)), NA)
})

test_that("'extract_type' must be specified with 'input_dataname'", {
  testthat::expect_error(
    teal:::init_filter_state(
      ADSL$SEX,
      varname = "SEX",
      input_dataname = NULL,
      extract_type = "matrix"
    )
  )
})



testthat::test_that("init_filter_state accepts, name or call as input_dataname", {
  testthat::expect_error(
    init_filter_state(7, varname = "test", input_dataname = NULL),
    NA
  )

  testthat::expect_error(
    init_filter_state(7, varname = "test", input_dataname = quote(test)),
    NA
  )
  testthat::expect_error(
    init_filter_state(7, varname = "test", input_dataname = call("test")),
    NA
  )
})

testthat::test_that("init_filter_state accepts character as extract_type", {
  testthat::expect_error(
    init_filter_state(7, varname = "test", extract_type = character(0)),
    NA
  )

  testthat::expect_error(
    init_filter_state(7, varname = "test", input_dataname = as.name("test"), extract_type = "list"),
    NA
  )
  testthat::expect_error(
    init_filter_state(7, varname = "test", input_dataname = as.name("test"), extract_type = "matrix"),
    NA
  )
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

testthat::test_that("init_filter_state returns a DateFilterState object if passed a Date object", {
  testthat::expect_true(is(init_filter_state(as.Date("1990/01/01"), varname = "test"), "DateFilterState"))
})

testthat::test_that("init_filter_state returns a DatetimeFilterState object if passed
  a POSIXct or POSIXlt object", {
  testthat::expect_true(is(init_filter_state(as.POSIXct("1900/01/01"), varname = "test"), "DatetimeFilterState"))
  testthat::expect_true(is(init_filter_state(as.POSIXlt("1900/01/01"), varname = "test"), "DatetimeFilterState"))
})

testthat::test_that("init_filter_state returns a RangeFilterState if passed a numeric array containing Inf", {
  testthat::expect_error(fs <- init_filter_state(c(1, 2, 3, 4, Inf), varname = "test"), NA)
  testthat::expect_true(is(fs, "RangeFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState if passed fewer than five non-NA elements", {
  testthat::expect_error(fs <- init_filter_state(c(1, 2, 3, 4, NA), varname = "test"), NA)
  testthat::expect_true(is(fs, "ChoicesFilterState"))
})

testthat::test_that("init_filter_state returns a ChoicesFilterState, if passed a character array", {
  testthat::expect_error(fs <- init_filter_state(c("a"), varname = "test"), NA)
  testthat::expect_true(is(fs, "ChoicesFilterState"))
})

testthat::test_that("init_filter_state return a LogicalFilterState, if passed a logical array", {
  testthat::expect_error(fs <- init_filter_state(c(TRUE), varname = "test"), NA)
  testthat::expect_true(is(fs, "LogicalFilterState"))
})
