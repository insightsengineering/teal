testthat::test_that("The constructor accepts logical values", {
  testthat::expect_error(LogicalFilterState$new(c(TRUE), varname = "test"), NA)
})

testthat::test_that("The constructor accepts NA values", {
  testthat::expect_error(LogicalFilterState$new(c(TRUE, NA), varname = "test"), NA)
})

testthat::test_that("get_call returns a condition true for TRUE passed in the constructor", {
  test <- c(TRUE)
  filter_state <- LogicalFilterState$new(test, varname = "test")
  testthat::expect_true(all(eval(isolate(filter_state$get_call()))))
})

testthat::test_that("get_call returns a condition TRUE for a value FALSE
  for an array (TRUE, FALSE) passed to constructor", {
  test <- c(TRUE, FALSE)
  filter_state <- LogicalFilterState$new(test, varname = "test")
  test <- FALSE
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})

testthat::test_that("set_selected does not throw when passed a scalar logical value", {
  filter_state <- LogicalFilterState$new(c(TRUE, FALSE), varname = "test")
  testthat::expect_error(filter_state$set_selected(TRUE), NA)
  testthat::expect_error(filter_state$set_selected(FALSE), NA)
})
