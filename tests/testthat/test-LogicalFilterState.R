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

testthat::test_that("set_selected warns about passed values which are not logical", {
  filter_state <- LogicalFilterState$new(c(TRUE, FALSE), varname = "test")
  testthat::expect_warning(filter_state$set_selected(c(TRUE, "test")), "are not logical")
})

testthat::test_that("set_selected sets an intersection of logical values and the passed array", {
  filter_state <- LogicalFilterState$new(c(TRUE, FALSE), varname = "test")
  suppressWarnings(filter_state$set_selected(c(TRUE, "test")))
  testthat::expect_equal(isolate(filter_state$get_selected()), TRUE)
})

testthat::test_that("get_call returns a condition true for the values passed in set_selected", {
  filter_state <- LogicalFilterState$new(c(TRUE, FALSE), varname = "test")
  filter_state$set_selected(TRUE)
  test <- c(TRUE, FALSE, FALSE, TRUE)
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(TRUE, FALSE, FALSE, TRUE))

  filter_state$set_selected(FALSE)
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, FALSE))
})
