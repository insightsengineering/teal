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

testthat::test_that("set_selected throw when selection not in c(TRUE, FALSE)", {
  filter_state <- LogicalFilterState$new(TRUE, varname = "test")

  testthat::expect_error(
    filter_state$set_selected("YES"),
    "should be a logical"
  )

  testthat::expect_error(
    filter_state$set_selected(1),
    "should be a single logical value"
  )
})

testthat::test_that("get_call returns a condition true for the values passed in set_selected", {
  filter_state <- LogicalFilterState$new(c(TRUE, FALSE), varname = "test")
  filter_state$set_selected(TRUE)
  test <- c(TRUE, FALSE, FALSE, TRUE)
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(TRUE, FALSE, FALSE, TRUE))

  filter_state$set_selected(FALSE)
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, FALSE))
})

testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  filter_state <- LogicalFilterState$new(x = c(TRUE, FALSE, NA), varname = "test")

  testthat::expect_error(filter_state$set_state(list(selected = FALSE, keep_na = TRUE)), NA)
  testthat::expect_identical(isolate(filter_state$get_selected()), FALSE)
  testthat::expect_true(isolate(filter_state$get_keep_na()))

  testthat::expect_error(filter_state$set_state(list(selected = TRUE, unknown = TRUE)), "all\\(names\\(state\\)")
})

testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- LogicalFilterState$new(x = c(TRUE, FALSE, NA), varname = "test")

  testthat::expect_error(filter_state$set_state(list(selected = FALSE, keep_na = TRUE)), NA)
  testthat::expect_error(filter_state$set_state(list(selected = TRUE)), NA)

  testthat::expect_true(isolate(filter_state$get_selected()))
  testthat::expect_true(isolate(filter_state$get_keep_na()))
})
