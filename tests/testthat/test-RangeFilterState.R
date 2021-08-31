testthat::test_that("The constructor accepts numerical values", {
  testthat::expect_error(RangeFilterState$new(c(1), varname = "test"), NA)
})

testthat::test_that("The constructor accepts infinite values", {
  testthat::expect_error(RangeFilterState$new(c(1, Inf, -Inf), varname = "test"), NA)
  testthat::expect_error(RangeFilterState$new(Inf, varname = "test"), "any\\(is.finite\\(x")
})

testthat::test_that("get_call returns a condition TRUE for all values passed to the constructor", {
  filter_state <- RangeFilterState$new(c(1, 2, 3), varname = "test")
  testthat::expect_equal(isolate(filter_state$get_call()), quote(test >= 1 & test <= 3))
  test <- c(1, 2, 3)
  testthat::expect_true(all(eval(isolate(filter_state$get_call()))))
})

testthat::test_that("get_call returns a condition TRUE for all values passed to the constructor", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_equal(isolate(filter_state$get_call()), quote(test >= 7 & test <= 7))
  test <- 7
  testthat::expect_true(all(eval(isolate(filter_state$get_call()))))
})

testthat::test_that("set_selected throws an error if selecting outside of the provided range", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_error(filter_state$set_selected(c(1, 3)), regexp = "not valid for full range")
})

testthat::test_that("get_call returns a valid call after an unsuccessfull set_selected", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_error(filter_state$set_selected(c(1, 3)), regexp = "not valid for full range")
  test <- 7
  testthat::expect_true(all(eval(isolate(filter_state$get_call()))))
})

testthat::test_that("set_selected accepts an array with two numerical elements", {
  filter_state <- RangeFilterState$new(7, varname = "test")
  testthat::expect_error(filter_state$set_selected(c(7, 7)), NA)
})

testthat::test_that("get_call returns the call with values passed in set_selected", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  filter_state$set_selected(c(3, 4))
  testthat::expect_equal(isolate(filter_state$get_call()), quote(test >= 3 & test <= 4))
})

testthat::test_that("get_call returns a condition true for the values from the range passed to set_selected", {
  filter_state <- RangeFilterState$new(c(3, 5), varname = "test")
  filter_state$set_selected(c(3, 5))
  test <- c(2:6)
  eval(isolate(filter_state$get_call()))
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

testthat::test_that("get_call returns the call with a condition false for infinite values", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  test <- Inf
  testthat::expect_false(eval(isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition true for infinite values after set_keep_inf(TRUE)", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  filter_state$set_keep_inf(TRUE)
  test <- Inf
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition returning NA for NA values", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  test <- NA
  testthat::expect_equal(eval(isolate(filter_state$get_call())), NA)
})

testthat::test_that("get_call returns a condition true for NAs after set_keep_na(TRUE)", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  filter_state$set_keep_na(TRUE)
  test <- NA
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition true for NAs and Inf values after setting NA and Inf flag", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  filter_state$set_keep_na(TRUE)
  filter_state$set_keep_inf(TRUE)
  test <- c(NA, Inf)
  testthat::expect_true(all(eval(isolate(filter_state$get_call()))))
})

testthat::test_that("set_selected throw when selection not within allowed choices", {
  filter_state <- RangeFilterState$new(c(1, 8), varname = "test")
  testthat::expect_error(
    filter_state$set_selected("a"),
    "should be a numeric"
  )

  testthat::expect_error(
    filter_state$set_selected(c(0, 1)),
    "not valid for full range"
  )
})

testthat::test_that("set_state need named list with selected and keep_na elements", {
  filter_state <- RangeFilterState$new(c(1, 8, NA_real_, Inf), varname = "test")
  testthat::expect_error(
    set_filter_state(list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE), filter_state),
    NA
  )
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(1, 2)
  )
  testthat::expect_true(isolate(filter_state$get_keep_na()))
  testthat::expect_true(isolate(filter_state$get_keep_inf()))
  testthat::expect_error(filter_state$set_state(list(selected = c(1, 2), unknown = TRUE)), "all\\(names\\(state\\)")
})


testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- RangeFilterState$new(c(1, 8, NA_real_, Inf), varname = "test")
  testthat::expect_error(
    set_filter_state(list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE), filter_state),
    NA
  )

  testthat::expect_error(
    set_filter_state(list(selected = c(5, 6), keep_na = TRUE, keep_inf = TRUE), filter_state),
    NA
  )
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(5, 6)
  )
  testthat::expect_true(isolate(filter_state$get_keep_na()))
  testthat::expect_true(isolate(filter_state$get_keep_inf()))
})
