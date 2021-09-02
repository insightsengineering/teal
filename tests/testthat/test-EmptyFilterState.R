testthat::test_that("get_call returns NULL after set_keep_na(FALSE)", {
  filter_state <- EmptyFilterState$new(7, varname = "7")
  filter_state$set_keep_na(FALSE)
  testthat::expect_false(isolate(filter_state$get_call()))
})

testthat::test_that("get_call returns a call after set_keep_na(TRUE)", {
  filter_state <- EmptyFilterState$new(7, varname = "test")
  filter_state$set_keep_na(TRUE)
  testthat::expect_equal(isolate(filter_state$get_call()), quote(is.na(test)))
})

testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  filter_state <- EmptyFilterState$new(7, varname = "test")
  filter_state$set_state(list(keep_na = TRUE))
  testthat::expect_true(isolate(filter_state$get_keep_na()))
  testthat::expect_error(
    filter_state$set_state(
      list(selected = 1)
    ),
    "All values in variable 'test' are `NA`"
  )
  testthat::expect_error(
    filter_state$set_state(
      list(keep_na = FALSE, unknown = TRUE)
    ),
    "all\\(names\\(state\\)"
  )
})
