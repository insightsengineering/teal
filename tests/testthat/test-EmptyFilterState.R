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
