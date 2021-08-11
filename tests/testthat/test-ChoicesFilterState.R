testthat::test_that("The constructor accepts character or factor", {
  testthat::expect_error(ChoicesFilterState$new("test", varname = "test"), NA)
  testthat::expect_error(ChoicesFilterState$new(as.factor("test"), varname = "test"), NA)
})

testthat::test_that("get_call returns a condition true for values passed in constructor", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")
  test <- "test"
  testthat::expect_true(eval(isolate(filter_state$get_call())))

  filter_state <- ChoicesFilterState$new(factor("test"), varname = "test")
  test <- factor("test")
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})

testthat::test_that("get_call returns a condition returning NA for NA values", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")
  test <- NA
  testthat::expect_equal(eval(isolate(filter_state$get_call())), NA)
})

testthat::test_that("get_call returns a condition true for NA values", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")
  filter_state$set_keep_na(TRUE)
  test <- NA
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})
