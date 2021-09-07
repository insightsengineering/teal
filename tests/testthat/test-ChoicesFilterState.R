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

testthat::test_that("get_call returns a condition true for the values passed to set_selected", {
  filter_state <- ChoicesFilterState$new(c(letters[1:7]), varname = "test")
  filter_state$set_selected(letters[2:3])
  test <- letters[1:4]
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, FALSE))
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

testthat::test_that("set_selected warns when selection not within allowed choices", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")
  testthat::expect_warning(filter_state$set_selected(c("test", 7)), "not in choices")
})

testthat::test_that("set_selected sets the intersection of choices and the passed values", {
  filter_state <- ChoicesFilterState$new(c("test1", "test2"), varname = "test")
  suppressWarnings(filter_state$set_selected(c("test1", 7)))
  testthat::expect_equal(isolate(filter_state$get_selected()), "test1")
})
