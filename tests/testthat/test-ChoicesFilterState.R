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

testthat::test_that("set_selected throw when selection not within allowed choices", {
  filter_state <- ChoicesFilterState$new("test", varname = "test")

  testthat::expect_error(
    filter_state$set_selected(1),
    "should be a character"
  )

  testthat::expect_error(
    filter_state$set_selected("a"),
    "not in valid choices"
  )
})

testthat::test_that("set_state need named list with selected and keep_na elements", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), varname = "test")

  testthat::expect_error(filter_state$set_state(list(selected = "a", keep_na = TRUE)), NA)
  testthat::expect_identical(isolate(filter_state$get_selected()), "a")
  testthat::expect_true(isolate(filter_state$get_keep_na()))

  testthat::expect_error(filter_state$set_state(list(selected = "a", unknown = TRUE)), "all\\(names\\(state\\)")
})

testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- ChoicesFilterState$new(x = c("a", "b", NA_character_), varname = "test")

  testthat::expect_error(filter_state$set_state(list(selected = "a", keep_na = TRUE)), NA)
  testthat::expect_error(filter_state$set_state(list(selected = "b")), NA)

  testthat::expect_identical(isolate(filter_state$get_selected()), "b")
  testthat::expect_true(isolate(filter_state$get_keep_na()))
})