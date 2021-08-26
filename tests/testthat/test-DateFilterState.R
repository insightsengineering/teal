testthat::test_that("The constructor accepts a Date object", {
  testthat::expect_error(DateFilterState$new(as.Date("13/07/2013"), varname = "test"), NA)
})

testthat::test_that("get_call returns a condition true for the object passed in the constructor", {
  test_date <- as.Date("13/07/2013")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})

testthat::test_that("set_selected accepts an array of two Date objects", {
  test_date <- as.Date("13/07/2013")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_error(filter_state$set_selected(c(test_date, test_date)), NA)
})

testthat::test_that("get_call returns a condition true for the objects in the selected range", {
  test_date <- as.Date(c("13/07/2013", "14/07/2013", "15/07/2013"))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  filter_state$set_selected(c(test_date[2], test_date[2]))
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(FALSE, TRUE, FALSE))
  testthat::expect_equal(
    isolate(filter_state$get_call()),
    quote(test_date >= as.Date("14-07-20") & test_date <= as.Date("14-07-20"))
  )
})

testthat::test_that("get_call returns a condition evaluating to NA for NA values", {
  test_date <- as.Date(c("13/07/2013", NA))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_equal(eval(isolate(filter_state$get_call()))[2], NA)
})

testthat::test_that("get_call reutrns a condition evaluating to TRUE for NA values after set_keep_na(TRUE)", {
  test_date <- as.Date(c("13/07/2013", NA))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  filter_state$set_keep_na(TRUE)
  testthat::expect_true(eval(isolate(filter_state$get_call()))[2])
})


testthat::test_that("set_selected throw when selection not within allowed choices", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15"))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")


  testthat::expect_error(
    filter_state$set_selected("a"),
    "should be a Date"
  )

  testthat::expect_error(
    filter_state$set_selected(c(test_date[1] - 3, test_date[2])),
    "not valid for full range"
  )
})
