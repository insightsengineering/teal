testthat::test_that("The constructor accepts a Date object", {
  testthat::expect_error(DateFilterState$new(as.Date("2013-07-13"), varname = "test"), NA)
})

testthat::test_that("get_call returns a condition true for the object passed in the constructor", {
  test_date <- as.Date("2013-07-13")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})

testthat::test_that("set_selected accepts an array of two Date objects", {
  test_date <- as.Date("2013-07-13")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_error(filter_state$set_selected(c(test_date, test_date)), NA)
})

testthat::test_that("set_selected warns when selection is not within the possible range", {
  test_date <- as.Date("13/07/2013")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_warning(
    filter_state$set_selected(c(test_date - 1, test_date)),
    regexp = "outside of the possible range"
  )
  testthat::expect_warning(
    filter_state$set_selected(c(test_date, test_date + 1)),
    regexp = "outside of the possible range"
  )
  testthat::expect_warning(
    filter_state$set_selected(c(test_date - 1, test_date + 1)),
    regexp = "outside of the possible range"
  )
})

testthat::test_that("set_selected limits the selected range to the lower and the upper bound of the possible range", {
  test_date <- as.Date(c("13/07/2013", "14/07/2013"))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  suppressWarnings(filter_state$set_selected(c(test_date[1] - 1, test_date[1])))
  testthat::expect_equal(isolate(filter_state$get_selected()), c(test_date[1], test_date[1]))

  suppressWarnings(filter_state$set_selected(c(test_date[2], test_date[2] + 1)))
  testthat::expect_equal(isolate(filter_state$get_selected()), c(test_date[2], test_date[2]))

  suppressWarnings(filter_state$set_selected(c(test_date[1] - 1, test_date[2] + 1)))
  testthat::expect_equal(isolate(filter_state$get_selected()), c(test_date[1], test_date[2]))
})

testthat::test_that("set_selected throws when selection is completely outside of the possible range", {
  test_date <- as.Date("13/07/2013")
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_error(
    suppressWarnings(filter_state$set_selected(c(test_date - 2, test_date - 1))),
    regexp = "the upper bound of the range lower than the lower bound"
  )
})

testthat::test_that("set_selected throws when selection is not Date", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15"))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_error(
    filter_state$set_selected(c("a", "b")),
    "The array of set values must contain values coercible to Date."
  )
})

testthat::test_that("get_call returns a condition true for the objects in the selected range", {
  test_date <- as.Date(c("2013-07-13", "2013-07-14", "2013-07-15"))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  filter_state$set_selected(c(test_date[2], test_date[2]))
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(FALSE, TRUE, FALSE))
  testthat::expect_equal(
    isolate(filter_state$get_call()),
    quote(test_date >= as.Date("2013-07-14") & test_date <= as.Date("2013-07-14"))
  )
})

testthat::test_that("get_call returns a condition evaluating to NA for NA values", {
  test_date <- as.Date(c("2013-07-13", NA))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  testthat::expect_equal(eval(isolate(filter_state$get_call()))[2], NA)
})

testthat::test_that("get_call reutrns a condition evaluating to TRUE for NA values after set_keep_na(TRUE)", {
  test_date <- as.Date(c("2013-07-13", NA))
  filter_state <- DateFilterState$new(test_date, varname = "test_date")
  filter_state$set_keep_na(TRUE)
  testthat::expect_true(eval(isolate(filter_state$get_call()))[2])
})

testthat::test_that("set_state accepts a named list with selected and keep_na elements", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  testthat::expect_error(
    filter_state$set_state(list(selected = c(test_date[2], test_date[3]), keep_na = TRUE)),
    NA
  )
  testthat::expect_error(
    filter_state$set_state(
      list(selected = c(test_date[2], test_date[3]), unknown = TRUE)
    ),
    "all\\(names\\(state\\)"
  )
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  filter_state$set_state(list(selected = c(test_date[2], test_date[3]), keep_na = TRUE))
  testthat::expect_identical(isolate(filter_state$get_selected()), c(test_date[2], test_date[3]))
  testthat::expect_true(isolate(filter_state$get_keep_na()))
})


testthat::test_that("set_state overwrites fields included in the input only", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16", "2013/08/17"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  filter_state$set_state(list(selected = c(test_date[2], test_date[3]), keep_na = TRUE))
  testthat::expect_error(filter_state$set_state(list(selected = c(test_date[3], test_date[4]))), NA)
  testthat::expect_identical(isolate(filter_state$get_selected()), c(test_date[3], test_date[4]))
  testthat::expect_true(isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state_reactive needs a named list with selected and keep_na elements", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16", "2013/08/17"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  testthat::expect_error(
    filter_state$set_state_reactive(list(selected = c("2013/08/16", "2013/08/17"), keep_na = TRUE)),
    NA
  )
  testthat::expect_error(
    filter_state$set_state_reactive(list(selected = c("2013/08/16", "2013/08/17"), unknown = TRUE)),
    "all\\(names\\(state\\)"
  )
})

testthat::test_that("set_selected_reactive warns when selection not within allowed range", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16", "2013/08/17"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  testthat::expect_warning(
    filter_state$set_selected_reactive(c("2001/08/16", "2013/08/17")),
    "Value: 2001-08-16 is outside of the possible range for column test of dataset  ."
  )
})

testthat::test_that("set_selected_reactive throws error when one argument is given only", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16", "2013/08/17"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  testthat::expect_error(
    filter_state$set_selected_reactive(c("2013/08/17")),
    "The array of set values must have length two."
  )
})

testthat::test_that("set_keep_na_reactive accepts logical input", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16", "2013/08/17"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  testthat::expect_error(filter_state$set_keep_na_reactive(TRUE), NA)
})

testthat::test_that("set_keep_na_reactive throws error if input is not logical", {
  test_date <- as.Date(c("2013/07/13", "2013/07/14", "2013/07/15", "2013/08/16", "2013/08/17"))
  filter_state <- DateFilterState$new(test_date, varname = "test")
  testthat::expect_error(filter_state$set_keep_na_reactive("TRUE"))
  testthat::expect_error(filter_state$set_keep_na_reactive(1))
})
