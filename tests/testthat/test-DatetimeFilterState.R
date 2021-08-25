testthat::test_that("The constructor accepts a POSIXct or POSIXlt object", {
  testthat::expect_error(DatetimeFilterState$new(as.POSIXct(8, origin = "1900/01/01"), varname = "test"), NA)
  testthat::expect_error(DatetimeFilterState$new(as.POSIXlt(8, origin = "1900/01/01"), varname = "test"), NA)
})

testthat::test_that("get_call returns a condition true for the object supplied in the constructor", {
  object <- as.POSIXct(8, origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(object, varname = "object")
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})

testthat::test_that("get_call set selected accepts an array of two POSIXct objects", {
  object <- as.POSIXct(8, origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(object, varname = "object")
  testthat::expect_error(filter_state$set_selected(c(object, object)), NA)
})

testthat::test_that("get_call returns a condition true for the object in the selected range", {
  objects <- as.POSIXct(c(1, 2, 3), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  filter_state$set_selected(c(objects[2], objects[2]))
  testthat::expect_true(eval(isolate(filter_state$get_call()))[2])
})

testthat::test_that("get_call returns a condition evaluating to TRUE for NA values after set_keep_na(TRUE)", {
  objects <- as.POSIXct(c(1, NA), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  filter_state$set_keep_na(TRUE)
  testthat::expect_equal(eval(isolate(filter_state$get_call()))[2], TRUE)
})

testthat::test_that("get_call returns a condition evaluating to NA for NA values", {
  objects <- as.POSIXct(c(1, NA), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_equal(eval(isolate(filter_state$get_call()))[2], NA)
})

testthat::test_that("DatetimeFilterState ignores the timezone of the ISO object passed to the constructor", {
  objects <- ISOdate(2021, 8, 25, tz = "GTM+10")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_equal(
    isolate(filter_state$get_call()),
    quote(
      objects >= as.POSIXct("2021-08-25 22:00:00", tz = "Etc/UTC") &
        objects <= as.POSIXct("2021-08-25 22:00:01", tz = "Etc/UTC")
    )
  )
})

