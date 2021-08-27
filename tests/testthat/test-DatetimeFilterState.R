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
  objects <- as.POSIXct(c(1:5), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  filter_state$set_selected(c(objects[2], objects[3]))
  test <- as.POSIXct(c(1:4), origin = "1900/01/01")
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, FALSE))
  test_sys_tz <- as.POSIXct(test, tz = Sys.timezone())
  testthat::expect_equal(
    isolate(filter_state$get_call()),
    bquote(
      test >= as.POSIXct(.(as.character(test_sys_tz[2])), tz = .(Sys.timezone())) &
      test < as.POSIXct(.(as.character(test_sys_tz[4])), tz = .(Sys.timezone()))
    )
  )
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

testthat::test_that("DatetimeFilterState applies the timezone of the ISO object passed to the constructor", {
  objects <- ISOdate(2021, 8, 25, tz = "America/Los_Angeles")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_equal(
    isolate(filter_state$get_call()),
    quote(
      objects >= as.POSIXct("2021-08-25 12:00:00", tz = "America/Los_Angeles") &
        objects < as.POSIXct("2021-08-25 12:00:01", tz = "America/Los_Angeles")
    )
  )
})

testthat::test_that("set_selected throw when selection not within allowed choices", {
  objects <- as.POSIXct(c(1, 2, 3), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_error(
    filter_state$set_selected("a"),
    "should be a POSIXct or POSIXlt"
  )

  testthat::expect_error(
    filter_state$set_selected(c(objects[2], objects[3] + 1)),
    "not valid for full range"
  )
})
