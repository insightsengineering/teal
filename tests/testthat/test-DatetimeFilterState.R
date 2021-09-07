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
  testthat::expect_equal(
    isolate(filter_state$get_call()),
    bquote(
      test >= as.POSIXct(.(as.character(test[2])), tz = .(Sys.timezone())) &
      test < as.POSIXct(.(as.character(test[4])), tz = .(Sys.timezone()))
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

testthat::test_that("DatetimeFilterState echoes the timezone of the ISO object passed to the constructor", {
  objects <- ISOdate(2021, 8, 25, tz = "GTM+10")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_equal(
    isolate(filter_state$get_call()),
    quote(
      objects >= as.POSIXct("2021-08-25 12:00:00", tz = "GTM+10") &
        objects < as.POSIXct("2021-08-25 12:00:01", tz = "GTM+10")
    )
  )
})

testthat::test_that("set_selected warns when the selected range intersects the possible range
  but is not fully included in it", {
  objects <- as.POSIXct(c(2, 3), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_warning(filter_state$set_selected(c(objects[1] - 1, objects[1])), "outside of the possible range")
  testthat::expect_warning(filter_state$set_selected(c(objects[2], objects[2] + 1)), "outside of the possible range")
  testthat::expect_warning(
    filter_state$set_selected(c(objects[1] - 1, objects[2] + 1)),
    "outside of the possible range"
  )
})

testthat::test_that("set_selected throws when the selected range is completely outside of the possible range", {
  objects <- as.POSIXct(c(2, 3), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_error(
    suppressWarnings(filter_state$set_selected(c(objects[2] + 1, objects[2] + 2))),
    "the upper bound of the range lower than the lower bound"
  )
})

testthat::test_that("set_selected limits the selected range to the lower and the upper bound of the possible range", {
  objects <- as.POSIXct(c(2, 3), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  suppressWarnings(filter_state$set_selected(c(objects[1] - 1, objects[1])))
  testthat::expect_equal(isolate(filter_state$get_selected()), c(objects[1], objects[1]))

  suppressWarnings(filter_state$set_selected(c(objects[2], objects[2] + 1)))
  testthat::expect_equal(isolate(filter_state$get_selected()), c(objects[2], objects[2]))

  suppressWarnings(filter_state$set_selected(c(objects[1] - 1, objects[2] + 1)))
  testthat::expect_equal(isolate(filter_state$get_selected()), c(objects[1], objects[2]))
})

testthat::test_that("set_selected throws when the values are not within the range passed to the constructor", {
  objects <- as.POSIXct(c(1, 2, 3), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_error(filter_state$set_selected("a"), "character string is not in a standard unambiguous format")
})
