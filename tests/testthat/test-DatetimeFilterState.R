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
  objects <- ISOdate(2021, 8, 25, tz = "Australia/Brisbane")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_equal(
    isolate(filter_state$get_call()),
    quote(
      objects >= as.POSIXct("2021-08-25 12:00:00", tz = "Australia/Brisbane") &
        objects < as.POSIXct("2021-08-25 12:00:01", tz = "Australia/Brisbane")
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

testthat::test_that("set_selected throws when the value type cannot be interpreted as POSIX", {
  objects <- as.POSIXct(c(1, 2, 3), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "objects")
  testthat::expect_error(
    filter_state$set_selected(c("a", "b")),
    "The array of set values must contain values coercible to POSIX."
  )
})

testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  objects <- as.POSIXct(c(1:4), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  testthat::expect_error(
    filter_state$set_state(list(selected = c(objects[2], objects[3]), keep_na = TRUE)),
    NA
  )
  testthat::expect_error(
    filter_state$set_state(
      list(selected = c(objects[3], objects[4]), unknown = TRUE)
    ),
    "all\\(names\\(state\\)"
  )
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  objects <- as.POSIXct(c(1:4), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  filter_state$set_state(list(selected = c(objects[2], objects[3]), keep_na = TRUE))
  testthat::expect_identical(isolate(filter_state$get_selected()), c(objects[2], objects[3]))
  testthat::expect_true(isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state overwrites fields included in the input only", {
  objects <- as.POSIXct(c(1:5), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  filter_state$set_state(list(selected = c(objects[2], objects[3]), keep_na = TRUE))
  testthat::expect_error(filter_state$set_state(list(selected = c(objects[3], objects[4]))), NA)
  testthat::expect_identical(isolate(filter_state$get_selected()), c(objects[3], objects[4]))
  testthat::expect_true(isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state_reactive needs a named list with selected and keep_na elements", {
  objects <- as.POSIXct(c(1:5), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  testthat::expect_error(
    isolate(filter_state$set_state_reactive(list(selected = c(objects[2], objects[3]), keep_na = TRUE))),
    NA
  )
  testthat::expect_error(
    filter_state$set_state_reactive(list(selected = c(objects[2], objects[3]), unknown = TRUE)),
    "all\\(names\\(state\\)"
  )
})

testthat::test_that("set_selected_reactive warns when selection not within allowed range", {
  objects <- as.POSIXct(c(1:5), origin = "1900/01/01")
  objects2 <- as.POSIXct(c(1), origin = "1899/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  testthat::expect_warning(
    filter_state$set_selected_reactive(c(objects2, objects[3])),
    paste(objects2, "is outside of the possible range for column test of dataset  .")
  )
})

testthat::test_that("set_selected_reactive throws error when one argument is given only", {
  objects <- as.POSIXct(c(1:5), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  testthat::expect_error(
    filter_state$set_selected_reactive(c(objects[3])),
    "The array of set values must have length two."
  )
})

testthat::test_that("set_keep_na_reactive accepts logical input", {
  objects <- as.POSIXct(c(1:5), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  testthat::expect_error(isolate(filter_state$set_keep_na_reactive(TRUE)), NA)
})

testthat::test_that("set_keep_na_reactive throws error if input is not logical", {
  objects <- as.POSIXct(c(1:5), origin = "1900/01/01")
  filter_state <- DatetimeFilterState$new(objects, varname = "test")
  testthat::expect_error(filter_state$set_keep_na_reactive("TRUE"))
  testthat::expect_error(filter_state$set_keep_na_reactive(1))
})
