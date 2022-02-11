testthat::test_that("The constructor accepts logical values", {
  testthat::expect_error(LogicalFilterState$new(c(TRUE), varname = "test"), NA)
})

testthat::test_that("The constructor accepts NA values", {
  testthat::expect_error(LogicalFilterState$new(c(TRUE, NA), varname = "test"), NA)
})

testthat::test_that("get_call returns a condition true for TRUE passed in the constructor", {
  test <- c(TRUE)
  filter_state <- LogicalFilterState$new(test, varname = "test")
  testthat::expect_true(all(eval(isolate(filter_state$get_call()))))
})

testthat::test_that("get_call returns a condition TRUE for a value FALSE
  for an array (TRUE, FALSE) passed to constructor", {
  test <- c(TRUE, FALSE)
  filter_state <- LogicalFilterState$new(test, varname = "test")
  test <- FALSE
  testthat::expect_true(eval(isolate(filter_state$get_call())))
})

testthat::test_that("set_selected does not throw when passed a scalar logical value", {
  filter_state <- LogicalFilterState$new(c(TRUE, FALSE), varname = "test")
  testthat::expect_error(filter_state$set_selected(TRUE), NA)
  testthat::expect_error(filter_state$set_selected(FALSE), NA)
})

testthat::test_that("set_selected throws when the passed values are not coercible to logical", {
  filter_state <- LogicalFilterState$new(c(TRUE, FALSE), varname = "test")
  testthat::expect_error(
    filter_state$set_selected(c(print)),
    "The array of set values must contain values coercible to logical."
  )
})

testthat::test_that("get_call returns a condition true for the values passed in set_selected", {
  filter_state <- LogicalFilterState$new(c(TRUE, FALSE), varname = "test")
  filter_state$set_selected(TRUE)
  test <- c(TRUE, FALSE, FALSE, TRUE)
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(TRUE, FALSE, FALSE, TRUE))
  filter_state$set_selected(FALSE)
  testthat::expect_equal(eval(isolate(filter_state$get_call())), c(FALSE, TRUE, TRUE, FALSE))
})

testthat::test_that("set_state needs a named list with selected and keep_na elements", {
  filter_state <- LogicalFilterState$new(x = c(TRUE, FALSE, NA), varname = "test")
  testthat::expect_error(filter_state$set_state(list(selected = FALSE, keep_na = TRUE)), NA)
  testthat::expect_error(filter_state$set_state(list(selected = TRUE, unknown = TRUE)), "all\\(names\\(state\\)")
})

testthat::test_that("set_state sets values of selected and keep_na as provided in the list", {
  filter_state <- LogicalFilterState$new(x = c(TRUE, FALSE, NA), varname = "test")
  filter_state$set_state(list(selected = FALSE, keep_na = TRUE))
  testthat::expect_identical(isolate(filter_state$get_selected()), FALSE)
  testthat::expect_true(isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state overwrites fields included in the input only", {
  filter_state <- LogicalFilterState$new(x = c(TRUE, FALSE, NA), varname = "test")
  filter_state$set_state(list(selected = FALSE, keep_na = TRUE))
  testthat::expect_error(filter_state$set_state(list(selected = TRUE)), NA)
  testthat::expect_true(isolate(filter_state$get_selected()))
  testthat::expect_true(isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state_reactive needs a named list with selected and keep_na elements", {
  filter_state <- LogicalFilterState$new(c(TRUE), varname = "test")
  testthat::expect_error(filter_state$set_state_reactive(list(selected = TRUE, keep_na = TRUE)), NA)
  testthat::expect_error(
    filter_state$set_state_reactive(list(selected = TRUE, unknown = TRUE)),
    "all\\(names\\(state\\)"
  )
})

testthat::test_that("set_selected_reactive throws error when selection is not coercible to logical", {
  filter_state <- LogicalFilterState$new(c(TRUE), varname = "test")
  testthat::expect_error(
    filter_state$set_selected_reactive(list(selected = "A")),
    "The array of set values must contain values coercible to logical."
  )
})

testthat::test_that("set_keep_na_reactive accepts logical input", {
  filter_state <- LogicalFilterState$new(c(TRUE), varname = "test")
  testthat::expect_error(filter_state$set_keep_na_reactive(TRUE), NA)
})

testthat::test_that("set_keep_na_reactive throws error if input is not logical", {
  filter_state <- LogicalFilterState$new(c(TRUE), varname = "test")
  testthat::expect_error(filter_state$set_keep_na_reactive("TRUE"))
  testthat::expect_error(filter_state$set_keep_na_reactive(1))
})

testthat::test_that(
  "LogicalFilterState$is_any_filtered works properly when NA is present in data",
  code = {
    filter_state <- teal:::LogicalFilterState$new(
      rep(c(TRUE, NA), 10),
      varname = "x",
      input_dataname = as.name("data"),
      extract_type = character(0)
    )
    isolate(filter_state$set_selected(TRUE))

    isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_false(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )
  }
)

testthat::test_that(
  "LogicalFilterState$is_any_filtered works properly when both TRUE and FALSE are present",
  code = {
    filter_state <- teal:::LogicalFilterState$new(
      rep(c(TRUE, FALSE, NA), 10),
      varname = "x",
      input_dataname = as.name("data"),
      extract_type = character(0)
    )
    isolate(filter_state$set_selected(TRUE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(FALSE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(TRUE))
    isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(TRUE))
    isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(FALSE))
    isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(FALSE))
    isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )
  }
)

testthat::test_that(
  "LogicalFilterState$is_any_filtered works properly when only TRUE or FALSE is present",
  code = {
    filter_state <- teal:::LogicalFilterState$new(
      rep(c(TRUE, NA), 10),
      varname = "x",
      input_dataname = as.name("data"),
      extract_type = character(0)
    )
    isolate(filter_state$set_selected(TRUE))
    isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_false(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(TRUE))
    isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(FALSE))
    isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(FALSE))
    isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    filter_state <- teal:::LogicalFilterState$new(
      rep(c(FALSE, NA), 10),
      varname = "x",
      input_dataname = as.name("data"),
      extract_type = character(0)
    )
    isolate(filter_state$set_selected(FALSE))
    isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_false(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(FALSE))
    isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(TRUE))
    isolate(filter_state$set_keep_na(TRUE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )

    isolate(filter_state$set_selected(TRUE))
    isolate(filter_state$set_keep_na(FALSE))
    testthat::expect_true(
      isolate(filter_state$is_any_filtered())
    )
  }
)
