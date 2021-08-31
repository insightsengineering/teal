testthat::test_that("set_filter_state for list", {
  filter_state <- teal::init_filter_state(
    c(1:10, NA, Inf),
    varname = "x"
  )

  testthat::expect_error(
    set_filter_state(
      list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE),
      filter_state
    ),
    NA
  )
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(1, 2)
  )
  testthat::expect_true(isolate(filter_state$get_keep_na()))
  testthat::expect_true(isolate(filter_state$get_keep_inf()))

  testthat::expect_error(
    set_filter_state(
      list(selected = c(2, 9), keep_na = FALSE, keep_inf = FALSE),
      filter_state
    ),
    NA
  )
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(2, 9)
  )
  testthat::expect_false(isolate(filter_state$get_keep_na()))
  testthat::expect_false(isolate(filter_state$get_keep_inf()))
})

testthat::test_that("set_filter_state for vector", {
  filter_state <- teal::init_filter_state(
    c(1:10, NA, Inf),
    varname = "x"
  )

  testthat::expect_error(
    set_filter_state(c(1, 2, NA_real_, Inf), filter_state),
    NA
  )
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(1, 2)
  )
  testthat::expect_true(isolate(filter_state$get_keep_na()))
  testthat::expect_true(isolate(filter_state$get_keep_inf()))
})

testthat::test_that("set_filter_state for default_filter", {
  filter_state <- teal::init_filter_state(
    c(1:10, NA, Inf),
    varname = "x"
  )

  df <- default_filter()
  testthat::expect_true(is(df, "default_filter"))
  testthat::expect_error(
    set_filter_state(default_filter(), filter_state),
    NA
  )

  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(1, 10)
  )
  testthat::expect_false(isolate(filter_state$get_keep_na()))
  testthat::expect_false(isolate(filter_state$get_keep_inf()))
})

testthat::test_that("set_filter_state overwrites fields included in the input only", {
  filter_state <- teal::init_filter_state(
    c(1:10, NA, Inf),
    varname = "x"
  )
  testthat::expect_error(
    set_filter_state(list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE), filter_state),
    NA
  )
  testthat::expect_error(
    set_filter_state(list(selected = c(2, 9), keep_inf = FALSE), filter_state),
    NA
  )
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(2, 9)
  )
  testthat::expect_true(
    isolate(filter_state$get_keep_na())
  )

  testthat::expect_false(
    isolate(filter_state$get_keep_inf())
  )

  testthat::expect_error(
    set_filter_state(c(NA_real_, Inf), filter_state),
    NA
  )

  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(2, 9)
  )
  testthat::expect_true(
    isolate(filter_state$get_keep_na())
  )
  testthat::expect_true(
    isolate(filter_state$get_keep_inf())
  )
})

testthat::test_that("set_filter_state - character values", {
  filter_state <- teal::init_filter_state(
    c("a", "b", NA_character_),
    varname = "x"
  )
  testthat::expect_error(
    set_filter_state(list(selected = "a", keep_na = TRUE), filter_state),
    NA
  )
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    "a"
  )
  testthat::expect_true(
    isolate(filter_state$get_keep_na())
  )
})

testthat::test_that("set_filter_state - date values", {
  dates <- Sys.Date() + seq_len(3)
  filter_state <- teal::init_filter_state(
    c(dates, NA),
    varname = "x"
  )
  testthat::expect_error(
    set_filter_state(list(selected = dates[1:2], keep_na = TRUE), filter_state),
    NA
  )
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    dates[1:2]
  )
  testthat::expect_true(
    isolate(filter_state$get_keep_na())
  )
})

testthat::test_that("set_filter_state - logical values", {
  filter_state <- teal::init_filter_state(
    c(TRUE, FALSE, NA),
    varname = "x"
  )
  testthat::expect_error(
    set_filter_state(list(selected = TRUE, keep_na = TRUE), filter_state),
    NA
  )
  testthat::expect_true(
    isolate(filter_state$get_selected())
  )
  testthat::expect_true(
    isolate(filter_state$get_keep_na())
  )
})
