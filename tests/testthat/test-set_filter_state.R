testthat::test_that("set_state sets state in the interactive session when is_reactive = FALSE", {
  filter_state <- teal:::init_filter_state(
    c(1:10, NA, Inf),
    varname = "x"
  )

  testthat::expect_error(
    teal:::set_state(
      filter_state,
      value = list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE),
      is_reactive = FALSE
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
    teal:::set_state(
      filter_state,
      value = list(selected = c(2, 9), keep_na = FALSE, keep_inf = FALSE),
      is_reactive = FALSE
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

testthat::test_that("set_state does not set 'state' when is_reactive = TRUE in interactive session", {
  filter_state <- teal:::init_filter_state(
    c(1:10, NA, Inf),
    varname = "x"
  )

  isolate(teal:::set_state(
    filter_state,
    value = list(selected = c(1, 2), keep_na = TRUE, keep_inf = TRUE),
    is_reactive = TRUE
  ))
  testthat::expect_identical(
    isolate(filter_state$get_selected()),
    c(1, 10)
  )
  testthat::expect_false(isolate(filter_state$get_keep_na()))
  testthat::expect_false(isolate(filter_state$get_keep_inf()))
})
