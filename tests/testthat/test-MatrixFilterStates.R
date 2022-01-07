testthat::test_that("The constructor does not throw", {
  testthat::expect_error(MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  ), NA)
})

testthat::test_that("The constructor initializes one queue", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_null(filter_states$queue_get(1))
})

testthat::test_that("get_call returns a call filtering a matrix with numeric values", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  filter_state <- RangeFilterState$new(
    x = c(1, 2, 3),
    varname = "a",
    input_dataname = as.name("test"),
    extract_type = "matrix"
  )
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")

  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:3, 1, drop = FALSE])
})

testthat::test_that("set_filter_state adds filters to reactiveQueue", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))
  fs <- list(a = c(1, 2))
  filter_states$set_filter_state(state = fs, data = test)

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:2, 1, drop = FALSE])
})

testthat::test_that("set_filter_state throws error when list is unnamed", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))
  fs <- list(c(1, 2))

  testthat::expect_error(
    filter_states$set_filter_state(state = fs, data = test),
    "Assertion on 'checkmate::test_null(names(state))' failed. FALSE.",
    fixed = TRUE
  )
})

testthat::test_that("remove_filter_state removes filters from reactiveQueue", {
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))
  fs <- list(a = c(1, 2))
  filter_states$set_filter_state(state = fs, data = test)

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:2, 1, drop = FALSE])

  filter_states$remove_filter_state("a")
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test)
})

testthat::test_that("remove_filter_state throws warning when element_id is not in reactiveQueue", {
  suppress_logs()
  filter_states <- MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = "test"
  )
  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))
  fs <- list(a = c(1, 2))
  filter_states$set_filter_state(state = fs, data = test)

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:2, 1, drop = FALSE])

  testthat::expect_warning(filter_states$remove_filter_state("B"))
})
