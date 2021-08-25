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
  filter_state <- RangeFilterState$new(c(1, 2, 3), varname = "a")
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")

  test <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, dimnames = list(c(), c("a")))

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test[1:3, 1, drop = FALSE])
})
