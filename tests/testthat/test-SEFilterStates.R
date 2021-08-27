testthat::test_that("The constructor does not throw", {
  testthat::expect_error(SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  ), NA)
})

testthat::test_that("The constructor initializes two queues", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_null(filter_states$queue_get(2))
})
