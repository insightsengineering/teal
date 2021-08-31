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


testthat::test_that("set_bookmark_state throws error on bad inputs", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_error(filter_states$set_bookmark_state())
  testthat::expect_error(filter_states$set_bookmark_state(data.frame(a = "test")))
  testthat::expect_error(filter_states$set_bookmark_state(list(subset = "test", select = "test")))
})

testthat::test_that("set_bookmark_state returns NULL on good inputs", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_null(filter_states$set_bookmark_state(data = data, state = list(subset = NULL, select = NULL)))
})
