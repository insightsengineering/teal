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

testthat::test_that("set_bookmark_state throws error when input is missing", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_error(filter_states$set_bookmark_state())
})

testthat::test_that("set_bookmark_state throws error when data argument is inputted", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_error(filter_states$set_bookmark_state(state = list(subset = NULL, select = NULL)))
})

testthat::test_that("set_bookmark_state throws error when data argument is not of class SummarizedExperiment", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  testthat::expect_error(filter_states$set_bookmark_state(data = data, state = list(subset = NULL, select = NULL)))
})

testthat::test_that("set_bookmark_state throws error when state argument contains extra elements", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_error(filter_states$set_bookmark_state(data = data, state = list(A = "test")))
})

testthat::test_that("set_bookmark_state throws error when state argument is not a list", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_error(filter_states$set_bookmark_state(data = data, state = c(subset = "A", select = "B")))
})

## acceptable inputs to set_bookmark_state
testthat::test_that("set_bookmark_state returns NULL when state argument contains subset and select set as NULL", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_null(filter_states$set_bookmark_state(data = data, state = list(subset = NULL, select = NULL)))
})

testthat::test_that("set_bookmark_state returns NULL when state argument is an empty list", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_null(filter_states$set_bookmark_state(data = data, state = list()))
})
