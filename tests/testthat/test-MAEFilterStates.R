testthat::test_that("The constructor does not throw", {
  testthat::expect_error(MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  ), NA)
})

testthat::test_that("get_fun returns the MAE specific subset function", {
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  testthat::expect_equal(filter_states$get_fun(), "MultiAssayExperiment::subsetByColData")
})

testthat::test_that("The constructor initializes a queue", {
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  testthat::expect_null(filter_states$queue_get(1))
})

testthat::test_that("get_call returns a call filtering an MAE object using ChoicesFilterState", {
  library(MultiAssayExperiment)
  data(miniACC)
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  filter_state <- ChoicesFilterState$new(
    x = c("white"),
    varname = as.name("race"),
    input_dataname = as.name("test"),
    extract_type = "list"
  )
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")

  test <- miniACC
  eval(isolate(filter_states$get_call()))
  testthat::expect_identical(
    output,
    MultiAssayExperiment::subsetByColData(test, test$race == "white")
  )
})

testthat::test_that("get_call returns a call filtering an MAE object using RangeFilterState", {
  library(MultiAssayExperiment)
  data(miniACC)
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  mean_purity <- mean(colData(miniACC)$purity, na.rm = TRUE)
  max_purity <- max(colData(miniACC)$purity, na.rm = TRUE)
  filter_state <- RangeFilterState$new(
    x = c(mean_purity, max_purity),
    varname = as.name("purity"),
    input_dataname = as.name("test"),
    extract_type = "list"
  )
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")

  test <- miniACC
  eval(isolate(filter_states$get_call()))
  testthat::expect_identical(
    output,
    MultiAssayExperiment::subsetByColData(
      test,
      colData(miniACC)$purity >= mean_purity & colData(miniACC)$purity <= max_purity
    )
  )
})
