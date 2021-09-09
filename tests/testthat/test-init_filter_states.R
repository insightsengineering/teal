testthat::test_that("init_filter_states returns a DFFilterStates object if passed an empty data.frame", {
  testthat::expect_error(
    filter_states <- init_filter_states(data.frame(), input_dataname = "test", output_dataname = "test"),
    NA
  )
  testthat::expect_true(
    is(filter_states, "DFFilterStates")
  )
})

testthat::test_that("init_filter_states returns a MatrixFilterStates object if passed an empty matrix", {
  testthat::expect_error(
    filter_states <- init_filter_states(matrix(), input_dataname = "test", output_dataname = "test"),
    NA
  )
  testthat::expect_true(
    is(filter_states, "MatrixFilterStates")
  )
})

testthat::test_that("init_filter_states returns an MAEFilterStates object if passed an object of class MAE", {
  mock_mae <- structure(list(), class = "MultiAssayExperiment")
  testthat::expect_error(
    filter_states <- init_filter_states(
      mock_mae,
      input_dataname = "test",
      output_dataname = "test",
      varlabels = "test"
    ),
    NA
  )
  testthat::expect_true(is(filter_states, "MAEFilterStates"))
})

testthat::test_that("init_filter_states returns an SEFilterStates object if passed an object of class SE", {
  mock_se <- structure(list(), class = "SummarizedExperiment")
  testthat::expect_error(
    filter_states <- init_filter_states(
      mock_se,
      input_dataname = "test",
      output_dataname = "test"
    ),
    NA
  )
  testthat::expect_true(is(filter_states, "SEFilterStates"))
})
