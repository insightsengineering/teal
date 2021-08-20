testthat::test_that("init_filter_states returns a DFFilterStates object if passed a data.frame", {
  testthat::expect_true(
    is(init_filter_states(data.frame(), input_dataname = "test", output_dataname = "test"), "DFFilterStates")
  )
})

testthat::test_that("init_filter_states returns a MatrixFilterStates object if passed a matrix", {
  testthat::expect_true(
    is(init_filter_states(matrix(), input_dataname = "test", output_dataname = "test"), "MatrixFilterStates")
  )
})

testthat::test_that("init_filter_states returns an MAEFilterStates object if passed an object of class MAE", {
  mock_mae <- structure(list(), class = "MultiAssayExperiment")
  testthat::expect_true(
    is(init_filter_states(
      mock_mae,
      input_dataname = "test",
      output_dataname = "test",
      varlabels = "test"
    ), "MAEFilterStates")
  )
})

testthat::test_that("init_filter_states returns an SEFilterStates object if passed an object of class SE", {
  mock_se <- structure(list(), class = "SummarizedExperiment")
  testthat::expect_true(
    is(init_filter_states(
      mock_se,
      input_dataname = "test",
      output_dataname = "test"
    ), "SEFilterStates")
  )
})
