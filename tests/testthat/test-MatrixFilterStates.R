testthat::test_that("The constructor does not throw", {
    testthat::expect_error(MatrixFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  ), NA)
})
