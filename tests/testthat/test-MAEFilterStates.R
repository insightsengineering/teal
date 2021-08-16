testthat::test_that("The constructor does not throw", {
  testthat::expect_error(MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  ), NA)
})
