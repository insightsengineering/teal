testthat::test_that("The contructor accepts a string as varlabels and keys", {
  testthat::expect_error(DFFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  ), NA)
})

testthat::test_that("get_fun returns dplyr::filter", {
  filter_states <- DFFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  )
  testthat::expect_equal(filter_states$get_fun(), "dplyr::filter")
})
