testthat::test_that("init_filtered_dataset returns a DefaultFilteredDataset when passed a TealDataset", {
  testthat::expect_error(filtered_dataset <- init_filtered_dataset(
    dataset = TealDataset$new("iris", head(iris))
  ), NA)
  testthat::expect_true(is(filtered_dataset, "DefaultFilteredDataset"))
})

testthat::test_that("init_filtered_dataset returns a CDISCFilteredDataset when passed a CDISCTealDataset", {
  testthat::expect_error(filtered_dataset <- init_filtered_dataset(
    dataset = CDISCTealDataset$new("iris", head(iris), parent = character(0), keys = c("Petal.Length"))
  ), NA)
  testthat::expect_true(is(filtered_dataset, "CDISCFilteredDataset"))
})

testthat::test_that("init_filtered_dataset returns an MAEFilteredDataset when passed an MAE", {
  library("MultiAssayExperiment")
  data("miniACC")
  mock_mae <- MAETealDataset$new("mock", miniACC)
  testthat::expect_error(filtered_dataset <- init_filtered_dataset(
    dataset = mock_mae
  ), NA)
  testthat::expect_true(is(filtered_dataset, "MAEFilteredDataset"))
})
