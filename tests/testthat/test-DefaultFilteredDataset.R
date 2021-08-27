testthat::test_that("The constructor accepts a Dataset object and an empty list", {
  testthat::expect_error(DefaultFilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  ), NA)
})

testthat::test_that("get_call returns a list of calls", {
  filtered_dataset <- DefaultFilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_true(is_class_list("language")(filtered_dataset$get_call()))
})
