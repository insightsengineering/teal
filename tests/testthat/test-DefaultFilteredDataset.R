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

testthat::test_that("get_filter_overview_info returns right array for DefaultFilteredDataset without filtering", {
  testthat::expect_equal(
    isolate(DefaultFilteredDataset$new(dataset = Dataset$new("iris", head(iris)))$get_filter_overview_info()),
    matrix(list("6/6", ""), nrow = 1, dimnames = list(c("iris"), c("Obs", "Subjects")))
  )
})

testthat::test_that("get_filter_overview_info returns right array for DefaultFilteredDataset with filtering", {
  dataset_iris <- DefaultFilteredDataset$new(dataset = Dataset$new("iris", head(iris)))
  filter_state_iris <- ChoicesFilterState$new(c("setosa", "virginica"), varname = "Species")
  filter_state_iris$set_selected("virginica")

  queue <- dataset_iris$get_filter_states(1)
  queue$queue_push(filter_state_iris, queue_index = 1L, element_id = "Species")

  testthat::expect_equal(
    isolate(dataset_iris$get_filter_overview_info()),
    matrix(list("0/6", ""), nrow = 1, dimnames = list(c("iris"), c("Obs", "Subjects")))
  )
})
