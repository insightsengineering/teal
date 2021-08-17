testthat::test_that("The constructor accepts a Dataset object and an empty list", {
  testthat::expect_error(FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  ), NA)
})

testthat::test_that("add_to_eval_env does not throw", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_error(filtered_dataset$add_to_eval_env("test", 7), NA)
})

testthat::test_that("queues_empty does not throw after initializing FilteredDataset", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_error(filtered_dataset$queues_empty(), NA)
})

testthat::test_that("get_data(FALSE) returns the object passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(filtered_dataset$get_data(filtered = FALSE), head(iris))
})

testthat::test_that("get_data(TRUE) throws an error due to Pure virtual method.", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_error(isolate(filtered_dataset$get_data(filtered = TRUE)), regex = "Pure virtual method.")
})

testthat::test_that("get_data_reactive throws an error due to pure virtual method", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_error(isolate(filtered_dataset$get_data_reactive()()), regex = "Pure virtual method")
})

testthat::test_that("get_filter_states returns an empty list after initialization", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(filtered_dataset$get_filter_states(), list())
})

testthat::test_that("get_data_info(FALSE) returns the number of rows of the unfiltered data", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(filtered_dataset$get_data_info(filtered = FALSE), nrow(head(iris)))
})

testthat::test_that("get_subjects_info(TRUE) and get_subjects_info(FALSE) return integer(0)", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(filtered_dataset$get_subjects_info(filtered = TRUE), integer(0))
  testthat::expect_equal(filtered_dataset$get_subjects_info(filtered = FALSE), integer(0))
})

testthat::test_that("get_dataname returns the name of the Dataset object passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(filtered_dataset$get_dataname(), "iris")
})

testthat::test_that("get_hash returns the hash of the Dataset object passed to the constructor", {
  dataset <- Dataset$new("iris", head(iris))
  filtered_dataset <- FilteredDataset$new(dataset = dataset, join_keys = list())
  testthat::expect_equal(dataset$get_hash(), filtered_dataset$get_hash())
})

testthat::test_that("get_keys returns the keys of the Dataset object passed to the constructor", {
  dataset <- Dataset$new("iris", head(iris), keys = c("Petal.length"))
  filtered_dataset <- FilteredDataset$new(dataset = dataset, join_keys = list())
  testthat::expect_equal(filtered_dataset$get_keys(), dataset$get_keys())
})

testthat::test_that("get_join_keys returns the list passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list(a = 1)
  )
  testthat::expect_equal(filtered_dataset$get_join_keys(), list(a = 1))
})

testthat::test_that("get_varlabels(NULL) returns a named array of NAs if Dataset has no varlabels", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(
    filtered_dataset$get_varlabels(),
    setNames(as.character(rep(NA, ncol(head(iris)))), nm = names(iris))
  )
})

testthat::test_that("get_varlabels returns labels for the part of the variables only", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(
    filtered_dataset$get_varlabels(variables = c("Petal.Length")),
    setNames(object = as.character(NA), nm = "Petal.Length")
  )
})

testthat::test_that("get_varnames returns the names of the variables in the data passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(filtered_dataset$get_varnames(), names(iris))
})

testthat::test_that("get_filtered_dataname returns <dataname>_FILTERED as default", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_equal(filtered_dataset$get_filtered_dataname(), "iris_FILTERED")
})

testthat::test_that("set_bookmark state is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_error(filtered_dataset$set_bookmark_state(), regex = "Pure virtual")
})

testthat::test_that("ui_add_filter_state is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_error(filtered_dataset$ui_add_filter_state(), regex = "Pure virtual")
})

testthat::test_that("srv_add_filter_state is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris)),
    join_keys = list()
  )
  testthat::expect_error(filtered_dataset$srv_add_filter_state(), regex = "Pure virtual")
})
