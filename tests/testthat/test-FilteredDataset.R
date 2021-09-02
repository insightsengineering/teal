testthat::test_that("The constructor accepts a Dataset object and an empty list", {
  testthat::expect_error(FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  ), NA)
})

testthat::test_that("add_to_eval_env does not throw", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_error(filtered_dataset$add_to_eval_env("test", 7), NA)
})

testthat::test_that("queues_empty does not throw after initializing FilteredDataset", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_error(filtered_dataset$queues_empty(), NA)
})

testthat::test_that("get_data(FALSE) returns the object passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_equal(filtered_dataset$get_data(filtered = FALSE), head(iris))
})

testthat::test_that("get_data(TRUE) throws an error due to Pure virtual method.", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_error(isolate(filtered_dataset$get_data(filtered = TRUE)), regex = "Pure virtual method.")
})

testthat::test_that("get_data_reactive throws an error due to pure virtual method", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_error(isolate(filtered_dataset$get_data_reactive()()), regex = "Pure virtual method")
})

testthat::test_that("get_filter_states returns an empty list after initialization", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_equal(filtered_dataset$get_filter_states(), list())
})

testthat::test_that("get_dataname returns the name of the Dataset object passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_equal(filtered_dataset$get_dataname(), "iris")
})

testthat::test_that("get_hash returns the hash of the Dataset object passed to the constructor", {
  dataset <- Dataset$new("iris", head(iris))
  filtered_dataset <- FilteredDataset$new(dataset = dataset)
  testthat::expect_equal(dataset$get_hash(), filtered_dataset$get_hash())
})

testthat::test_that("get_keys returns the keys of the Dataset object passed to the constructor", {
  dataset <- Dataset$new("iris", head(iris), keys = c("Petal.length"))
  filtered_dataset <- FilteredDataset$new(dataset = dataset)
  testthat::expect_equal(filtered_dataset$get_keys(), dataset$get_keys())
})

testthat::test_that("get_join_keys returns NULL", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_equal(filtered_dataset$get_join_keys(), NULL)
})

testthat::test_that("get_varlabels(NULL) returns a named array of NAs if Dataset has no varlabels", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_equal(
    filtered_dataset$get_varlabels(),
    setNames(as.character(rep(NA, ncol(head(iris)))), nm = names(iris))
  )
})

testthat::test_that("get_varlabels returns labels for the part of the variables only", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_equal(
    filtered_dataset$get_varlabels(variables = c("Petal.Length")),
    setNames(object = as.character(NA), nm = "Petal.Length")
  )
})

testthat::test_that("get_varnames returns the names of the variables in the data passed to the constructor", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_equal(filtered_dataset$get_varnames(), names(iris))
})

testthat::test_that("get_filtered_dataname returns <dataname>_FILTERED as default", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_equal(filtered_dataset$get_filtered_dataname(), "iris_FILTERED")
})

testthat::test_that("set_bookmark state is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_error(filtered_dataset$set_bookmark_state(), regex = "Pure virtual")
})

testthat::test_that("ui_add_filter_state is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_error(filtered_dataset$ui_add_filter_state(), regex = "Pure virtual")
})

testthat::test_that("srv_add_filter_state is pure virtual", {
  filtered_dataset <- FilteredDataset$new(
    dataset = Dataset$new("iris", head(iris))
  )
  testthat::expect_error(filtered_dataset$srv_add_filter_state(), regex = "Pure virtual")
})

library(scda)
library(MultiAssayExperiment)

datasets <- teal:::FilteredData$new()
adsl <- as.data.frame(as.list(setNames(nm = c(get_cdisc_keys("ADSL")))))
adsl$sex <- c("F")
mock_iris <- head(iris)
data("miniACC")

datasets$set_dataset(cdisc_dataset("ADSL", adsl))
datasets$set_dataset(dataset("mock_iris", mock_iris))
datasets$set_dataset(dataset("miniACC", miniACC))

test_that("get_filter_overview_info non-desired input argument", {
  expect_silent(isolate(datasets$get_filtered_datasets("ADSL")$get_filter_overview_info()))
  expect_error(isolate(datasets$get_filtered_datasets("")$get_filter_overview_info()))
  expect_error(isolate(datasets$get_filtered_datasets("AA")$get_filter_overview_info()))
})

test_that("get_filter_overview_info returns right array for CDISC dataset", {
  # without filter
  expect_equal(
    isolate(datasets$get_filtered_datasets("ADSL")$get_filter_overview_info()),
    matrix(list("1/1", "1/1"), nrow = 1, dimnames = list(c("ADSL"), c("Obs", "Subjects")))
  )

  # with filter
  filter_state_adsl <- ChoicesFilterState$new(c("F", "M"), varname = "sex")
  filter_state_adsl$set_selected("M")

  queue <- datasets$get_filtered_datasets("ADSL")$get_filter_states(1)
  queue$queue_push(filter_state_adsl, queue_index = 1L, element_id = "sex")

  expect_equal(
    isolate(datasets$get_filtered_datasets("ADSL")$get_filter_overview_info()),
    matrix(list("0/1", "0/1"), nrow = 1, dimnames = list(c("ADSL"), c("Obs", "Subjects")))
  )
})

test_that("get_filter_overview_info returns right array for non-cdisc dataset", {
  # without filter
  expect_equal(
    isolate(datasets$get_filtered_datasets("mock_iris")$get_filter_overview_info()),
    matrix(list("6/6", ""), nrow = 1, dimnames = list(c("mock_iris"), c("Obs", "Subjects")))
  )

  # with filter
  filter_state_iris <- ChoicesFilterState$new(c("setosa", "virginica"), varname = "Species")
  filter_state_iris$set_selected("virginica")

  queue <- datasets$get_filtered_datasets("mock_iris")$get_filter_states(1)
  queue$queue_push(filter_state_iris, queue_index = 1L, element_id = "Species")

  expect_equal(
    isolate(datasets$get_filtered_datasets("mock_iris")$get_filter_overview_info()),
    matrix(list("0/6", ""), nrow = 1, dimnames = list(c("mock_iris"), c("Obs", "Subjects")))
  )
})

test_that("get_filter_overview_info returns right array for MAE dataset", {
  # without filter
  expect_equal(
    isolate(datasets$get_filtered_datasets("miniACC")$get_filter_overview_info()),
    matrix(
      list("", "92/92", "79/79", "79/79", "90/90", "90/90", "46/46", "46/46", "90/90", "90/90", "80/80", "80/80"),
      nrow = 6,
      byrow = TRUE,
      dimnames = list(
        c("miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray", "- Mutations", "- miRNASeqGene"),
        c("Obs", "Subjects")
      )
    )
  )

  # with filter
  filter_state_mae <- ChoicesFilterState$new(
    x = c("white"),
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )

  queue <- datasets$get_filtered_datasets("miniACC")$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")

  expect_equal(
    isolate(datasets$get_filtered_datasets("miniACC")$get_filter_overview_info()),
    matrix(
      list("", "78/92", "66/79", "66/79", "76/90", "76/90", "35/46", "35/46", "77/90", "77/90", "67/80", "67/80"),
      nrow = 6,
      byrow = TRUE,
      dimnames = list(
        c("miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray", "- Mutations", "- miRNASeqGene"),
        c("Obs", "Subjects")
      )
    )
  )
})
