testthat::test_that("The constructor does not throw", {
  testthat::expect_error(FilteredData$new(), NA)
})

testthat::test_that("datanames returns an empty array after initialization", {
  filtered_data <- FilteredData$new()
  testthat::expect_equal(filtered_data$datanames(), c())
})

testthat::test_that("set_dataset does not throw when passed a Dataset object", {
  filtered_data <- FilteredData$new()
  testthat::expect_error(filtered_data$set_dataset(Dataset$new("iris", head(iris))), NA)
})

testthat::test_that("set_dataset does throw when passed a DatasetConnector object before pulling it", {
  filtered_data <- FilteredData$new()
  testthat::expect_error(filtered_data$set_dataset(
    DatasetConnector$new("iris", callable_function(function() head(iris)))
  ), regexp = "has not been pulled yet")
})

testthat::test_that("set_dataset does not throw when passed a pulled DatasetConnector object", {
  filtered_data <- FilteredData$new()
  connector <- DatasetConnector$new("iris", callable_function(function() head(iris)))
  connector$pull()
  testthat::expect_error(filtered_data$set_dataset(connector, NA))
})

testthat::test_that("set_join_keys does not throw when passed a JoinKeys object", {
  filtered_data <- FilteredData$new()
  testthat::expect_error(filtered_data$set_join_keys(JoinKeys$new()), NA)
})

testthat::test_that("get_keys returns an empty character a Dataset has no keys", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  testthat::expect_equal(filtered_data$get_keys("iris"), character(0))
})

test_that("get_keys returns the same character array if a Dataset has keys", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris), keys = c("test")))
  testthat::expect_equal(filtered_data$get_keys("iris"), "test")
})

testthat::test_that("get_varnames returns dataname's column names", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  testthat::expect_equal(filtered_data$get_varnames("iris"), colnames(iris))
})

testthat::test_that("get_varlabels returns an array of NAs when a Dataset has no variable labels", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep(as.character(NA), ncol(iris)), nm = colnames(iris))
  )
})

testthat::test_that("get_varlabels returns array's labels when a Dataset has variable labels", {
  mock_iris <- head(iris)
  rtables::var_labels(mock_iris) <- rep("test", ncol(mock_iris))
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", mock_iris))
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep("test", ncol(mock_iris)), nm = colnames(mock_iris))
  )
})

testthat::test_that("get_datalabel returns character(0) for a Dataset with no labels", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), character(0))
})

testthat::test_that("get_datalabel returns the label of a passed Dataset", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris), label = "test"))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), "test")
})

testthat::test_that("set_code does not throw when passed a CodeClass", {
  filtered_data <- FilteredData$new()
  testthat::expect_error(filtered_data$set_code(CodeClass$new("'preprocessing code'")), NA)
})

testthat::test_that("get_code return the code passed to set_code", {
  filtered_data <- FilteredData$new()
  filtered_data$set_code(CodeClass$new("'preprocessing code'"))
  testthat::expect_equal(filtered_data$get_code(), "\"preprocessing code\"")
})

testthat::test_that("get_data does not throw when passed a dataset name", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  testthat::expect_equal(isolate(filtered_data$get_data("iris")), head(iris))
})

testthat::test_that("get_filtered_datasets returns a list of FilteredDataset", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  testthat::expect_true(is_class_list("FilteredDataset")(filtered_data$get_filtered_datasets()))
})

testthat::test_that("get_filtered_datasets returns a list with elements names after set datasets", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  filtered_data$set_dataset(Dataset$new("mtcars", head(mtcars)))
  testthat::expect_equal(names(filtered_data$get_filtered_datasets()), c("iris", "mtcars"))
})

testthat::test_that("get_call returns a list of language objects", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  testthat::expect_true(is_class_list("language")(filtered_data$get_call("iris")))
})

testthat::test_that("get call returns a call assigning the filtered object to <name>_FILTERED", {
  mock_iris <- head(iris)
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("mock_iris", mock_iris))
  eval(filtered_data$get_call("mock_iris")[[1]])
  testthat::expect_equal(mock_iris_FILTERED, mock_iris)
})
