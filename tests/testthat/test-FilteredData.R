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

testthat::test_that("get_varlabels returns variable labels of a Dataset", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(Dataset$new("iris", head(iris)))
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep(as.character(NA), ncol(iris)), nm = colnames(iris))
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
