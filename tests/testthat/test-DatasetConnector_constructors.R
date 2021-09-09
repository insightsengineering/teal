testthat::test_that("code_dataset_connector returns the same object as DatasetConnector", {
  x <- code_dataset_connector(dataname = "test", code = "head(mtcars)")
  expected <- DatasetConnector$new(dataname = "test", pull_callable = callable_function(function() head(mtcars)))
  x$pull()
  expected$pull()
  testthat::expect_equal(x$get_dataset()$data, expected$get_dataset()$data)
})
