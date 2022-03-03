testthat::test_that("code_dataset_connector returns the same object as TealDatasetConnector", {
  x <- code_dataset_connector(dataname = "test", code = "head(mtcars)")
  expected <- teal.data:::TealDatasetConnector$new(dataname = "test", pull_callable =  teal.data::callable_function(function() head(mtcars)))
  x$pull()
  expected$pull()
  testthat::expect_equal(x$get_dataset()$data, expected$get_dataset()$data)
})
