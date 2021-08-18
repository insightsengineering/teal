## CDISCDataset ====
testthat::test_that("CDISCDataset basics", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  testthat::expect_error(
    CDISCDataset$new(dataname = "abc", x = x)
  )

  testthat::expect_silent({
    test_ds <- CDISCDataset$new(
      dataname = "testds",
      x = x,
      keys = "x",
      parent = "testds2"
    )
  })

  testthat::expect_equal(
    test_ds$get_parent(),
    "testds2"
  )

  testthat::expect_silent(test_ds$set_parent("testds3"))
  testthat::expect_equal(
    test_ds$get_parent(),
    "testds3"
  )
})

testthat::test_that("CDISCDataset$get_code() does not return duplicated code when
  CDISCDataset$mutate method is called", {
  iris_dataset <- CDISCDataset$new("iris", head(iris), code = "head(iris)", parent = character(0), keys = c("test"))
  mtcars_dataset <- CDISCDatasetConnector$new(
    "mtcars",
    callable_function(function() head(mtcars)),
    parent = character(0),
    keys = c("test")
  )
  mtcars_dataset$pull()
  mtcars_dataset$mutate("'mutating connector'")

  iris_dataset$mutate("'mutating dataset'", vars = list(mtcars_dataset = mtcars_dataset))
  testthat::expect_equal(
    iris_dataset$get_code(),
    paste(
      "mtcars <- (function() head(mtcars))()",
      "\"mutating connector\"",
      "mtcars_dataset <- mtcars",
      "head(iris)",
      "\"mutating dataset\"",
      sep = "\n"
    )
  )
})
