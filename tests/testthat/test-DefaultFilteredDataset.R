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

testthat::test_that("set_bookmark_state sets correct filters for DefaultFilteredDataset", {
  dataset <- teal:::DefaultFilteredDataset$new(dataset("iris", iris))
  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4)),
    Species = c("setosa", "versicolor")
  )
  expect_error(dataset$set_bookmark_state(fs), NA)
  expect_equal(
    isolate(dataset$get_call()),
    list(
      filter = quote(
        iris_FILTERED <- dplyr::filter( # nolint
          iris,
          Sepal.Length >= 5.1 & Sepal.Length <= 6.4 &
            Species %in% c("setosa", "versicolor")
        )
      )
    )
  )
})
