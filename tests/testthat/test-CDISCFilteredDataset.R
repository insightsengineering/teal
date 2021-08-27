testthat::test_that("The constructor accepts a CDISCDataset and an empty list", {
  testthat::expect_error(CDISCFilteredDataset$new(
    dataset = CDISCDataset$new(
      "iris",
      head(iris),
      parent = character(0),
      keys = c("Petal.Length")
    )
  ), NA)
})

testthat::test_that("get_call returns a list of calls", {
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCDataset$new(
      "iris",
      head(iris),
      parent = character(0),
      keys = c("Petal.Length")
    )
  )
  testthat::expect_true(is_class_list("language")(filtered_dataset$get_call()))
})

testthat::test_that("get_call returns a list call for a CDISCDataset with a parent", {
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCDataset$new(
      "iris",
      head(iris),
      parent = "test",
      keys = c("Petal.Length")
    )
  )
  testthat::expect_true(is_class_list("language")(filtered_dataset$get_call()))
})
