testthat::test_that("The constructor accepts a CDISCDataset and an empty list", {
  testthat::expect_error(CDISCFilteredDataset$new(
    dataset = CDISCDataset$new(
      "iris",
      head(iris),
      parent = character(0),
      keys = c("Petal.Length")
    ),
    join_keys = list()
  ), NA)
})

testthat::test_that("get_call returns a list of calls", {
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCDataset$new(
      "iris",
      head(iris),
      parent = character(0),
      keys = c("Petal.Length")
    ),
    join_keys = list()
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
    ),
    join_keys = list()
  )
  testthat::expect_true(is_class_list("language")(filtered_dataset$get_call()))
})

testthat::test_that("get_subjects_info(FALSE) returns number of unique
  observations in the columns designated by the keys argument", {
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCDataset$new(
      "iris",
      head(iris),
      parent = character(0),
      keys = c("Petal.Length")
    ),
    join_keys = list()
  )
  testthat::expect_equal(filtered_dataset$get_subjects_info(FALSE), length(unique(head(iris)$Petal.Length)))
})
