testthat::test_that("The constructor accepts a TealDataset object and an empty list", {
  testthat::expect_error(DefaultFilteredDataset$new(
    dataset = TealDataset$new("iris", head(iris))
  ), NA)
})

testthat::test_that("get_call returns a list of calls", {
  filtered_dataset <- DefaultFilteredDataset$new(
    dataset = TealDataset$new("iris", head(iris))
  )
  checkmate::expect_list(filtered_dataset$get_call(), types = "<-")
})

testthat::test_that(
  "DefaultFilteredDataset$set_filter_state sets filters specified by list names",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset("iris", iris))

    fs <- list(
      Sepal.Length = c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    dataset$set_filter_state(state = fs)
    testthat::expect_equal(
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
  }
)

testthat::test_that(
  "DefaultFilteredDataset$set_filter_state throws error when list is not named",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset("iris", iris))
    fs <- list(
      c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    testthat::expect_error(dataset$set_filter_state(state = fs))
  }
)

testthat::test_that(
  "DefaultFilteredDataset$remove_filter_state removes desired filter",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset("iris", iris))
    fs <- list(
      Sepal.Length = c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    dataset$set_filter_state(state = fs)
    dataset$remove_filter_state("Species")

    testthat::expect_equal(
      isolate(dataset$get_call()),
      list(
        filter = quote(
          iris_FILTERED <- dplyr::filter( # nolint
            iris,
            Sepal.Length >= 5.1 & Sepal.Length <= 6.4
          )
        )
      )
    )
  }
)

testthat::test_that(
  "DefaultFilteredDataset$get_filter_state returns list identical to input",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset("iris", iris))
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
      Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
    dataset$set_filter_state(state = fs)
    testthat::expect_identical(isolate(dataset$get_filter_state()), fs)
  }
)

testthat::test_that(
  "DefaultFilteredDataset$remove_filter_state removes more than one filter",
  code = {
    dataset <- DefaultFilteredDataset$new(dataset("iris", iris))
    fs <- list(
      Sepal.Length = c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    dataset$set_filter_state(state = fs)
    dataset$remove_filter_state(c("Species", "Sepal.Length"))

    testthat::expect_equal(
      isolate(dataset$get_call()),
      list(
        filter = quote(iris_FILTERED <- iris) # nolint
      )
    )
  }
)

testthat::test_that("get_filter_overview_info returns overview matrix for DefaultFilteredDataset without filtering", {
  testthat::expect_equal(
    isolate(DefaultFilteredDataset$new(dataset = TealDataset$new("iris", head(iris)))$get_filter_overview_info()),
    matrix(list("6/6", ""), nrow = 1, dimnames = list(c("iris"), c("Obs", "Subjects")))
  )
})

testthat::test_that("get_filter_overview_info returns overview matrix for DefaultFilteredDataset with filtering", {
  dataset_iris <- DefaultFilteredDataset$new(dataset = TealDataset$new("iris", head(iris)))
  filter_state_iris <- ChoicesFilterState$new(c("setosa", "virginica"), varname = "Species")
  filter_state_iris$set_selected("virginica")
  queue <- dataset_iris$get_filter_states(1)
  queue$queue_push(filter_state_iris, queue_index = 1L, element_id = "Species")
  testthat::expect_equal(
    isolate(dataset_iris$get_filter_overview_info()),
    matrix(list("0/6", ""), nrow = 1, dimnames = list(c("iris"), c("Obs", "Subjects")))
  )
})
