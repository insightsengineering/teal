testthat::test_that("The contructor accepts a string as varlabels and keys", {
  testthat::expect_error(DFFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  ), NA)
})

testthat::test_that("get_fun returns dplyr::filter", {
  filter_states <- DFFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = "test",
    keys = "test"
  )
  testthat::expect_equal(filter_states$get_fun(), "dplyr::filter")
})

testthat::test_that("set_bookmark_state sets correct filters for DefaultFilteredDataset", {
  dffs <- teal:::DFFilterStates$new(
    input_dataname = "iris",
    output_dataname = "iris_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  fs <- list(
    Sepal.Length = list(selected = c(5.1, 6.4)),
    Species = c("setosa", "versicolor")
  )
  testthat::expect_error(dffs$set_bookmark_state(state = fs, data = iris), NA)
  testthat::expect_equal(
    isolate(dffs$get_call()),
    quote(
      iris_filtered <- dplyr::filter(
        iris,
        Sepal.Length >= 5.1 & Sepal.Length <= 6.4 &
          Species %in% c("setosa", "versicolor")
      )
    )
  )
})
