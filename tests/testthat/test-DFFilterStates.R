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

testthat::test_that("DFFilterStates$set_bookmark_state sets filters in FilterState(s) specified by the named list", {
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
  shiny::testServer(dffs$set_bookmark_state, args = list(state = fs, data = iris), expr = NULL)
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

testthat::test_that("Selecting a new 'var_to_add' initializes a new filter state", {
  dffs <- teal:::DFFilterStates$new(
    input_dataname = "iris",
    output_dataname = "iris_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  shiny::testServer(
    dffs$srv_add_filter_state,
    args = list(data = iris),
    expr = {
      session$setInputs(var_to_add = "Sepal.Length")
    }
  )

  testthat::expect_identical(
    isolate(dffs$get_call()),
    quote(iris_filtered <- dplyr::filter(iris, Sepal.Length >= 4.3 & Sepal.Length <= 7.9))
  )
})

testthat::test_that("Adding 'var_to_add' adds another filter state", {
  dffs <- teal:::DFFilterStates$new(
    input_dataname = "iris",
    output_dataname = "iris_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  shiny::testServer(
    dffs$srv_add_filter_state,
    args = list(data = iris),
    expr = {
      session$setInputs(var_to_add = "Sepal.Length")
    }
  )
  shiny::testServer(
    dffs$srv_add_filter_state,
    args = list(data = iris),
    expr = {
      session$setInputs(var_to_add = "Species")
    }
  )

  # testthat::expect_identical(
  #   isolate(dffs$get_call()),
  #   quote(
  #     iris_filtered <- dplyr::filter(
  #       iris,
  #       Sepal.Length >= 4.3 & Sepal.Length <= 7.9 & Species %in% c("setosa", "versicolor", "virginica")
  #     )
  #   )
  # )
})
