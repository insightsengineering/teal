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

testthat::test_that(
  "DFFilterStates$set_filter_state sets filters in FilterState(s) specified by the named list", {
    dffs <- teal:::DFFilterStates$new(
      input_dataname = "iris",
      output_dataname = "iris_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      Sepal.Length = c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    dffs$set_filter_state(state = fs, data = iris)
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
  }
)

testthat::test_that("DFFilterStates$set_filter_state sets filters as a named/unnamed selected list", {
  dffs <- teal:::DFFilterStates$new(
    input_dataname = "iris",
    output_dataname = "iris_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  fs <- list(
    Sepal.Length = list(c(5.1, 6.4)),
    Species = list(selected = c("setosa", "versicolor"))
  )
  dffs$set_filter_state(state = fs, data = iris)
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

testthat::test_that(
  "DFFilterStates$set_filter_state throws error when using an unnamed list", {
    dffs <- teal:::DFFilterStates$new(
      input_dataname = "iris",
      output_dataname = "iris_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      c(5.1, 6.4),
      Species = c("setosa", "versicolor")
    )
    testthat::expect_error(dffs$set_filter_state(state = fs, data = iris))
  }
)

testthat::test_that("Selecting a new variable initializes a new filter state", {
  dffs <- teal:::DFFilterStates$new(
    input_dataname = "iris",
    output_dataname = "iris_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  expect_null(
    dffs$queue_get(queue_index = 1, element_id = "Sepal.Length")
  )
  shiny::testServer(
    dffs$srv_add_filter_state,
    args = list(data = iris),
    expr = {
      session$setInputs(var_to_add = "Sepal.Length")
    }
  )

  expect_is(
    dffs$queue_get(queue_index = 1, element_id = "Sepal.Length"),
    "list"
  )
  expect_is(
    dffs$queue_get(queue_index = 1, element_id = "Sepal.Length")[[1]],
    "RangeFilterState"
  )
  expect_identical(
    dffs$queue_get(queue_index = 1, element_id = "Sepal.Length")[[1]]$get_varname(deparse = TRUE),
    "Sepal.Length"
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

  testthat::expect_identical(
    isolate(dffs$get_call()),
    quote(
      iris_filtered <- dplyr::filter(
        iris,
        Sepal.Length >= 4.3 & Sepal.Length <= 7.9 & Species %in% c("setosa", "versicolor", "virginica")
      )
    )
  )
})

testthat::test_that(
  "DFFilterStates$remove_filter_state removes specified filter in FilterState(s)", {
    dffs <- teal:::DFFilterStates$new(
      input_dataname = "iris",
      output_dataname = "iris_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4)),
      Species = list(selected = c("setosa", "versicolor"))
    )

    dffs$set_filter_state(state = fs, data = iris)
    dffs$remove_filter_state("Species")

    testthat::expect_equal(
      isolate(dffs$get_call()),
      quote(iris_filtered <- dplyr::filter(iris, Sepal.Length >= 5.1 & Sepal.Length <= 6.4))
    )
  }
)
