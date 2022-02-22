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
  "DFFilterStates$set_filter_state sets filters in FilterState(s) specified by the named list",
  code = {
    dffs <- DFFilterStates$new(
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
  dffs <- DFFilterStates$new(
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

testthat::test_that("DFFilterStates$set_filter_state updates filter state which was set already", {
  dffs <- DFFilterStates$new(
    input_dataname = "iris",
    output_dataname = "iris_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  dffs$set_filter_state(
    state = list(Sepal.Length = c(5.1, 6.4), Species = c("setosa", "versicolor")),
    data = iris
  )
  dffs$set_filter_state(
    state = list(Species = "setosa", Petal.Length = c(2.0, 5.0)),
    data = iris
  )
  expect_identical(
    isolate(dffs$get_filter_state()),
    list(
      Sepal.Length = list(selected = c(5.1, 6.4), keep_na = FALSE, keep_inf = FALSE),
      Species = list(selected = "setosa", keep_na = FALSE),
      Petal.Length = list(selected = c(2.0, 5.0), keep_na = FALSE, keep_inf = FALSE)
    )
  )
})

testthat::test_that("DFFilterStates$set_filter_state throws error when using an unnamed list",
  code = {
    dffs <- DFFilterStates$new(
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

testthat::test_that("DFFilterStates$get_filter_state returns list identical to input",
  code = {
    dffs <- DFFilterStates$new(
      input_dataname = "iris",
      output_dataname = "iris_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
      Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    )
    dffs$set_filter_state(state = fs, data = iris)
    testthat::expect_identical(isolate(dffs$get_filter_state()), fs)
  }
)

testthat::test_that("Selecting a new variable initializes a new filter state", {
  dffs <- DFFilterStates$new(
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
  dffs <- DFFilterStates$new(
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
    quote(iris_filtered <- iris)
  )
})

testthat::test_that(
  "DFFilterStates$remove_filter_state removes specified filter in FilterState(s)",
  code = {
    dffs <- DFFilterStates$new(
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

testthat::test_that(
  "DFFilterStates$remove_filter_state throws warning when name is not in FilterStates",
  code = {
    suppress_logs()
    dffs <- DFFilterStates$new(
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
    testthat::expect_warning(dffs$remove_filter_state("Species2"))
  }
)
