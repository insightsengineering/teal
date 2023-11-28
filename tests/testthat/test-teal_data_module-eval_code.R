testthat::test_that("within.teal_data_module returns an object with teal_data_module class", {
  tdm <- teal_data_module(
    ui = function(id) div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive(teal.data::teal_data(IRIS = iris))
      })
    }
  )

  tdm2 <- within(tdm, IRIS$id <- seq_len(nrow(IRIS))) # nolint: object_name_linter.

  testthat::expect_s3_class(tdm2, "teal_data_module")
})

testthat::test_that("eval_code.teal_data_module ui has modified namespace for id", {
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- NS(id)
      div(id = ns("element"))
    },
    server = function(id) NULL
  )

  tdm2 <- within(tdm, 1 + 1)

  output_ui <- tdm2$ui("top_level_ns")

  testthat::expect_match(output_ui$attribs$id, "^top_level_ns-")
  testthat::expect_match(output_ui$attribs$id, "-element$")
  testthat::expect_match(output_ui$attribs$id, "-mutate_inner-")
})

testthat::test_that("eval_code.teal_data_module modifies the reactive teal_data object with expression parameter", {
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() shiny::MockShinySession$new(),
    .package = "shiny"
  )

  tdm <- teal_data_module(
    ui = function(id) div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive(teal.data::teal_data(IRIS = iris))
      })
    }
  )

  tdm2 <- eval_code(tdm, expression(IRIS$id <- seq_len(nrow(IRIS)))) # nolint: object_name_linter.

  # Columns were added via eval_code.teal_data_module
  testthat::expect_setequal(
    c(names(iris), "id"),
    colnames(shiny::isolate(tdm2$server("test")()[["IRIS"]]))
  )
})

testthat::test_that("eval_code.teal_data_module modifies the reactive teal_data object with quoted parameter", {
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() shiny::MockShinySession$new(),
    .package = "shiny"
  )

  tdm <- teal_data_module(
    ui = function(id) div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive(teal.data::teal_data(IRIS = iris))
      })
    }
  )

  tdm2 <- eval_code(tdm, quote(IRIS$id <- seq_len(nrow(IRIS)))) # nolint: object_name.

  # Columns were added via eval_code.teal_data_module
  testthat::expect_setequal(
    c(names(iris), "id"),
    colnames(shiny::isolate(tdm2$server("test")()[["IRIS"]]))
  )
})

testthat::test_that("within.teal_data_module modifies the reactive tea_data object", {
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() shiny::MockShinySession$new(),
    .package = "shiny"
  )

  tdm <- teal_data_module(
    ui = function(id) div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive(teal.data::teal_data(IRIS = iris))
      })
    }
  )

  tdm2 <- within(tdm, IRIS$id <- seq_len(nrow(IRIS))) # nolint: object_name_linter.

  # teal_data_modules are different
  testthat::expect_failure(
    testthat::expect_identical(
      shiny::isolate(tdm$server("test")()[["IRIS"]]),
      shiny::isolate(tdm2$server("test")()[["IRIS"]])
    )
  )

  # Columns were added via within.teal_data_module
  testthat::expect_setequal(
    c(names(iris), "id"),
    colnames(shiny::isolate(tdm2$server("test")()[["IRIS"]]))
  )

  # Original teal_data_module was left untouched
  testthat::expect_setequal(
    c(names(iris)),
    colnames(shiny::isolate(tdm$server("test")()[["IRIS"]]))
  )
})
