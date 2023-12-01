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

  testthat::expect_identical(
    shiny::isolate(tdm2$server("test")()[["IRIS"]]),
    within(iris, id <- seq_len(NROW(Species)))
  )
})

testthat::test_that("eval_code.teal_data_module will execute several executions until error", {
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

  tdm2 <- eval_code(tdm, "stop_me <- FALSE") %>%
    eval_code("stopifnot(previous_error = stop_me)")

  testthat::expect_error(
    shiny::isolate(tdm2$server("test")()[["IRIS"]]),
    "previous_error.*when evaluating qenv code"
  )
})

testthat::test_that("eval_code.teal_data_module throws error when result is not reactive", {
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() shiny::MockShinySession$new(),
    .package = "shiny"
  )

  tdm <- teal_data_module(
    ui = function(id) div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        "I am not reactive, I am a string"
      })
    }
  )

  tdm2 <- eval_code(tdm, "1 + 1")

  testthat::expect_error(
    shiny::isolate(tdm2$server("test")()[["IRIS"]]),
    "The `teal_data_module` must return a reactive expression."
  )
})

testthat::test_that("eval_code.teal_data_module throws error when result is not reactive", {
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() shiny::MockShinySession$new(),
    .package = "shiny"
  )

  tdm <- teal_data_module(
    ui = function(id) div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        "I am not reactive, I am a string"
      })
    }
  )

  tdm2 <- eval_code(tdm, "1 + 1")

  testthat::expect_error(
    shiny::isolate(tdm2$server("test")()[["IRIS"]]),
    "The `teal_data_module` must return a reactive expression."
  )
})

testthat::test_that("eval_code.teal_data_module propagates error from the original/first call", {
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() shiny::MockShinySession$new(),
    .package = "shiny"
  )

  tdm <- teal_data_module(
    ui = function(id) div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        reactive(
          validate(
            FALSE,
            "an error at the bottom/original teal_data_module"
          )
        )
      })
    }
  )

  tdm2 <- eval_code(tdm, "1 + 1")

  testthat::expect_error(
    shiny::isolate(tdm2$server("test")()),
    "an error at the bottom/original teal_data_module"
  )
})


testthat::test_that("eval_code.teal_data_module does not execute on a object (other than `teal_data` or `error`)", {
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() shiny::MockShinySession$new(),
    .package = "shiny"
  )

  tdm <- teal_data_module(
    ui = function(id) div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        reactive(list())
      })
    }
  )

  tdm2 <- eval_code(tdm, "1 + 1")

  testthat::expect_error(
    shiny::isolate(tdm2$server("test")()),
    "It must always return a reactive with `teal_data`"
  )
})
