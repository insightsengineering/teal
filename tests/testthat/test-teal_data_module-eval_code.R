testthat::test_that("within.teal_data_module returns an object with teal_data_module class", {
  tdm <- teal_data_module(
    ui = function(id) tags$div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive(teal.data::teal_data(IRIS = iris))
      })
    }
  )

  tdm2 <- within(tdm, IRIS$id <- seq_len(nrow(IRIS))) # nolint: object_name.

  testthat::expect_s3_class(tdm2, "teal_data_module")
})

testthat::test_that("eval_code.teal_data_module ui has modified namespace for id", {
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- NS(id)
      tags$div(id = ns("element"))
    },
    server = function(id) NULL
  )

  tdm2 <- eval_code(tdm, "1 + 1")

  output_ui <- tdm2$ui("top_level_ns")

  testthat::expect_match(output_ui$attribs$id, "^top_level_ns-")
  testthat::expect_match(output_ui$attribs$id, "-element$")
  testthat::expect_match(output_ui$attribs$id, "-mutate_inner-")
})

testthat::test_that("within.teal_data_module modifies the reactive tea_data object", {
  tdm <- teal_data_module(
    ui = function(id) tags$div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive(teal.data::teal_data(IRIS = iris))
      })
    }
  )

  tdm2 <- within(tdm, IRIS$id <- seq_len(nrow(IRIS))) # nolint: object_name.

  testthat::expect_no_error(
    shiny::testServer(
      app = tdm2$server,
      expr = {
        testthat::expect_identical(
          td()[["IRIS"]],
          within(iris, id <- seq_len(NROW(Species)))
        )
      }
    )
  )
})

testthat::test_that("eval_code.teal_data_module will execute several times until error", {
  tdm <- teal_data_module(
    ui = function(id) tags$div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive(teal.data::teal_data(IRIS = iris))
      })
    }
  )

  tdm2 <- eval_code(tdm, quote(stop_me <- FALSE))
  tdm2 <- eval_code(tdm2, "stopifnot(previous_error = stop_me)")

  testthat::expect_no_error(
    shiny::testServer(
      app = tdm2$server,
      expr = {
        testthat::expect_s3_class(td(), "qenv.error")
        testthat::expect_match(td()$message, "previous_error.*when evaluating qenv code")
      }
    )
  )
})

testthat::test_that("eval_code.teal_data_module throws error when original teal_data_module result is not reactive", {
  tdm <- teal_data_module(
    ui = function(id) tags$div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        "I am not reactive, I am a string"
      })
    }
  )

  tdm2 <- eval_code(tdm, "1 + 1")

  testthat::expect_error(
    shiny::testServer(
      app = tdm2$server,
      expr = {}
    ),
    "Must be a reactive"
  )
})

testthat::test_that("eval_code.teal_data_module propagates qenv error from the original/first call", {
  tdm <- teal_data_module(
    ui = function(id) tags$div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        reactive(
          within(teal.data::teal_data(IRIS = iris), non_existing_var + 1)
        )
      })
    }
  )

  tdm2 <- eval_code(tdm, "IRIS$const <- 1 + 1")

  testthat::expect_no_error(
    shiny::testServer(
      app = tdm2$server,
      expr = {
        testthat::expect_s3_class(
          td(),
          "qenv.error"
        )
      }
    )
  )
})

testthat::test_that("eval_code.teal_data_module handles an arbitrary object (other than `teal_data` or `qenv.error`)", {
  tdm <- teal_data_module(
    ui = function(id) tags$div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        reactive(list())
      })
    }
  )

  tdm2 <- eval_code(tdm, "1 + 1")

  testthat::expect_no_error(
    shiny::testServer(
      app = tdm2$server,
      expr = {
        testthat::expect_identical(
          td(),
          list()
        )
      }
    )
  )
})

testthat::test_that("eval_code.teal_data_module handles a `NULL` result", {
  tdm <- teal_data_module(
    ui = function(id) tags$div(),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        reactive(NULL)
      })
    }
  )

  tdm2 <- eval_code(tdm, "1 + 1")

  testthat::expect_no_error(
    shiny::testServer(
      app = tdm2$server,
      expr = {
        testthat::expect_null(td())
      }
    )
  )
})

testthat::test_that("teal_data_module preserves once attribute after calling eval_code and within", {
  testthat::expect_true(
    attr(
      eval_code(
        teal_data_module(ui = function(id) NULL, server = function(id) NULL, once = TRUE),
        code = "a <- 1"
      ),
      "once"
    )
  )
  testthat::expect_false(
    attr(
      eval_code(
        teal_data_module(ui = function(id) NULL, server = function(id) NULL, once = FALSE),
        code = "a <- 1"
      ),
      "once"
    )
  )

  testthat::expect_true(
    attr(
      within(
        teal_data_module(ui = function(id) NULL, server = function(id) NULL, once = TRUE),
        expr = a <- 1
      ),
      "once"
    )
  )
  testthat::expect_false(
    attr(
      within(
        teal_data_module(ui = function(id) NULL, server = function(id) NULL, once = FALSE),
        expr = a <- 1
      ),
      "once"
    )
  )
})
