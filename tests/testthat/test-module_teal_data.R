
testthat::test_that("ui_teal_data_module returns tagList with wrapper and validation UI via init", {
  data_module <- teal_data_module(
    ui = function(id) tags$div("test"),
    server = function(id) reactive(teal_data(iris = iris))
  )
  app <- init(
    data = data_module,
    modules = example_module(label = "Example Module")
  )
  testthat::expect_identical(class(app$ui), "function")
  testthat::expect_identical(class(app$server), "function")
})

testthat::test_that("srv_teal_data_module handles valid teal_data via srv_teal", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal_data_module(
        ui = function(id) NULL,
        server = function(id) {
          moduleServer(id, function(input, output, session) {
            reactive(teal_data(iris = iris))
          })
        }
      ),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_s4_class(data_handled(), "teal_data")
    }
  )
})

testthat::test_that("srv_teal_data_module handles error in data_module via srv_teal", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal_data_module(
        ui = function(id) NULL,
        server = function(id) {
          moduleServer(id, function(input, output, session) {
            reactive(stop("test error"))
          })
        }
      ),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_s3_class(data_handled(), "error")
    }
  )
})

testthat::test_that("srv_validate_reactive_teal_data handles qenv.error via srv_teal", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal_data_module(
        ui = function(id) NULL,
        server = function(id) {
          moduleServer(id, function(input, output, session) {
            reactive(within(teal_data(), stop("qenv error")))
          })
        }
      ),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_s3_class(data_handled(), "qenv.error")
    }
  )
})

testthat::test_that("srv_validate_reactive_teal_data handles shiny.error via srv_teal", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal_data_module(
        ui = function(id) NULL,
        server = function(id) {
          moduleServer(id, function(input, output, session) {
            reactive(structure(list(message = "test error"), class = c("error", "condition")))
          })
        }
      ),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_s3_class(data_handled(), "error")
    }
  )
})

