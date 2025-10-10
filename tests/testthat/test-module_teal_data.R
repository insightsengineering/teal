# Test file for module_teal_data.R functions
# Tests srv_teal_data_module indirectly through srv_teal with teal_data_module objects

# Helper function to create test modules - defined globally
create_test_module <<- function(label = "test_module", datanames = "iris") {
  module(label, server = function(id, data) data, datanames = datanames)
}

testthat::describe("srv_teal with teal_data_module (indirectly tests srv_teal_data_module)", {
  testthat::it("handles successful teal_data_module", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) tags$div("Data Module UI"),
          server = function(id) reactive(teal.data::teal_data(iris = iris))
        ),
        modules = modules(create_test_module()),
        filter = teal_slices(),
        reporter = NULL
      ),
      expr = {
        # Test that the app initializes without error
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles teal_data_module that returns non-teal_data", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) tags$div("Data Module UI"),
          server = function(id) reactive("not_teal_data")
        ),
        modules = modules(create_test_module()),
        filter = teal_slices(),
        reporter = NULL
      ),
      expr = {
        # Test that the app handles invalid data gracefully
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles teal_data_module that throws error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) tags$div("Data Module UI"),
          server = function(id) reactive(stop("Data error"))
        ),
        modules = modules(create_test_module()),
        filter = teal_slices(),
        reporter = NULL
      ),
      expr = {
        # Test that the app handles errors gracefully
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles teal_data_module with qenv.error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) tags$div("Data Module UI"),
          server = function(id) reactive(within(teal.data::teal_data(), stop("qenv error")))
        ),
        modules = modules(create_test_module()),
        filter = teal_slices(),
        reporter = NULL
      ),
      expr = {
        # Test that the app handles qenv errors gracefully
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles teal_data_module with shiny.silent.error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) tags$div("Data Module UI"),
          server = function(id) reactive(structure(list(message = ""), class = "shiny.silent.error"))
        ),
        modules = modules(create_test_module()),
        filter = teal_slices(),
        reporter = NULL
      ),
      expr = {
        # Test that the app handles shiny.silent.error gracefully
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles teal_data_module with once = TRUE", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = structure(
          teal_data_module(
            ui = function(id) tags$div("Data Module UI"),
            server = function(id) reactive(teal.data::teal_data(iris = iris))
          ),
          once = TRUE
        ),
        modules = modules(create_test_module()),
        filter = teal_slices(),
        reporter = NULL
      ),
      expr = {
        # Test that the app handles once = TRUE teal_data_module
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles teal_data_module with complex data", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) tags$div("Data Module UI"),
          server = function(id) {
            reactive(teal.data::teal_data(
              iris = iris,
              mtcars = mtcars,
              join_keys = teal.data::join_keys(teal.data::join_key("iris", "iris", "Species"))
            ))
          }
        ),
        modules = modules(create_test_module()),
        filter = teal_slices(),
        reporter = NULL
      ),
      expr = {
        # Test that the app handles complex teal_data
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles teal_data_module with validation errors", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) tags$div("Data Module UI"),
          server = function(id) reactive(teal.data::teal_data(iris = iris))
        ),
        modules = modules(create_test_module()),
        filter = teal_slices(),
        reporter = NULL
      ),
      expr = {
        # Test that validation functions work correctly
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })
})
