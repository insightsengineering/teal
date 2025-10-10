# Test file for module_teal_data.R functions
# Tests cover UI and server functions for teal_data_module handling

# Helper function to create test modules - defined globally
create_test_module <<- function(label = "test_module", datanames = "iris") {
  module(label, server = function(id, data) data, datanames = datanames)
}

testthat::describe("ui_teal_data_module", {
  testthat::it("creates proper UI structure with data_module", {
    testthat::expect_no_error({
      ui <- ui_teal_data_module("test_id", function(id) tags$div("test"))
      testthat::expect_s3_class(ui, "shiny.tag.list")
    })
  })

  testthat::it("creates UI with NULL data_module", {
    testthat::expect_no_error({
      ui <- ui_teal_data_module("test_id", function(id) NULL)
      testthat::expect_s3_class(ui, "shiny.tag.list")
    })
  })

  testthat::it("throws error for invalid id", {
    testthat::expect_error(
      ui_teal_data_module(123, function(id) NULL),
      "Must be of type 'string'"
    )
  })

  testthat::it("throws error for invalid data_module", {
    testthat::expect_error(
      ui_teal_data_module("test_id", "not_a_function"),
      "Must be a function"
    )
  })

  testthat::it("throws error for data_module with wrong arguments", {
    testthat::expect_error(
      ui_teal_data_module("test_id", function(x) NULL),
      "Must have formal arguments: id"
    )
  })
})


testthat::describe("ui_validate_reactive_teal_data", {
  testthat::it("creates proper UI structure", {
    testthat::expect_no_error({
      ui <- ui_validate_reactive_teal_data("test_id")
      testthat::expect_s3_class(ui, "shiny.tag")
    })
  })

  testthat::it("creates UI with numeric id (no validation)", {
    testthat::expect_no_error({
      ui <- ui_validate_reactive_teal_data(123)
      testthat::expect_s3_class(ui, "shiny.tag")
    })
  })
})

testthat::describe("srv_validate_reactive_teal_data", {
  testthat::it("handles valid teal_data", {
    shiny::testServer(
      app = srv_validate_reactive_teal_data,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(iris = iris)),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        hide_validation_error = reactive(FALSE)
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles qenv.error", {
    shiny::testServer(
      app = srv_validate_reactive_teal_data,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), stop("qenv error"))),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        hide_validation_error = reactive(FALSE)
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles shiny.silent.error with validation enabled", {
    shiny::testServer(
      app = srv_validate_reactive_teal_data,
      args = list(
        id = "test",
        data = reactive(structure(list(message = ""), class = "shiny.silent.error")),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        hide_validation_error = reactive(FALSE)
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles shiny.silent.error with validation disabled", {
    shiny::testServer(
      app = srv_validate_reactive_teal_data,
      args = list(
        id = "test",
        data = reactive(structure(list(message = ""), class = "shiny.silent.error")),
        modules = NULL,
        validate_shiny_silent_error = FALSE,
        hide_validation_error = reactive(FALSE)
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles hide_validation_error TRUE", {
    shiny::testServer(
      app = srv_validate_reactive_teal_data,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(iris = iris)),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        hide_validation_error = reactive(TRUE)
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("throws error for invalid id", {
    testthat::expect_error(
      srv_validate_reactive_teal_data(123, reactive(teal.data::teal_data())),
      "Must be of type 'string'"
    )
  })

  testthat::it("throws error for invalid modules", {
    testthat::expect_error(
      srv_validate_reactive_teal_data("test", reactive(teal.data::teal_data()), modules = "invalid"),
      "Must inherit from class"
    )
  })

  testthat::it("throws error for invalid validate_shiny_silent_error", {
    testthat::expect_error(
      srv_validate_reactive_teal_data("test", reactive(teal.data::teal_data()), validate_shiny_silent_error = "invalid"),
      "Must be of type 'logical flag'"
    )
  })
})


