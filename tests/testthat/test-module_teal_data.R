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

testthat::describe("srv_teal_data_module", {
  testthat::it("initializes successfully with valid inputs", {
    testthat::expect_no_error({
      shiny::testServer(
        app = srv_teal_data_module,
        args = list(
          id = "test",
          data_module = function(id) reactive(teal.data::teal_data(iris = iris)),
          modules = NULL,
          validate_shiny_silent_error = TRUE,
          is_transform_failed = reactiveValues()
        ),
        expr = {
          testthat::expect_false(is_transform_failed[["test"]])
        }
      )
    })
  })

  testthat::it("handles data_module that returns teal_data", {
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(
        id = "test",
        data_module = function(id) reactive(teal.data::teal_data(iris = iris)),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        is_transform_failed = reactiveValues()
      ),
      expr = {
        testthat::expect_false(is_transform_failed[["test"]])
      }
    )
  })

  testthat::it("handles data_module that returns non-teal_data", {
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(
        id = "test",
        data_module = function(id) reactive("not_teal_data"),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        is_transform_failed = reactiveValues()
      ),
      expr = {
        session$flushReact()
        testthat::expect_true(is_transform_failed[["test"]])
      }
    )
  })

  testthat::it("handles data_module that throws error", {
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(
        id = "test",
        data_module = function(id) reactive(stop("test error")),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        is_transform_failed = reactiveValues()
      ),
      expr = {
        session$flushReact()
        testthat::expect_true(is_transform_failed[["test"]])
      }
    )
  })

  testthat::it("handles previous transform failures", {
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(
        id = "test2",
        data_module = function(id) reactive(teal.data::teal_data(iris = iris)),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        is_transform_failed = reactiveValues(test1 = TRUE)
      ),
      expr = {
        testthat::expect_true(is_previous_failed())
      }
    )
  })

  testthat::it("handles no previous transform failures", {
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(
        id = "test1",
        data_module = function(id) reactive(teal.data::teal_data(iris = iris)),
        modules = NULL,
        validate_shiny_silent_error = TRUE,
        is_transform_failed = reactiveValues(test2 = FALSE)
      ),
      expr = {
        testthat::expect_false(is_previous_failed())
      }
    )
  })

  testthat::it("throws error for invalid id", {
    testthat::expect_error(
      srv_teal_data_module(123, function(id) NULL),
      "Must be of type 'string'"
    )
  })

  testthat::it("throws error for invalid data_module", {
    testthat::expect_error(
      srv_teal_data_module("test", "not_function"),
      "Must be a function"
    )
  })

  testthat::it("throws error for invalid modules", {
    testthat::expect_error(
      srv_teal_data_module("test", function(id) NULL, modules = "invalid"),
      "Must inherit from class"
    )
  })

  testthat::it("throws error for invalid is_transform_failed", {
    testthat::expect_error(
      srv_teal_data_module("test", function(id) NULL, is_transform_failed = "invalid"),
      "Must inherit from class 'reactivevalues'"
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

testthat::describe("ui_validate_error", {
  testthat::it("creates proper UI structure", {
    testthat::expect_no_error({
      ui <- ui_validate_error("test_id")
      testthat::expect_s3_class(ui, "shiny.tag")
    })
  })

  testthat::it("creates UI with numeric id (no validation)", {
    testthat::expect_no_error({
      ui <- ui_validate_error(123)
      testthat::expect_s3_class(ui, "shiny.tag")
    })
  })
})

testthat::describe("srv_validate_error", {
  testthat::it("handles qenv.error", {
    shiny::testServer(
      app = srv_validate_error,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), stop("qenv error"))),
        validate_shiny_silent_error = TRUE
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles regular error", {
    shiny::testServer(
      app = srv_validate_error,
      args = list(
        id = "test",
        data = reactive(stop("regular error")),
        validate_shiny_silent_error = TRUE
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
      app = srv_validate_error,
      args = list(
        id = "test",
        data = reactive(structure(list(message = ""), class = "shiny.silent.error")),
        validate_shiny_silent_error = TRUE
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
      app = srv_validate_error,
      args = list(
        id = "test",
        data = reactive(structure(list(message = ""), class = "shiny.silent.error")),
        validate_shiny_silent_error = FALSE
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles valid data", {
    shiny::testServer(
      app = srv_validate_error,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(iris = iris)),
        validate_shiny_silent_error = TRUE
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
      srv_validate_error(123, reactive(teal.data::teal_data())),
      "Must be of type 'string'"
    )
  })

  testthat::it("throws error for invalid validate_shiny_silent_error", {
    testthat::expect_error(
      srv_validate_error("test", reactive(teal.data::teal_data()), validate_shiny_silent_error = "invalid"),
      "Must be of type 'logical flag'"
    )
  })
})

testthat::describe("ui_check_class_teal_data", {
  testthat::it("creates proper UI structure", {
    testthat::expect_no_error({
      ui <- ui_check_class_teal_data("test_id")
      testthat::expect_s3_class(ui, "shiny.tag")
    })
  })

  testthat::it("creates UI with numeric id (no validation)", {
    testthat::expect_no_error({
      ui <- ui_check_class_teal_data(123)
      testthat::expect_s3_class(ui, "shiny.tag")
    })
  })
})

testthat::describe("srv_check_class_teal_data", {
  testthat::it("handles valid teal_data", {
    shiny::testServer(
      app = srv_check_class_teal_data,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(iris = iris))
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles error class", {
    shiny::testServer(
      app = srv_check_class_teal_data,
      args = list(
        id = "test",
        data = reactive(stop("test error"))
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles invalid class", {
    shiny::testServer(
      app = srv_check_class_teal_data,
      args = list(
        id = "test",
        data = reactive("not_teal_data")
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
      srv_check_class_teal_data(123, reactive(teal.data::teal_data())),
      "Must be of type 'string'"
    )
  })
})

testthat::describe("ui_check_module_datanames", {
  testthat::it("creates proper UI structure", {
    testthat::expect_no_error({
      ui <- ui_check_module_datanames("test_id")
      testthat::expect_s3_class(ui, "shiny.tag")
    })
  })

  testthat::it("creates UI with numeric id (no validation)", {
    testthat::expect_no_error({
      ui <- ui_check_module_datanames(123)
      testthat::expect_s3_class(ui, "shiny.tag")
    })
  })
})

testthat::describe("srv_check_module_datanames", {
  testthat::it("handles valid teal_data with matching datanames", {
    shiny::testServer(
      app = srv_check_module_datanames,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(iris = iris)),
        modules = modules(create_test_module())
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles valid teal_data with missing datanames", {
    shiny::testServer(
      app = srv_check_module_datanames,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(iris = iris)),
        modules = modules(create_test_module(datanames = c("iris", "missing")))
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles non-teal_data", {
    shiny::testServer(
      app = srv_check_module_datanames,
      args = list(
        id = "test",
        data = reactive("not_teal_data"),
        modules = modules(create_test_module())
      ),
      expr = {
        testthat::expect_no_error({
          session$flushReact()
        })
      }
    )
  })

  testthat::it("handles NULL modules", {
    shiny::testServer(
      app = srv_check_module_datanames,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(iris = iris)),
        modules = NULL
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
      srv_check_module_datanames(123, reactive(teal.data::teal_data())),
      "Must be of type 'string'"
    )
  })
})

testthat::describe(".trigger_on_success", {
  testthat::it("triggers on valid teal_data", {
    shiny::testServer(
      app = function(input, output, session) {
        data_reactive <- reactive(teal.data::teal_data(iris = iris))
        trigger_out <- .trigger_on_success(data_reactive)

        # Trigger the reactive to activate observeEvent
        data_reactive()
        session$flushReact()

        # Test the reactive value
        result <- trigger_out()
        testthat::expect_s4_class(result, "teal_data")
      },
      expr = {
        session$flushReact()
      }
    )
  })

  testthat::it("does not trigger on invalid data", {
    shiny::testServer(
      app = function(input, output, session) {
        data_reactive <- reactive("not_teal_data")
        trigger_out <- .trigger_on_success(data_reactive)

        # Trigger the reactive to activate observeEvent
        data_reactive()
        session$flushReact()

        # Test the reactive value
        result <- trigger_out()
        testthat::expect_null(result)
      },
      expr = {
        session$flushReact()
      }
    )
  })

  testthat::it("does not trigger on error", {
    shiny::testServer(
      app = function(input, output, session) {
        data_reactive <- reactive(tryCatch(stop("test error"), error = function(e) e))
        trigger_out <- .trigger_on_success(data_reactive)

        # Trigger the reactive to activate observeEvent
        data_reactive()
        session$flushReact()

        # Test the reactive value
        result <- trigger_out()
        testthat::expect_null(result)
      },
      expr = {
        session$flushReact()
      }
    )
  })

  testthat::it("updates when data changes", {
    shiny::testServer(
      app = function(input, output, session) {
        data_val <- reactiveVal(teal.data::teal_data(iris = iris))
        trigger_out <- .trigger_on_success(data_val)

        # Initial trigger
        data_val()
        session$flushReact()
        result1 <- trigger_out()
        testthat::expect_s4_class(result1, "teal_data")

        # Update data
        data_val(teal.data::teal_data(mtcars = mtcars))
        session$flushReact()
        result2 <- trigger_out()
        testthat::expect_s4_class(result2, "teal_data")
        testthat::expect_identical(names(result2), "mtcars")
      },
      expr = {
        session$flushReact()
      }
    )
  })

  testthat::it("does not trigger on identical data", {
    shiny::testServer(
      app = function(input, output, session) {
        data_val <- reactiveVal(teal.data::teal_data(iris = iris))
        trigger_out <- .trigger_on_success(data_val)

        # Initial trigger
        data_val()
        session$flushReact()
        initial_data <- trigger_out()
        testthat::expect_s4_class(initial_data, "teal_data")

        # Set same data again
        data_val(initial_data)
        session$flushReact()
        final_data <- trigger_out()
        testthat::expect_identical(final_data, initial_data)
      },
      expr = {
        session$flushReact()
      }
    )
  })
})

testthat::describe("Integration tests for module_teal_data", {
  testthat::it("handles complete workflow with valid data", {
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(
        id = "test",
        data_module = function(id) reactive(teal.data::teal_data(iris = iris)),
        modules = modules(create_test_module()),
        validate_shiny_silent_error = TRUE,
        is_transform_failed = reactiveValues()
      ),
      expr = {
        testthat::expect_false(is_transform_failed[["test"]])
        testthat::expect_false(is_previous_failed())
      }
    )
  })

  testthat::it("handles complete workflow with transform failure", {
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(
        id = "test",
        data_module = function(id) reactive("invalid_data"),
        modules = modules(create_test_module()),
        validate_shiny_silent_error = TRUE,
        is_transform_failed = reactiveValues()
      ),
      expr = {
        # Force evaluation of the reactive by accessing it through the validation
        # The validation functions should trigger the observeEvent
        session$flushReact()
        testthat::expect_true(is_transform_failed[["test"]])
      }
    )
  })

  testthat::it("handles complete workflow with previous failures", {
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(
        id = "test2",
        data_module = function(id) reactive(teal.data::teal_data(iris = iris)),
        modules = modules(create_test_module()),
        validate_shiny_silent_error = TRUE,
        is_transform_failed = reactiveValues(test1 = TRUE)
      ),
      expr = {
        testthat::expect_true(is_previous_failed())
      }
    )
  })
})
