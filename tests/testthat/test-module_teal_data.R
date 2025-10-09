# ui_teal_data_module ----

testthat::test_that("ui_teal_data_module returns a tagList", {
  result <- ui_teal_data_module("test_id")
  testthat::expect_s3_class(result, "shiny.tag.list")
})

testthat::test_that("ui_teal_data_module validates id parameter", {
  testthat::expect_error(
    ui_teal_data_module(123),
    "Assertion on 'id' failed"
  )
  testthat::expect_error(
    ui_teal_data_module(c("id1", "id2")),
    "Assertion on 'id' failed"
  )
})

testthat::test_that("ui_teal_data_module validates data_module parameter", {
  testthat::expect_error(
    ui_teal_data_module("test_id", data_module = "not_a_function"),
    "Assertion on 'data_module' failed"
  )
  testthat::expect_error(
    ui_teal_data_module("test_id", data_module = function(x, y) NULL),
    "Assertion on 'data_module' failed"
  )
})

testthat::test_that("ui_teal_data_module generates correct namespace", {
  result <- ui_teal_data_module("test_id", data_module = function(id) shiny::div("test"))
  testthat::expect_true(grepl("test_id-wrapper", as.character(result)))
  testthat::expect_true(grepl("test_id-validate", as.character(result)))
})

testthat::test_that("ui_teal_data_module includes data_module UI", {
  data_module_ui <- function(id) shiny::div(id = id, "custom_ui")
  result <- ui_teal_data_module("test_id", data_module = data_module_ui)
  result_str <- as.character(result)
  testthat::expect_true(grepl("custom_ui", result_str))
})

# srv_teal_data_module ----

testthat::test_that("srv_teal_data_module validates id parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(id = 123),
      expr = NULL
    ),
    "Assertion on 'id' failed"
  )
})

testthat::test_that("srv_teal_data_module validates data_module parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(id = "test", data_module = "not_a_function"),
      expr = NULL
    ),
    "Assertion on 'data_module' failed"
  )
})

testthat::test_that("srv_teal_data_module validates modules parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(id = "test", modules = "invalid"),
      expr = NULL
    ),
    "Assertion on 'modules' failed"
  )
})

testthat::test_that("srv_teal_data_module validates is_transform_failed parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_teal_data_module,
      args = list(id = "test", is_transform_failed = list()),
      expr = NULL
    ),
    "Assertion on 'is_transform_failed' failed"
  )
})

testthat::test_that("srv_teal_data_module initializes is_transform_failed to FALSE", {
  data_module_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      reactive(teal_data(iris = iris))
    })
  }
  
  shiny::testServer(
    app = srv_teal_data_module,
    args = list(
      id = "test",
      data_module = data_module_server
    ),
    expr = {
      testthat::expect_false(is_transform_failed[["test"]])
    }
  )
})

testthat::test_that("srv_teal_data_module sets is_transform_failed to TRUE when data is not teal_data", {
  data_module_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      reactive("not_teal_data")
    })
  }
  
  shiny::testServer(
    app = srv_teal_data_module,
    args = list(
      id = "test",
      data_module = data_module_server
    ),
    expr = {
      session$flushReact()
      testthat::expect_true(is_transform_failed[["test"]])
    }
  )
})

testthat::test_that("srv_teal_data_module sets is_transform_failed to FALSE when data is teal_data", {
  data_module_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      reactive(teal_data(iris = iris))
    })
  }
  
  shiny::testServer(
    app = srv_teal_data_module,
    args = list(
      id = "test",
      data_module = data_module_server
    ),
    expr = {
      session$flushReact()
      testthat::expect_false(is_transform_failed[["test"]])
    }
  )
})

testthat::test_that("srv_teal_data_module detects previous failures correctly", {
  data_module_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      reactive(teal_data(iris = iris))
    })
  }
  
  transform_failed <- shiny::reactiveValues(first = TRUE, second = FALSE)
  
  shiny::testServer(
    app = srv_teal_data_module,
    args = list(
      id = "second",
      data_module = data_module_server,
      is_transform_failed = transform_failed
    ),
    expr = {
      session$flushReact()
      # second should detect that first failed
      testthat::expect_true(is_previous_failed())
    }
  )
})

testthat::test_that("srv_teal_data_module calls srv_validate_reactive_teal_data", {
  data_module_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      reactive(teal_data(iris = iris))
    })
  }
  
  shiny::testServer(
    app = srv_teal_data_module,
    args = list(
      id = "test",
      data_module = data_module_server,
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      # If srv_validate_reactive_teal_data runs without error, this passes
      testthat::expect_true(TRUE)
    }
  )
})

# ui_validate_reactive_teal_data ----

testthat::test_that("ui_validate_reactive_teal_data returns a tag", {
  result <- ui_validate_reactive_teal_data("test_id")
  testthat::expect_s3_class(result, "shiny.tag")
})

testthat::test_that("ui_validate_reactive_teal_data creates proper structure", {
  result <- ui_validate_reactive_teal_data("test_id")
  result_str <- as.character(result)
  testthat::expect_true(grepl("test_id-validate_messages", result_str))
  testthat::expect_true(grepl("test_id-previous_failed", result_str))
  testthat::expect_true(grepl("teal_validated", result_str))
})

# srv_validate_reactive_teal_data ----

testthat::test_that("srv_validate_reactive_teal_data validates id parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_validate_reactive_teal_data,
      args = list(id = 123, data = reactive(teal_data())),
      expr = NULL
    ),
    "Assertion on 'id' failed"
  )
})

testthat::test_that("srv_validate_reactive_teal_data validates modules parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_validate_reactive_teal_data,
      args = list(id = "test", data = reactive(teal_data()), modules = "invalid"),
      expr = NULL
    ),
    "Assertion on 'modules' failed"
  )
})

testthat::test_that("srv_validate_reactive_teal_data validates validate_shiny_silent_error parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_validate_reactive_teal_data,
      args = list(id = "test", data = reactive(teal_data()), validate_shiny_silent_error = "not_logical"),
      expr = NULL
    ),
    "Assertion on 'validate_shiny_silent_error' failed"
  )
})

testthat::test_that("srv_validate_reactive_teal_data returns reactive when data is valid teal_data", {
  shiny::testServer(
    app = srv_validate_reactive_teal_data,
    args = list(
      id = "test",
      data = reactive(teal_data(iris = iris))
    ),
    expr = {
      result <- session$returned()
      testthat::expect_true(is.reactive(result))
      testthat::expect_s4_class(result(), "teal_data")
    }
  )
})

testthat::test_that("srv_validate_reactive_teal_data hides validation messages when hide_validation_error is TRUE", {
  shiny::testServer(
    app = srv_validate_reactive_teal_data,
    args = list(
      id = "test",
      data = reactive(teal_data(iris = iris)),
      hide_validation_error = reactive(TRUE)
    ),
    expr = {
      session$flushReact()
      testthat::expect_true(grepl("One of previous transformators failed", output$previous_failed))
    }
  )
})

testthat::test_that("srv_validate_reactive_teal_data shows validation messages when hide_validation_error is FALSE", {
  shiny::testServer(
    app = srv_validate_reactive_teal_data,
    args = list(
      id = "test",
      data = reactive(teal_data(iris = iris)),
      hide_validation_error = reactive(FALSE)
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(output$previous_failed)
    }
  )
})

# srv_validate_error ----

testthat::test_that("srv_validate_error validates id parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_validate_error,
      args = list(id = 123, data = reactive(teal_data()), validate_shiny_silent_error = FALSE),
      expr = NULL
    ),
    "Assertion on 'id' failed"
  )
})

testthat::test_that("srv_validate_error validates validate_shiny_silent_error parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_validate_error,
      args = list(id = "test", data = reactive(teal_data()), validate_shiny_silent_error = "not_logical"),
      expr = NULL
    ),
    "Assertion on 'validate_shiny_silent_error' failed"
  )
})

testthat::test_that("srv_validate_error returns NULL when data is valid teal_data", {
  shiny::testServer(
    app = srv_validate_error,
    args = list(
      id = "test",
      data = reactive(teal_data(iris = iris)),
      validate_shiny_silent_error = FALSE
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(output$message)
    }
  )
})

testthat::test_that("srv_validate_error displays message for qenv.error", {
  qenv_error_data <- reactive({
    result <- within(teal_data(), stop("test error"))
    result
  })
  
  shiny::testServer(
    app = srv_validate_error,
    args = list(
      id = "test",
      data = qenv_error_data,
      validate_shiny_silent_error = FALSE
    ),
    expr = {
      session$flushReact()
      testthat::expect_true(grepl("Error when executing the `data` module", output$message))
    }
  )
})

testthat::test_that("srv_validate_error displays message for regular error", {
  error_data <- reactive(stop("regular error"))
  
  shiny::testServer(
    app = srv_validate_error,
    args = list(
      id = "test",
      data = error_data,
      validate_shiny_silent_error = TRUE
    ),
    expr = {
      session$flushReact()
      testthat::expect_true(grepl("Shiny error when executing the `data` module", output$message))
    }
  )
})

testthat::test_that("srv_validate_error returns NULL for shiny.silent.error when validate_shiny_silent_error is FALSE", {
  silent_error <- structure(
    list(message = ""),
    class = c("shiny.silent.error", "error", "condition")
  )
  
  shiny::testServer(
    app = srv_validate_error,
    args = list(
      id = "test",
      data = reactive(silent_error),
      validate_shiny_silent_error = FALSE
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(output$message)
    }
  )
})

# srv_check_class_teal_data ----

testthat::test_that("srv_check_class_teal_data validates id parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_check_class_teal_data,
      args = list(id = 123, data = reactive(teal_data())),
      expr = NULL
    ),
    "Assertion on 'id' failed"
  )
})

testthat::test_that("srv_check_class_teal_data returns NULL when data is teal_data", {
  shiny::testServer(
    app = srv_check_class_teal_data,
    args = list(
      id = "test",
      data = reactive(teal_data(iris = iris))
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(output$message)
    }
  )
})

testthat::test_that("srv_check_class_teal_data returns NULL when data is error", {
  shiny::testServer(
    app = srv_check_class_teal_data,
    args = list(
      id = "test",
      data = reactive(simpleError("test error"))
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(output$message)
    }
  )
})

testthat::test_that("srv_check_class_teal_data validates when data is not teal_data or error", {
  shiny::testServer(
    app = srv_check_class_teal_data,
    args = list(
      id = "test",
      data = reactive("not_teal_data")
    ),
    expr = {
      session$flushReact()
      testthat::expect_true(grepl("Did not receive `teal_data` object", output$message))
    }
  )
})

# srv_check_module_datanames ----

testthat::test_that("srv_check_module_datanames validates id parameter", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_check_module_datanames,
      args = list(id = 123, data = reactive(teal_data()), modules = NULL),
      expr = NULL
    ),
    "Assertion on 'id' failed"
  )
})

testthat::test_that("srv_check_module_datanames returns NULL when data is not teal_data", {
  shiny::testServer(
    app = srv_check_module_datanames,
    args = list(
      id = "test",
      data = reactive("not_teal_data"),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(output$message)
    }
  )
})

testthat::test_that("srv_check_module_datanames returns NULL when modules datanames match", {
  shiny::testServer(
    app = srv_check_module_datanames,
    args = list(
      id = "test",
      data = reactive(teal_data(iris = iris, mtcars = mtcars)),
      modules = modules(example_module(datanames = c("iris", "mtcars")))
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(output$message)
    }
  )
})

testthat::test_that("srv_check_module_datanames shows warning when datanames don't match", {
  testthat::skip_if_not_installed("rvest")
  
  shiny::testServer(
    app = srv_check_module_datanames,
    args = list(
      id = "test",
      data = reactive(teal_data(iris = iris)),
      modules = modules(example_module(datanames = c("iris", "missing_data")))
    ),
    expr = {
      session$flushReact()
      testthat::expect_true(grepl("missing_data", output$message))
    }
  )
})

# .trigger_on_success ----

testthat::test_that(".trigger_on_success returns reactive", {
  result <- shiny::testServer(
    app = function(input, output, session) {
      data <- reactive(teal_data(iris = iris))
      .trigger_on_success(data)
    },
    expr = {
      testthat::expect_true(is.reactive(session$returned()))
    }
  )
})

testthat::test_that(".trigger_on_success triggers on teal_data", {
  shiny::testServer(
    app = function(input, output, session) {
      data <- reactiveVal(NULL)
      result <- .trigger_on_success(data)
      
      observe({
        data(teal_data(iris = iris))
      })
      
      result
    },
    expr = {
      session$flushReact()
      testthat::expect_s4_class(session$returned()(), "teal_data")
    }
  )
})

testthat::test_that(".trigger_on_success does not trigger on error", {
  shiny::testServer(
    app = function(input, output, session) {
      data <- reactive(simpleError("test error"))
      result <- .trigger_on_success(data)
      result
    },
    expr = {
      session$flushReact()
      testthat::expect_null(session$returned()())
    }
  )
})

testthat::test_that(".trigger_on_success updates when data changes", {
  shiny::testServer(
    app = function(input, output, session) {
      data <- reactiveVal(teal_data(iris = iris))
      result <- .trigger_on_success(data)
      
      observe({
        data(teal_data(mtcars = mtcars))
      })
      
      result
    },
    expr = {
      first_data <- session$returned()()
      testthat::expect_true("iris" %in% names(first_data))
      
      session$flushReact()
      second_data <- session$returned()()
      testthat::expect_true("mtcars" %in% names(second_data))
    }
  )
})

testthat::test_that(".trigger_on_success does not update when data is identical", {
  shiny::testServer(
    app = function(input, output, session) {
      td <- teal_data(iris = iris)
      data <- reactiveVal(td)
      result <- .trigger_on_success(data)
      
      counter <- reactiveVal(0)
      
      observe({
        result()
        isolate(counter(counter() + 1))
      })
      
      observe({
        # Set the same data again
        data(td)
      })
      
      list(result = result, counter = counter)
    },
    expr = {
      session$flushReact()
      # Counter should only increment once, not multiple times
      testthat::expect_equal(session$returned()$counter(), 1)
    }
  )
})

# ui_check_class_teal_data ----

testthat::test_that("ui_check_class_teal_data returns uiOutput", {
  result <- ui_check_class_teal_data("test_id")
  testthat::expect_s3_class(result, "shiny.tag")
  testthat::expect_true(grepl("test_id-message", as.character(result)))
})

# ui_check_module_datanames ----

testthat::test_that("ui_check_module_datanames returns uiOutput", {
  result <- ui_check_module_datanames("test_id")
  testthat::expect_s3_class(result, "shiny.tag")
  testthat::expect_true(grepl("test_id-message", as.character(result)))
})

# ui_validate_error ----

testthat::test_that("ui_validate_error returns uiOutput", {
  result <- ui_validate_error("test_id")
  testthat::expect_s3_class(result, "shiny.tag")
  testthat::expect_true(grepl("test_id-message", as.character(result)))
})
