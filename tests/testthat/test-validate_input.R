testthat::test_that("validate_input(inputId) has to be a character(1)", {
  testthat::expect_error(
    validate_input(inputId = character(0)),
    "Assertion on 'inputId' failed"
  )
  testthat::expect_error(
    validate_input(inputId = 123),
    "Assertion on 'inputId' failed"
  )
  testthat::expect_error(
    validate_input(inputId = NULL),
    "Assertion on 'inputId' failed"
  )
})

testthat::test_that("validate_input(condition) has to be a logical(1) or function with nargs = length(inputId)", {
  testthat::expect_error(
    validate_input(inputId = "test", condition = "not_logical"),
    "Assertion failed"
  )
  testthat::expect_error(
    validate_input(inputId = "test", condition = c(TRUE, FALSE)),
    "Assertion failed"
  )

  testthat::expect_error(
    validate_input(inputId = "test", condition = function(x, y) TRUE),
    "Assertion failed"
  )

  testthat::expect_error(
    validate_input(inputId = c("test", "test2"), condition = function(x) TRUE),
    "Assertion failed"
  )
})

testthat::test_that("validate_input(message) has to be a character(1)", {
  testthat::expect_error(
    validate_input(inputId = "test", message = 123),
    "Assertion on 'message' failed"
  )
  testthat::expect_error(
    validate_input(inputId = "test", message = c("a", "b")),
    "Assertion on 'message' failed"
  )
})

testthat::test_that("validate_input returns NULL when condition is TRUE", {
  test_module <- function(id) {
    moduleServer(id, function(input, output, session) {
      result <- reactive({
        validate_input(
          inputId = "test_input",
          condition = TRUE,
          message = "Test message",
          session = session
        )
      })
    })
  }

  shiny::testServer(
    app = test_module,
    args = list(id = "test"),
    expr = testthat::expect_null(result())
  )
})

testthat::test_that("validate_input throws `message` as shiny.silent.error when `condition = FALSE`", {
  test_module <- function(id) {
    moduleServer(id, function(input, output, session) {
      result <- reactive({
        validate_input(
          inputId = "test_input",
          condition = FALSE,
          message = "Test message",
          session = session
        )
      })
    })
  }

  shiny::testServer(
    app = test_module,
    args = list(id = "test"),
    expr = testthat::expect_error(result(), "Test message")
  )
})

testthat::test_that("validate_input works with function condition that returns `TRUE` for input with `inputId`", {
  test_module <- function(id) {
    moduleServer(id, function(input, output, session) {
      result <- reactive({
        validate_input(
          inputId = "test_input",
          condition = function(x) identical(x, "valid_value"),
          message = "Input is invalid",
          session = session
        )
      })
    })
  }

  shiny::testServer(
    app = test_module,
    expr = {
      # Set up input value
      session$setInputs(test_input = "valid_value")
      testthat::expect_null(result())
    },
    args = list(id = "test")
  )
})

testthat::test_that("validate_input works with function condition that returns `FALSE` for input with `inputId`", {
  test_module <- function(id) {
    moduleServer(id, function(input, output, session) {
      result <- reactive({
        validate_input(
          inputId = "test_input",
          condition = function(x) identical(x, "valid_value"),
          message = "Input is invalid",
          session = session
        )
      })
    })
  }

  shiny::testServer(
    app = test_module,
    expr = {
      # Set up input value
      session$setInputs(test_input = "invalid_value")
      testthat::expect_error(result(), "Input is invalid")
    },
    args = list(id = "test")
  )
})

testthat::test_that("validate_input with multiple inputIds throws shiny.silent.error when any input is invalid", {
  test_module <- function(id) {
    moduleServer(id, function(input, output, session) {
      result <- reactive({
        validate_input(
          inputId = c("test_input1", "test_input2"),
          condition = function(x, y) identical(x, y),
          message = "Are not identical",
          session = session
        )
      })
    })
  }

  shiny::testServer(
    app = test_module,
    expr = {
      # Set up input value
      session$setInputs(test_input1 = 99, test_input2 = 98)
      testthat::expect_error(result(), "Are not identical")
    },
    args = list(id = "test")
  )
})

testthat::test_that("validate_input with multiple inputIds doesn't throw shiny.silent.error if all inputs are valid", {
  test_module <- function(id) {
    moduleServer(id, function(input, output, session) {
      result <- reactive({
        validate_input(
          inputId = c("test_input1", "test_input2"),
          condition = function(x, y) identical(x, y),
          message = "Input is invalid",
          session = session
        )
      })
    })
  }

  shiny::testServer(
    app = test_module,
    expr = {
      # Set up input value
      session$setInputs(test_input1 = 99, test_input2 = 99)
      testthat::expect_no_error(result())
    },
    args = list(id = "test")
  )
})

testthat::describe("validate_input with add argument", {
  it("1 validation", {
    test_module <- function(id) {
      moduleServer(id, function(input, output, session) {
        reactive({
          validation_collection <- make_validation_collection()
          validate_input(
            inputId = "test_input",
            condition = function(x) is.null(x),
            message = "input must be null",
            session = session,
            add = validation_collection
          )
          validation_collection$validate()
        })
      })
    }

    shiny::testServer(
      app = test_module,
      args = list(id = "test"),
      expr = {
        # Set up input value
        testthat::expect_no_error(session$returned())
        session$setInputs(test_input = "invalid_value")
        testthat::expect_error(session$returned(), "input must be null")
      }
    )
  })

  it("2 validations", {
    test_module <- function(id) {
      moduleServer(id, function(input, output, session) {
        reactive({
          validation_collection <- make_validation_collection()
          validate_input(
            inputId = "test_input1",
            condition = function(x) is.null(x),
            message = "input1 must be null",
            session = session,
            add = validation_collection
          )
          validate_input(
            inputId = "test_input2",
            condition = function(x) is.null(x),
            message = "input2 must be null",
            session = session,
            add = validation_collection
          )
          validation_collection$validate()
        })
      })
    }

    shiny::testServer(
      app = test_module,
      args = list(id = "test"),
      expr = {
        # Set up input value
        testthat::expect_no_error(session$returned())
        session$setInputs(test_input1 = "invalid_value", test_input2 = "invalid_value")
        testthat::expect_error(session$returned(), "input1 must be null\ninput2 must be null")
        session$setInputs(test_input1 = NULL, test_input2 = "invalid_value")
        testthat::expect_error(session$returned(), "^input2 must be null$")
      }
    )
  })
})


testthat::describe("validate_input shinytest2", {
  my_module <- module(
    label = "My Module",
    datanames = NULL,
    ui = function(id) {
      ns <- NS(id)
      tagList(
        checkboxGroupInput(ns("letters1"), "Select letters:", choices = head(LETTERS), inline = TRUE),
        checkboxGroupInput(ns("letters2"), "Select letters:", choices = head(LETTERS), inline = TRUE),
        tags$h3("Sample plot"),
        plotOutput(ns("plot"))
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        output$plot <- renderPlot({
          validate(
            need_input(
              "letters1",
              condition = length(input$letters1) > 0,
              message = "Select at least one letter."
            ),
            need_input(
              c("letters1", "letters2"),
              condition = function(x, y) all(!x %in% y),
              message = "Letters in the first group should not be in the second group."
            )
          )
          tab <- rbind(
            Group1 = as.integer(head(LETTERS) %in% input$letters1),
            Group2 = as.integer(head(LETTERS) %in% input$letters2)
          )
          colnames(tab) <- head(LETTERS)
          barplot(
            tab,
            beside = TRUE, legend.text = TRUE, main = "Selected letters per group",
            col = c("steelblue", "tomato")
          )
        })
      })
    }
  )

  app_driver <- TealAppDriver$new(
    app = init(
      data = within(teal_data(), iris <- iris),
      modules = my_module
    )
  )
  withr::defer(app_driver$stop())

  it("Shows 1 error messages in initialization", {
    testthat::expect_equal(
      app_driver$get_text(
        "#teal-teal_modules-nav-my_module-module-letters1 ~ .shiny-output-error.shiny-input-validation-error"
      ),
      "Select at least one letter."
    )
    testthat::expect_length(app_driver$get_text(".shiny-output-error.shiny-input-validation-error"), 1)
  })

  it("Shows error in output", {
    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-nav-my_module-module-plot"),
      "Select at least one letter."
    )
  })

  it("after clicking on first letter no errors appear", {
    app_driver$set_inputs("teal-teal_modules-nav-my_module-module-letters1" = "A")
    testthat::expect_null(app_driver$get_text(".shiny-output-error.shiny-input-validation-error"))
  })

  it("after clicking on the same letter in the second group validation error appears", {
    app_driver$set_inputs("teal-teal_modules-nav-my_module-module-letters2" = "A")
    testthat::expect_equal(
      app_driver$get_text(".shiny-output-error.shiny-input-validation-error"),
      rep("Letters in the first group should not be in the second group.", 2)
    )
  })
})

# testthat::test_that("validate_input shinytest2 - stale messages are not shown", {
#   my_module <- module(
#     label = "My Module",
#     datanames = NULL,
#     ui = function(id) {
#       ns <- NS(id)
#       tagList(
#         textInput(ns("letters1"), "Select letters:", value = "A"),
#         textInput(ns("letters2"), "Select letters:", value = ""),
#         uiOutput(ns("validation_message"))
#       )
#     },
#     server = function(id, data) {
#       moduleServer(id, function(input, output, session) {
#         output$validation_message <- renderUI({
#           validate(
#             need_input(
#               "letters1",
#               condition = nchar(input$letters1) > 0,
#               message = "Select at least one letter for first input."
#             )
#           )
#           validate(
#             need_input(
#               c("letters2"),
#               condition = nchar(input$letters2) > 0,
#               message = "Select at least one letter for second input."
#             )
#           )
#           tags$div("All inputs are valid.")
#         })
#       })
#     }
#   )

#   app_driver <- TealAppDriver$new(
#     app = init(
#       data = within(teal_data(), iris <- iris),
#       modules = my_module
#     )
#   )
#   withr::defer(app_driver$stop())


# })