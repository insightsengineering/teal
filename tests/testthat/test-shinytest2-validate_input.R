testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: validate_input displays validation message when input is invalid, no message when valid", {
  skip_if_too_deep(5)

  validation_module <- function(label = "validation test") {
    module(
      label = label,
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          output$plot <- renderPlot({
            teal:::validate_input(
              inputId = "number",
              condition = function(x) !is.null(x) && as.numeric(x) > 5,
              message = "Please select a number greater than 5",
              session = session
            )
            plot(1:10)
          })
        })
      },
      ui = function(id) {
        ns <- NS(id)
        tagList(
          numericInput(ns("number"), "Enter a number:", value = 3, min = 1, max = 10),
          plotOutput(ns("plot"))
        )
      }
    )
  }

  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = validation_module(label = "Validation Test")
    )
  )

  # Check that validation error is displayed in the output
  error_text <- app$get_text(".shiny-output-error-validation")
  testthat::expect_match(error_text, "Please select a number greater than 5")


  # Update input to valid value
  app$set_input(app$namespaces()$module("number"), 7)
  app$wait_for_idle()

  # Check that validation error is gone
  testthat::expect_null(app$get_text(".shiny-output-error-validation"))

  app$stop()
})

testthat::test_that("e2e: validate_input validates many inputs (and linked output) when they return invalid value", {
  skip_if_too_deep(5)

  multi_validation_module <- function(label = "multi validation") {
    module(
      label = label,
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          output$result <- renderText({
            teal:::validate_input(
              inputId = c("input1", "input2"),
              condition = function(x, y) identical(x, y),
              message = "x and y must be identical",
              session = session
            )
            paste("Valid inputs:", input$input1, input$input2)
          })
        })
      },
      ui = function(id) {
        ns <- NS(id)
        tagList(
          numericInput(ns("input1"), "Enter y:", value = 4, min = 0, max = 100),
          numericInput(ns("input2"), "Enter y:", value = 5, min = 0, max = 100),
          textOutput(ns("result"))
        )
      }
    )
  }

  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = multi_validation_module(label = "Multi Validation")
    )
  )

  # Initially input2 is invalid (value 5 < 10)
  error_text <- app$get_text(".shiny-output-error-validation")
  testthat::expect_match(error_text, "x and y must be identical")
  testthat::expect_match(
    app$get_text(app$namespaces(TRUE)$module("result")),
    "x and y must be identical"
  )

  # Update input2 to valid value
  app$set_input(app$namespaces()$module("input1"), 5)
  app$set_input(app$namespaces()$module("input2"), 5)
  app$wait_for_idle()

  # Check that validation passes and result is shown
  error_text <- app$get_text(".shiny-output-error-validation")
  testthat::expect_null(error_text)

  testthat::expect_match(
    app$get_text(app$namespaces(TRUE)$module("result")),
    "Valid inputs: 5 5"
  )

  app$stop()
})

testthat::test_that("e2e: validate_input validates sliderInput")
testthat::test_that("e2e: validate_input validates selectInput")
testthat::test_that("e2e: validate_input validates selectizeInput")
testthat::test_that("e2e: validate_input validates radioButtons")
testthat::test_that("e2e: validate_input validates checkboxGroupInput")
testthat::test_that("e2e: validate_input validates checkboxInput")
testthat::test_that("e2e: validate_input validates dateInput")
testthat::test_that("e2e: validate_input validates dateRangeInput")
