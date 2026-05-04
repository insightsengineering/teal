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

testthat::describe("e2e: validate_input validates", {
  all_inputs_mod <- module(
    label = "all inputs",
    ui = function(id) {
      ns <- NS(id)
      tagList(
        selectInput(ns("select"), "Select Input:", choices = c("A", "B", "C")),
        selectizeInput(ns("selectize"), "Selectize Input:", choices = c("X", "Y", "Z")),
        radioButtons(ns("radio"), "Radio Buttons:", choices = c("Option 1", "Option 2")),
        checkboxGroupInput(ns("checkbox_group"), "Checkbox Group:", choices = c("Check 1", "Check 2")),
        checkboxInput(ns("checkbox"), "Checkbox Input"),
        dateInput(ns("date"), "Date Input:"),
        dateRangeInput(ns("date_range"), "Date Range Input:")
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          validation <- list(
            try(
              teal:::validate_input("select", function(x) identical(x, "B"), "select must be B"),
              silent = TRUE
            ),
            try(
              teal:::validate_input("selectize", function(x) identical(x, "Y"), "selectize must be Y", ),
              silent = TRUE
            ),
            try(
              teal:::validate_input("radio", function(x) identical(x, "Option 2"), "radio must be Option 2"),
              silent = TRUE
            ),
            try(
              teal:::validate_input("checkbox_group", function(x) identical(x, c("Check 2")), "checkbox_group must be Check 2"),
              silent = TRUE
            ),
            try(
              teal:::validate_input("checkbox", function(x) identical(x, TRUE), "checkbox must be TRUE"),
              silent = TRUE
            ),
            try(
              teal:::validate_input("date", function(x) identical(x, as.Date("2024-01-01")), "date must be 2024-01-01"),
              silent = TRUE
            ),
            try(
              teal:::validate_input(
                "date_range",
                function(x) identical(x, c(as.Date("2024-01-01"), as.Date("2024-01-31"))),
                "date_range must be 2024-01-01 to 2024-01-31"
              ),
              silent = TRUE
            )
          )
          data()
        })
      })
    }
  )

  app_driver <- TealAppDriver$new(init(data = simple_teal_data(), modules = all_inputs_mod))
  withr::defer(app_driver$stop())

  it("selectInput", {
    message <- "select must be B"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("select"), "B")
    testthat::expect_failure(
      testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    )
  })

  it("selectizeInput", {
    message <- "selectize must be Y"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("selectize"), "Y")
    testthat::expect_failure(
      testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    )
  })

  it("radioButtons", {
    message <- "radio must be Option 2"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("radio"), "Option 2")
    testthat::expect_failure(
      testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    )
  })

  it("checkboxGroupInput", {
    message <- "checkbox_group must be Check 2"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("checkbox_group"), "Check 2")
    testthat::expect_failure(
      testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    )
  })

  it("checkboxInput", {
    message <- "checkbox must be TRUE"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("checkbox"), TRUE)
    testthat::expect_failure(
      testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    )
  })

  it("dateInput", {
    message <- "date must be 2024-01-01"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("date"), "2024-01-01")
    testthat::expect_failure(
      testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    )
  })

  it("dateRangeInput", {
    message <- "date_range must be 2024-01-01 to 2024-01-31"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("date_range"), c("2024-01-01", "2024-01-31"))
    testthat::expect_failure(
      testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    )
  })

  it("no input errors at the end", {
    testthat::expect_null(app_driver$get_text(".shiny-output-error"))
  })
})
