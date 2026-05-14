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
            validate_input(
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

  app_driver <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = validation_module(label = "Validation Test")
    )
  )
  withr::defer(app_driver$stop())

  # Check that validation error is displayed in the output
  error_text <- app_driver$get_text(".shiny-output-error-validation")
  testthat::expect_match(error_text, "Please select a number greater than 5")


  # Update input to valid value
  app_driver$set_input(app_driver$namespaces()$module("number"), 7)
  app_driver$wait_for_idle()

  # Check that validation error is gone
  testthat::expect_null(app_driver$get_text(".shiny-output-error-validation"))
})

testthat::test_that("e2e: validate_input validates many inputs (and linked output) when they return invalid value", {
  skip_if_too_deep(5)

  multi_validation_module <- function(label = "multi validation") {
    module(
      label = label,
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          output$result <- renderText({
            validate_input(
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
          numericInput(ns("input1"), "Enter x:", value = 4, min = 0, max = 100),
          numericInput(ns("input2"), "Enter y:", value = 6, min = 0, max = 100),
          textOutput(ns("result"))
        )
      }
    )
  }

  app_driver <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = multi_validation_module(label = "Multi Validation")
    )
  )
  withr::defer(app_driver$stop())

  error_text <- app_driver$get_text(".shiny-output-error-validation")
  testthat::expect_match(error_text, "x and y must be identical")
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("result")),
    "x and y must be identical"
  )

  # Update input2 to valid value
  app_driver$set_input(app_driver$namespaces()$module("input1"), 5)
  app_driver$set_input(app_driver$namespaces()$module("input2"), 5)
  app_driver$wait_for_idle()

  # Check that validation passes and result is shown
  error_text <- app_driver$get_text(".shiny-output-error-validation")
  testthat::expect_null(error_text)

  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("result")),
    "Valid inputs: 5 5"
  )
})

testthat::describe("e2e: validate_input validates generic fields", {
  skip_if_too_deep(5)
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
          validate(
            need_input("select", function(x) identical(x, "B"), "select must be B"),
            need_input("selectize", function(x) identical(x, "Y"), "selectize must be Y"),
            need_input("radio", function(x) identical(x, "Option 2"), "radio must be Option 2"),
            need_input(
              "checkbox_group",
              function(x) identical(x, c("Check 2")),
              "checkbox_group must be Check 2"
            ),
            need_input("checkbox", function(x) identical(x, TRUE), "checkbox must be TRUE"),
            need_input("date", function(x) identical(x, as.Date("2024-01-01")), "date must be 2024-01-01"),
            need_input(
              "date_range",
              function(x) identical(x, c(as.Date("2024-01-01"), as.Date("2024-01-31"))),
              "date_range must be 2024-01-01 to 2024-01-31"
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
    testthat::expect_no_match(app_driver$get_text(".shiny-output-error") %||% character(0L), message, fixed = TRUE)
  })

  it("selectizeInput", {
    message <- "selectize must be Y"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("selectize"), "Y")
    testthat::expect_no_match(app_driver$get_text(".shiny-output-error") %||% character(0L), message, fixed = TRUE)
  })

  it("radioButtons", {
    message <- "radio must be Option 2"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("radio"), "Option 2")
    testthat::expect_no_match(app_driver$get_text(".shiny-output-error") %||% character(0L), message, fixed = TRUE)
  })

  it("checkboxGroupInput", {
    message <- "checkbox_group must be Check 2"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("checkbox_group"), "Check 2")
    testthat::expect_no_match(app_driver$get_text(".shiny-output-error") %||% character(0L), message, fixed = TRUE)
  })

  it("checkboxInput", {
    message <- "checkbox must be TRUE"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("checkbox"), TRUE)
    testthat::expect_no_match(app_driver$get_text(".shiny-output-error") %||% character(0L), message, fixed = TRUE)
  })

  it("dateInput", {
    message <- "date must be 2024-01-01"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("date"), "2024-01-01")
    testthat::expect_no_match(app_driver$get_text(".shiny-output-error") %||% character(0L), message, fixed = TRUE)
  })

  it("dateRangeInput", {
    message <- "date_range must be 2024-01-01 to 2024-01-31"
    testthat::expect_match(app_driver$get_text(".shiny-output-error"), message, fixed = TRUE, all = FALSE)
    app_driver$set_input(app_driver$namespaces()$module("date_range"), c("2024-01-01", "2024-01-31"))
    testthat::expect_no_match(app_driver$get_text(".shiny-output-error") %||% character(0L), message, fixed = TRUE)
  })
})

testthat::test_that("validate_input shinytest2 - sequence of errors with parallel validation", {
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

  # Shows 1 error messages in initialization
  testthat::expect_equal(
    app_driver$get_text(
      "#teal-teal_modules-nav-my_module-module-letters1 ~ .shiny-output-error.shiny-input-validation-error"
    ),
    "Select at least one letter."
  )
  testthat::expect_length(app_driver$get_text(".shiny-output-error.shiny-input-validation-error"), 1)
  # Shows error in output
  testthat::expect_equal(
    app_driver$get_text("#teal-teal_modules-nav-my_module-module-plot"),
    "Select at least one letter."
  )
  # after clicking on first letter no errors appear
  app_driver$set_inputs("teal-teal_modules-nav-my_module-module-letters1" = "A")
  testthat::expect_null(app_driver$get_text(".shiny-output-error.shiny-input-validation-error"))

  # after clicking on the same letter in the second group validation error appears
  app_driver$set_inputs("teal-teal_modules-nav-my_module-module-letters2" = "A")
  testthat::expect_equal(
    app_driver$get_text(".shiny-output-error.shiny-input-validation-error"),
    rep("Letters in the first group should not be in the second group.", 2)
  )
})

testthat::test_that("validate_input shinytest2 - stale messages are not shown", {
  my_module <- module(
    label = "My Module",
    datanames = NULL,
    ui = function(id) {
      ns <- NS(id)
      tagList(
        textInput(ns("letters1"), "Select letters:", value = "A"),
        textInput(ns("letters2"), "Select letters:", value = ""),
        uiOutput(ns("validation_message"))
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        output$validation_message <- renderUI({
          validate(
            need_input(
              "letters1",
              condition = nchar(input$letters1) > 0,
              message = "Select at least one letter for first input."
            )
          )
          validate(
            need_input(
              c("letters2"),
              condition = nchar(input$letters2) > 0,
              message = "Select at least one letter for second input."
            )
          )
          tags$div("All inputs are valid.")
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

  app_driver$set_active_module_input("letters1", character(0))
  app_driver$set_active_module_input("letters2", "B")
  testthat::expect_no_match(
    app_driver$get_text(".shiny-input-validation-error"),
    "Select at least one letter for second input."
  )
})
