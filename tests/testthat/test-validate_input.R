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
