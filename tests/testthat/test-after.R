# Tests for after.R functions

testthat::test_that("after validates x is a teal_module", {
  testthat::expect_error(
    after(list()),
    "Assertion.*failed.*Must inherit from class 'teal_module'"
  )
})

testthat::test_that("after validates ui function signature", {
  mod <- module(label = "test")
  
  testthat::expect_error(
    after(mod, ui = function(wrong_arg) NULL),
    "ui should be a function of id and elem"
  )
  
  testthat::expect_error(
    after(mod, ui = "not a function"),
    "ui should be a function of id and elem"
  )
})

testthat::test_that("after validates server function signature", {
  mod <- module(label = "test")
  
  testthat::expect_error(
    after(mod, server = function(wrong_arg) NULL),
    "server should be a function of `input` and `output`, `session`, `data`"
  )
  
  testthat::expect_error(
    after(mod, server = "not a function"),
    "server should be a function of `input` and `output`, `session`, `data`"
  )
})

testthat::test_that("after returns a teal_module", {
  mod <- module(label = "test")
  result <- after(mod)
  
  testthat::expect_s3_class(result, "teal_module")
})

testthat::test_that("after accepts default ui and server functions", {
  mod <- module(label = "test")
  
  testthat::expect_no_error(
    after(mod)
  )
})

testthat::test_that("after accepts custom ui function", {
  mod <- module(label = "test")
  custom_ui <- function(id, elem) {
    tags$div(elem, "custom")
  }
  
  result <- after(mod, ui = custom_ui)
  testthat::expect_s3_class(result, "teal_module")
})

testthat::test_that("after accepts custom server function", {
  mod <- module(label = "test")
  custom_server <- function(input, output, session, data) {
    data
  }
  
  result <- after(mod, server = custom_server)
  testthat::expect_s3_class(result, "teal_module")
})

testthat::test_that("after accepts additional arguments", {
  mod <- module(label = "test")
  
  result <- after(mod, custom_arg = 42, another_arg = "test")
  testthat::expect_s3_class(result, "teal_module")
})

testthat::test_that(".after_ui wraps original ui function", {
  original_ui <- function(id) tags$div("original")
  wrapper_ui <- function(id, elem) tags$div(elem, "wrapped")
  
  wrapped <- .after_ui(original_ui, wrapper_ui, list())
  
  testthat::expect_true(is.function(wrapped))
  testthat::expect_named(formals(wrapped), names(formals(original_ui)))
})

testthat::test_that(".after_ui handles additional arguments", {
  original_ui <- function(id) tags$div("original")
  wrapper_ui <- function(id, elem) tags$div(elem, "wrapped")
  additional_args <- list(custom_arg = 42)
  
  wrapped <- .after_ui(original_ui, wrapper_ui, additional_args)
  
  testthat::expect_true(is.function(wrapped))
})

testthat::test_that(".after_server wraps original server function", {
  original_server <- function(id) NULL
  wrapper_server <- function(input, output, session, data) data
  
  wrapped <- .after_server(original_server, wrapper_server, list())
  
  testthat::expect_true(is.function(wrapped))
  testthat::expect_named(formals(wrapped), names(formals(original_server)))
})

testthat::test_that(".after_server handles additional arguments", {
  original_server <- function(id) NULL
  wrapper_server <- function(input, output, session, data) data
  additional_args <- list(custom_arg = 42)
  
  wrapped <- .after_server(original_server, wrapper_server, additional_args)
  
  testthat::expect_true(is.function(wrapped))
})
