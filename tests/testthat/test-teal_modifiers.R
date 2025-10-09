# Tests for public functions in R/teal_modifiers.R
# Testing: modify_title, modify_header, modify_footer, add_landing_modal

# modify_title ----

testthat::test_that("modify_title returns a teal_app object", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  modified_app <- modify_title(app, title = "Test Title")
  
  testthat::expect_s3_class(modified_app, "teal_app")
})

testthat::test_that("modify_title accepts character title", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_title(app, title = "Custom Title")
  )
})

testthat::test_that("modify_title accepts shiny.tag title", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_title(app, title = tags$span("Custom Title"))
  )
})

testthat::test_that("modify_title accepts shiny.tag.list title", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_title(app, title = tagList(tags$span("Title")))
  )
})

testthat::test_that("modify_title uses default favicon when NULL", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_title(app, title = "Test", favicon = NULL)
  )
})

testthat::test_that("modify_title accepts custom favicon path", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_title(app, title = "Test", favicon = "path/to/favicon.png")
  )
})

testthat::test_that("modify_title throws error when x is not teal_app", {
  testthat::expect_error(
    modify_title(list(), title = "Test"),
    "Assertion on 'x' failed"
  )
})

testthat::test_that("modify_title throws error when title is invalid type", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_error(
    modify_title(app, title = 123),
    "Assertion on 'title' failed"
  )
})

testthat::test_that("modify_title throws error when favicon is not string", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_error(
    modify_title(app, title = "Test", favicon = 123),
    "Assertion on 'favicon' failed"
  )
})

# modify_header ----

testthat::test_that("modify_header returns a teal_app object", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  modified_app <- modify_header(app, element = tags$p("Test Header"))
  
  testthat::expect_s3_class(modified_app, "teal_app")
})

testthat::test_that("modify_header accepts shiny.tag element", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_header(app, element = tags$div("Header"))
  )
})

testthat::test_that("modify_header accepts shiny.tag.list element", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_header(app, element = tagList(tags$h3("Header")))
  )
})

testthat::test_that("modify_header accepts character element", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_header(app, element = "Simple Header")
  )
})

testthat::test_that("modify_header uses default element when not specified", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_header(app)
  )
})

testthat::test_that("modify_header throws error when x is not teal_app", {
  testthat::expect_error(
    modify_header(list(), element = tags$p("Header")),
    "Assertion on 'x' failed"
  )
})

testthat::test_that("modify_header throws error when element is invalid type", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_error(
    modify_header(app, element = 123),
    "Assertion on 'element' failed"
  )
})

# modify_footer ----

testthat::test_that("modify_footer returns a teal_app object", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  modified_app <- modify_footer(app, element = tags$p("Test Footer"))
  
  testthat::expect_s3_class(modified_app, "teal_app")
})

testthat::test_that("modify_footer accepts shiny.tag element", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_footer(app, element = tags$div("Footer"))
  )
})

testthat::test_that("modify_footer accepts shiny.tag.list element", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_footer(app, element = tagList(tags$p("Footer")))
  )
})

testthat::test_that("modify_footer accepts character element", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_footer(app, element = "Simple Footer")
  )
})

testthat::test_that("modify_footer uses default element when not specified", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    modify_footer(app)
  )
})

testthat::test_that("modify_footer throws error when x is not teal_app", {
  testthat::expect_error(
    modify_footer(list(), element = tags$p("Footer")),
    "Assertion on 'x' failed"
  )
})

testthat::test_that("modify_footer throws error when element is invalid type", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_error(
    modify_footer(app, element = 123),
    "Assertion on 'element' failed"
  )
})

# add_landing_modal ----

testthat::test_that("add_landing_modal returns a teal_app object", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  modified_app <- add_landing_modal(
    app,
    title = "Welcome",
    content = "Test content"
  )
  
  testthat::expect_s3_class(modified_app, "teal_app")
})

testthat::test_that("add_landing_modal accepts character title", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    add_landing_modal(app, title = "Welcome")
  )
})

testthat::test_that("add_landing_modal accepts NULL title", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    add_landing_modal(app, title = NULL)
  )
})

testthat::test_that("add_landing_modal accepts character content", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    add_landing_modal(app, content = "Modal content")
  )
})

testthat::test_that("add_landing_modal accepts shiny.tag content", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    add_landing_modal(app, content = tags$p("Modal content"))
  )
})

testthat::test_that("add_landing_modal accepts shiny.tag.list content", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    add_landing_modal(app, content = tagList(tags$p("Content")))
  )
})

testthat::test_that("add_landing_modal accepts NULL content", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    add_landing_modal(app, content = NULL)
  )
})

testthat::test_that("add_landing_modal accepts custom footer", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    add_landing_modal(
      app,
      footer = tagList(modalButton("Close"), actionButton("accept", "Accept"))
    )
  )
})

testthat::test_that("add_landing_modal uses default footer when not specified", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    add_landing_modal(app, title = "Test")
  )
})

testthat::test_that("add_landing_modal throws error when x is not teal_app", {
  testthat::expect_error(
    add_landing_modal(list(), title = "Test"),
    "Assertion on 'x' failed"
  )
})

# Chaining modifiers ----

testthat::test_that("modify_title, modify_header, and modify_footer can be chained", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    app |>
      modify_title(title = "Chained Title") |>
      modify_header(element = tags$p("Chained Header")) |>
      modify_footer(element = tags$p("Chained Footer"))
  )
})

testthat::test_that("add_landing_modal can be chained with other modifiers", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  testthat::expect_no_error(
    app |>
      modify_title(title = "Test") |>
      add_landing_modal(title = "Welcome", content = "Content")
  )
})

testthat::test_that("all modifiers can be chained together", {
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  )
  
  modified_app <- app |>
    modify_title(title = "Complete App") |>
    modify_header(element = tags$div("App Header")) |>
    modify_footer(element = tags$div("App Footer")) |>
    add_landing_modal(title = "Welcome", content = "Please read the instructions")
  
  testthat::expect_s3_class(modified_app, "teal_app")
})
