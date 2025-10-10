# Tests for public functions in R/teal_modifiers.R
# Testing: modify_title, modify_header, modify_footer, add_landing_modal

testthat::describe("modify_title", {
  testthat::it("returns a teal_app object", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- modify_title(app, title = "Test Title")

    testthat::expect_s3_class(modified_app, "teal_app")
  })

  testthat::it("accepts character title", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_title(app, title = "Custom Title")
    )
  })

  testthat::it("accepts shiny.tag title", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_title(app, title = tags$span("Custom Title"))
    )
  })

  testthat::it("accepts shiny.tag.list title", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_title(app, title = tagList(tags$span("Title")))
    )
  })

  testthat::it("accepts html class title", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_title(app, title = HTML("<b>HTML Title</b>"))
    )
  })

  testthat::it("uses default favicon when NULL", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_title(app, title = "Test", favicon = NULL)
    )
  })

  testthat::it("accepts custom favicon path", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_title(app, title = "Test", favicon = "path/to/favicon.png")
    )
  })

  testthat::it("throws error when x is not teal_app", {
    testthat::expect_error(
      modify_title(list(), title = "Test"),
      "Assertion on 'x' failed"
    )
  })

  testthat::it("throws error when title is invalid type", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_error(
      modify_title(app, title = 123),
      "Assertion on 'title' failed"
    )
  })

  testthat::it("throws error when favicon is not string", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_error(
      modify_title(app, title = "Test", favicon = 123),
      "Assertion on 'favicon' failed"
    )
  })

  testthat::it("modified UI function can be called", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- modify_title(app, title = "Test Title")

    testthat::expect_no_error({
      ui_result <- modified_app$ui(request = NULL)
    })
  })
})

testthat::describe("modify_header", {
  testthat::it("returns a teal_app object", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- modify_header(app, element = tags$p("Test Header"))

    testthat::expect_s3_class(modified_app, "teal_app")
  })

  testthat::it("accepts shiny.tag element", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_header(app, element = tags$div("Header"))
    )
  })

  testthat::it("accepts shiny.tag.list element", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_header(app, element = tagList(tags$h3("Header")))
    )
  })

  testthat::it("accepts character element", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_header(app, element = "Simple Header")
    )
  })

  testthat::it("accepts html class element", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_header(app, element = HTML("<div>HTML Header</div>"))
    )
  })

  testthat::it("uses default element when not specified", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_header(app)
    )
  })

  testthat::it("throws error when x is not teal_app", {
    testthat::expect_error(
      modify_header(list(), element = tags$p("Header")),
      "Assertion on 'x' failed"
    )
  })

  testthat::it("throws error when element is invalid type", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_error(
      modify_header(app, element = 123),
      "Assertion on 'element' failed"
    )
  })

  testthat::it("modified UI function can be called", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- modify_header(app, element = tags$div("Header"))

    testthat::expect_no_error({
      ui_result <- modified_app$ui(request = NULL)
    })
  })
})

testthat::describe("modify_footer", {
  testthat::it("returns a teal_app object", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- modify_footer(app, element = tags$p("Test Footer"))

    testthat::expect_s3_class(modified_app, "teal_app")
  })

  testthat::it("accepts shiny.tag element", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_footer(app, element = tags$div("Footer"))
    )
  })

  testthat::it("accepts shiny.tag.list element", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_footer(app, element = tagList(tags$p("Footer")))
    )
  })

  testthat::it("accepts character element", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_footer(app, element = "Simple Footer")
    )
  })

  testthat::it("accepts html class element", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_footer(app, element = HTML("<div>HTML Footer</div>"))
    )
  })

  testthat::it("uses default element when not specified", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      modify_footer(app)
    )
  })

  testthat::it("throws error when x is not teal_app", {
    testthat::expect_error(
      modify_footer(list(), element = tags$p("Footer")),
      "Assertion on 'x' failed"
    )
  })

  testthat::it("throws error when element is invalid type", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_error(
      modify_footer(app, element = 123),
      "Assertion on 'element' failed"
    )
  })

  testthat::it("modified UI function can be called", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- modify_footer(app, element = tags$div("Footer"))

    testthat::expect_no_error({
      ui_result <- modified_app$ui(request = NULL)
    })
  })
})

testthat::describe("add_landing_modal", {
  testthat::it("returns a teal_app object", {
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

  testthat::it("accepts character title", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      add_landing_modal(app, title = "Welcome")
    )
  })

  testthat::it("accepts NULL title", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      add_landing_modal(app, title = NULL)
    )
  })

  testthat::it("accepts character content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      add_landing_modal(app, content = "Modal content")
    )
  })

  testthat::it("accepts shiny.tag content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      add_landing_modal(app, content = tags$p("Modal content"))
    )
  })

  testthat::it("accepts shiny.tag.list content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      add_landing_modal(app, content = tagList(tags$p("Content")))
    )
  })

  testthat::it("accepts html content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      add_landing_modal(app, content = HTML("<p>HTML content</p>"))
    )
  })

  testthat::it("accepts NULL content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      add_landing_modal(app, content = NULL)
    )
  })

  testthat::it("accepts custom footer", {
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

  testthat::it("uses default footer when not specified", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    testthat::expect_no_error(
      add_landing_modal(app, title = "Test")
    )
  })

  testthat::it("throws error when x is not teal_app", {
    testthat::expect_error(
      add_landing_modal(list(), title = "Test"),
      "Assertion on 'x' failed"
    )
  })

  testthat::it("server function executes with character content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- add_landing_modal(
      app,
      title = "Test Title",
      content = "Test Content",
      footer = modalButton("Close")
    )

    testthat::expect_no_error(
      shiny::testServer(
        app = modified_app$server,
        expr = {
          session$flushReact()
        }
      )
    )
  })

  testthat::it("server function executes with tag content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- add_landing_modal(
      app,
      title = "Test",
      content = tags$p("Tag content"),
      footer = tags$div(modalButton("OK"))
    )

    testthat::expect_no_error(
      shiny::testServer(
        app = modified_app$server,
        expr = {
          session$flushReact()
        }
      )
    )
  })

  testthat::it("server function executes with NULL title and content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- add_landing_modal(
      app,
      title = NULL,
      content = NULL
    )

    testthat::expect_no_error(
      shiny::testServer(
        app = modified_app$server,
        expr = {
          session$flushReact()
        }
      )
    )
  })

  testthat::it("server function executes with tagList content", {
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )

    modified_app <- add_landing_modal(
      app,
      title = "Test",
      content = tagList(tags$p("First"), tags$p("Second"))
    )

    testthat::expect_no_error(
      shiny::testServer(
        app = modified_app$server,
        expr = {
          session$flushReact()
        }
      )
    )
  })
})

testthat::describe("function chaining", {
  testthat::it("modify_title, modify_header, and modify_footer can be chained", {
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

  testthat::it("add_landing_modal can be chained with other modifiers", {
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

  testthat::it("all modifiers can be chained together", {
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
})
