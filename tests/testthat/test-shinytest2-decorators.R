testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: module with decorator UI and output is modified interactively upon changes in decorator", {
  skip_if_too_deep(5)

  interactive_decorator <- teal_transform_module(
    ui = function(id) {
      ns <- NS(id)
      div(
        textInput(ns("append_text"), "Append text", value = "random text")
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          req(data())
          within(data(),
            {
              object <- paste0(object, append_text)
            },
            append_text = input$append_text
          )
        })
      })
    }
  )

  app <- TealAppDriver$new(
    data = teal.data::teal_data(x = "Text Input"),
    modules = example_module(label = "Example Module", decorators = list(interactive_decorator))
  )

  app$navigate_teal_tab("Example Module")

  input_id <- Reduce(
    shiny::NS,
    c("decorate", "transform_1", "transform", "append_text")
  )

  testthat::expect_true(
    app$is_visible(sprintf("#%s-%s", app$active_module_ns(), input_id))
  )

  testthat::expect_identical(
    app$active_module_element_text(sprintf("%s-label", input_id)),
    "Append text"
  )

  testthat::expect_identical(
    app$get_active_module_input(input_id),
    "random text"
  )

  testthat::expect_identical(
    app$get_active_module_output("text"),
    paste0('[1] \"', "Text Input", "random text", '\"'),
    "[1] \"Text Inputrandom text\""
  )

  app$set_active_module_input(input_id, "new text")

  testthat::expect_identical(
    app$get_active_module_output("text"),
    paste0('[1] \"', "Text Input", "new text", '\"'),
    "[1] \"Text Inputrandom text\""
  )

  app$stop()
})

testthat::test_that("e2e: module with decorator, where server fails,  shows shiny error message", {
  skip_if_too_deep(5)
  failing_decorator <- teal_transform_module(
    ui = function(id) {
      ns <- NS(id)
      div(
        textInput(ns("append_text"), "Append text", value = "random text")
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive(stop("This is error"))
      })
    }
  )
  app <- TealAppDriver$new(
    data = teal.data::teal_data(iris = iris),
    modules = example_module(label = "Example Module", decorators = list(failing_decorator))
  )

  app$navigate_teal_tab("Example Module")

  input_id <- Reduce(
    shiny::NS,
    c("decorate", "transform_1", "silent_error", "message")
  )

  testthat::expect_true(app$is_visible(sprintf("#%s-%s", app$active_module_ns(), input_id)))

  app$expect_validation_error()

  testthat::expect_identical(
    strsplit(app$active_module_element_text(input_id), "\n")[[1]],
    c(
      "Shiny error when executing the `data` module.",
      "This is error",
      "Check your inputs or contact app developer if error persists."
    )
  )

  testthat::expect_setequal(
    app$get_active_module_output("text")$type,
    c("shiny.silent.error", "validation")
  )

  app$stop()
})
