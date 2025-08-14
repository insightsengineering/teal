testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

example_teal_data_module <- function(needs_submit = FALSE, once = TRUE) {
  teal_data_module(
    ui = function(id) {
      tagList(
        numericInput(NS(id, "iris_rows"), "iris rows", min = 0, max = 150, step = 1, value = 10),
        if (needs_submit) actionButton(NS(id, "submit"), "Submit")
      )
    },
    server = function(id, ...) {
      moduleServer(id, function(input, output, session) {
        if (needs_submit) {
          eventReactive(input$submit, {
            teal_data(iris = head(iris, input$iris_rows), mtcars = mtcars)
          })
        } else {
          reactive(teal_data(iris = head(iris, input$iris_rows), mtcars = mtcars))
        }
      })
    },
    once = once
  )
}

testthat::test_that("e2e: teal_data_module `Load data` button is shown when once=FALSE", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = example_teal_data_module(once = FALSE),
    modules = example_module(label = "Example Module")
  )
  testthat::expect_true(app$is_visible("#teal-open_teal_data_module_ui"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module `Load data` button is not shown when once=TRUE", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = example_teal_data_module(once = TRUE),
    modules = example_module(label = "Example Module")
  )
  testthat::expect_null(app$is_visible("#teal-open_teal_data_module_ui"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module shows modal on startup when data isn't ready", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = example_teal_data_module(needs_submit = TRUE),
    modules = example_module(label = "Example Module")
  )
  testthat::expect_true(app$is_visible(".teal-data-module-popup"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module auto-closes modal when `once=TRUE` and data is ready (clicked submit)", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = example_teal_data_module(needs_submit = TRUE, once = TRUE),
    modules = example_module(label = "Example Module")
  )
  app$click("teal-data-teal_data_module-submit")
  testthat::expect_null(app$is_visible(".teal-data-module-popup"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module auto-closes modal when `once=TRUE` and data is ready", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = example_teal_data_module(needs_submit = FALSE, once = TRUE),
    modules = example_module(label = "Example Module")
  )
  testthat::expect_null(app$is_visible(".teal-data-module-popup"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module doesn't auto-close when `once=FALSE` and data is ready (clicked submit)", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = example_teal_data_module(needs_submit = TRUE, once = FALSE),
    modules = example_module(label = "Example Module")
  )
  app$click(selector = "#teal-close_teal_data_module_modal button")
  testthat::expect_true(app$is_visible(".teal-data-module-popup"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module doesn't auto-close when `once=FALSE` and data is ready (no submit)", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = example_teal_data_module(needs_submit = FALSE, once = FALSE),
    modules = example_module(label = "Example Module")
  )
  testthat::expect_true(app$is_visible(".teal-data-module-popup"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module modal close button is enabled from disabled when data is ready", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = example_teal_data_module(needs_submit = TRUE, once = FALSE),
    modules = example_module(label = "Example Module")
  )

  testthat::expect_identical(
    app$get_attr("#teal-close_teal_data_module_modal", "disabled"),
    "disabled"
  )
  app$click("teal-data-teal_data_module-submit")
  testthat::expect_true(is.na(app$get_attr("#teal-close_teal_data_module_modal", "disabled")))
  app$stop()
})

testthat::test_that("e2e: datasets from teal_data_module show in filter panel", {
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) shiny::actionButton(shiny::NS(id, "submit"), label = "Load data"),
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, within(teal_data(), {
          dataset1 <- iris
          dataset2 <- mtcars
        }))
      })
    }
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  app$click("teal-data-teal_data_module-submit")
  app$navigate_teal_tab("Example Module")
  testthat::expect_setequal(app$get_active_filter_vars(), c("dataset1", "dataset2"))

  app$stop()
})

testthat::test_that("e2e: teal_data_module shows validation errors", {
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      shiny::tagList(
        shiny::textInput(shiny::NS(id, "new_column"), label = "New column name"),
        shiny::actionButton(shiny::NS(id, "submit"), label = "Load data")
      )
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, {
          shiny::validate(
            shiny::need(input$new_column, "Please provide a new column name")
          )
          within(teal_data(), dataset1 <- iris)
        })
      })
    }
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  app$click("teal-data-teal_data_module-submit")
  app$expect_validation_error()

  app$stop()
})

testthat::test_that("e2e: teal_data_module inputs change teal_data object that is passed to teal main UI", {
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      shiny::tagList(
        shiny::textInput(shiny::NS(id, "new_column"), label = "New column name"),
        shiny::actionButton(shiny::NS(id, "submit"), label = "Load data")
      )
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, {
          shiny::validate(
            shiny::need(input$new_column, "Please provide a new column name")
          )
          within(
            teal_data(),
            {
              dataset1 <- iris
              dataset1[[new_column]] <- sprintf("%s new", dataset1$Species)
            },
            new_column = input$new_column
          )
        })
      })
    }
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  app$set_input("teal-data-teal_data_module-new_column", "A_New_Column")
  app$click("teal-data-teal_data_module-submit")
  app$navigate_teal_tab("Example Module")

  # This may fail if teal_data_module does not perform the transformation
  testthat::expect_no_error(app$add_filter_var("dataset1", "A_New_Column"))

  testthat::expect_setequal(
    app$get_active_data_filters("dataset1")$A_New_Column,
    unique(sprintf("%s new", iris$Species))
  )

  app$stop()
})
