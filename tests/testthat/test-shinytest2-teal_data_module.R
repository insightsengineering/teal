testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: teal_data_module auto resolves when `once=TRUE` and data is passed", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      numericInput(NS(id, "iris_rows"), "iris rows", min = 0, max = 150, step = 1, value = 10)
    },
    server = function(id, ...) {
      moduleServer(id, function(input, output, session) {
        reactive(
          teal_data(iris = head(iris, input$iris_rows), mtcars = mtcars)
        )
      })
    },
    once = TRUE
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  testthat::expect_true(app$get_js("document.querySelector('.teal-data-module-popup') === null"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module only resolves when `once=TRUE` and data is passed", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      tagList(
        numericInput(NS(id, "iris_rows"), "iris rows", min = 0, max = 150, step = 1, value = 10),
        actionButton(NS(id, "submit"), "Submit")
      )
    },
    server = function(id, ...) {
      moduleServer(id, function(input, output, session) {
        eventReactive(input$submit, {
          teal_data(iris = head(iris, input$iris_rows), mtcars = mtcars)
        })
      })
    },
    once = TRUE
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  testthat::expect_false(app$get_js("document.querySelector('.teal-data-module-popup') === null"))
  app$click("teal-data-teal_data_module-submit")
  testthat::expect_true(app$get_js("document.querySelector('.teal-data-module-popup') === null"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module modal is still open when `once=FALSE` and data is passed", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      numericInput(NS(id, "iris_rows"), "iris rows", min = 0, max = 150, step = 1, value = 10)
    },
    server = function(id, ...) {
      moduleServer(id, function(input, output, session) {
        reactive(
          teal_data(iris = head(iris, input$iris_rows), mtcars = mtcars)
        )
      })
    },
    once = FALSE
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  testthat::expect_false(app$get_js("document.querySelector('.teal-data-module-popup') === null"))
  app$click(selector = "#teal-close_teal_data_module_modal button")
  testthat::expect_true(app$get_js("document.querySelector('.teal-data-module-popup') === null"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module modal close is disabled `once=FALSE` and data is passed", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      tagList(
        numericInput(NS(id, "iris_rows"), "iris rows", min = 0, max = 150, step = 1, value = 10),
        actionButton(NS(id, "submit"), "Submit")
      )
    },
    server = function(id, ...) {
      moduleServer(id, function(input, output, session) {
        eventReactive(input$submit, {
          teal_data(iris = head(iris, input$iris_rows), mtcars = mtcars)
        })
      })
    },
    once = FALSE
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  testthat::expect_false(
    app$get_js("document.querySelector('#teal-close_teal_data_module_modal button.disabled') === null")
  )
  app$click("teal-data-teal_data_module-submit")
  app$click(selector = "#teal-close_teal_data_module_modal button")
  testthat::expect_true(
    app$get_js("document.querySelector('#teal-close_teal_data_module_modal button.disabled') === null")
  )
  testthat::expect_true(app$get_js("document.querySelector('.teal-data-module-popup') === null"))
  app$stop()
})

testthat::test_that("e2e: teal_data_module will have a delayed load of datasets", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::actionButton(ns("submit"), label = "Load data")
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, {
          data <- within(
            teal_data(),
            {
              dataset1 <- iris
              dataset2 <- mtcars
            }
          )

          data
        })
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
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::textInput(ns("new_column"), label = "New column name"),
        shiny::actionButton(ns("submit"), label = "Load data")
      )
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, {
          shiny::validate(
            shiny::need(input$new_column, "Please provide a new column name")
          )
          data <- within(teal_data(), dataset1 <- iris)
          data
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
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::textInput(ns("new_column"), label = "New column name"),
        shiny::actionButton(ns("submit"), label = "Load data")
      )
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, {
          shiny::validate(
            shiny::need(input$new_column, "Please provide a new column name")
          )
          data <- within(
            teal_data(),
            {
              dataset1 <- iris
              dataset1[[new_column]] <- sprintf("%s new", dataset1$Species)
            },
            new_column = input$new_column
          )

          data
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

testthat::test_that("e2e: teal_data_module gets removed after successful data load, when once = TRUE", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::actionButton(ns("submit"), label = "Load data")
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, {
          data <- within(
            teal_data(),
            {
              dataset1 <- iris
              dataset2 <- mtcars
            }
          )

          data
        })
      })
    },
    once = TRUE
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  submit <- "teal-data-teal_data_module-submit"
  app$click(submit)

  testthat::expect_null(
    app$get_html('#teal-teal_modules-active_tab a[data-value="teal_data_module"]')
  )

  testthat::expect_null(
    app$is_visible(sprintf("#%s", submit))
  )

  app$stop()
})

testthat::test_that("e2e: teal_data_module is still visible after successful data load, when once = FALSE", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::actionButton(ns("submit"), label = "Load data")
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, {
          data <- within(
            teal_data(),
            {
              dataset1 <- iris
              dataset2 <- mtcars
            }
          )

          data
        })
      })
    },
    once = FALSE
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = example_module(label = "Example Module")
  )

  app$click("teal-data-teal_data_module-submit")

  testthat::expect_true(
    app$is_visible('#teal-teal_modules-active_tab a[data-value="teal_data_module"]')
  )

  app$stop()
})

testthat::test_that("e2e: teal_data_module will make other tabs inactive before successful data load", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  tdm <- teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::actionButton(ns("submit"), label = "Load data")
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::eventReactive(input$submit, {
          data <- within(
            teal_data(),
            {
              dataset1 <- iris
              dataset2 <- mtcars
            }
          )

          data
        })
      })
    },
    once = FALSE
  )

  app <- TealAppDriver$new(
    data = tdm,
    modules = modules(
      example_module(label = "Example Module 1"),
      example_module(label = "Example Module 2")
    )
  )

  testthat::expect_equal(
    rvest::html_attr(
      rvest::html_nodes(
        app$get_html_rvest("#teal-teal_modules-active_tab"),
        "a[data-value*='example_module']"
      ),
      "disabled"
    ),
    c("disabled", "disabled")
  )

  app$click("teal-data-teal_data_module-submit")

  testthat::expect_true(
    is.na(
      unique(
        rvest::html_attr(
          rvest::html_nodes(
            app$get_html_rvest("#teal-teal_modules-active_tab"),
            "a[data-value*='example_module']"
          ),
          "disabled"
        )
      )
    )
  )

  app$stop()
})
