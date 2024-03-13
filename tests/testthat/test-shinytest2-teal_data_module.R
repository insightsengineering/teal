testthat::test_that("e2e: teal_data_module will have a delayed load of datasets", {
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
          datanames(data) <- c("dataset1", "dataset2")

          data
        })
      })
    }
  )

  app <- TealAppDriver$new(
    data = tdm, modules = example_module(label = "Example Module")
  )

  app$click("teal_data_module-submit")
  app$wait_for_idle(timeout = default_idle_timeout)
  testthat::expect_setequal(app$get_active_filter_vars(), c("dataset1", "dataset2"))

  app$stop()
})

testthat::test_that("e2e: teal_data_module fails to proceed without input", {
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
          datanames(data) <- c("dataset1")
          data
        })
      })
    }
  )

  app <- TealAppDriver$new(
    data = tdm, modules = example_module(label = "Example Module")
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  app$click("teal_data_module-submit")

  app$expect_validation_error()

  app$stop()
})

testthat::test_that("e2e: teal_data_module adds new column to datasets", {
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
            dataset1 <- dplyr::mutate(
              iris,
              !!new_column := sprintf("%s new", .data$Species)
            ),
            new_column = input$new_column
          )
          datanames(data) <- c("dataset1")

          data
        })
      })
    }
  )

  app <- TealAppDriver$new(
    data = tdm, modules = example_module(label = "Example Module")
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  app$set_input("teal_data_module-new_column", "A_New_Column")
  app$click("teal_data_module-submit")

  # This may fail if teal_data_module does not perform the transformation
  testthat::expect_no_error(app$add_filter_var("dataset1", "A_New_Column"))

  app$wait_for_idle(timeout = default_idle_timeout)
  testthat::expect_setequal(
    app$get_active_filter_selection("dataset1", "A_New_Column"),
    unique(sprintf("%s new", iris$Species))
  )

  app$stop()
})
