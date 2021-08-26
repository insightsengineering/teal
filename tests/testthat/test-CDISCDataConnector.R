library(random.cdisc.data)

# Single rcd_data connector ----
testthat::test_that("One cached and one dependent connector wrapped in a single rcd data connector", {
  test.nest::skip_if_too_deep(3)
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  data <- rcd_data(adsl, adae)

  items <- data$get_items()
  testthat::expect_true(inherits(data, "RelationalDataConnector"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))

  testthat::expect_equal(items$ADSL$get_code(), "ADSL <- radsl(cached = TRUE)")
  testthat::expect_equal(items$ADAE$get_code(), "ADSL <- radsl(cached = TRUE)\nADAE <- radae(ADSL = ADSL)")
  data$pull()

  testthat::expect_equal(
    get_code(data, "ADSL"), "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)"
  )
  testthat::expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)\nADAE <- radae(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(data), "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)\nADAE <- radae(ADSL = ADSL)"
  )
})


# RelationalDataConnector with custom UI and server ----
testthat::test_that("RelationalDataConnector with custom UI and server", {
  test.nest::skip_if_too_deep(3)
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adlb <- rcd_cdisc_dataset_connector("ADLB", radlb, ADSL = adsl)
  con <- teal:::rcd_connection()
  x <- teal:::RelationalDataConnector$new(connection = con, connectors = list(adsl, adlb))

  items <- x$get_items()
  testthat::expect_true(inherits(x, "RelationalDataConnector"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))

  testthat::expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl(cached = TRUE)")
  testthat::expect_equal(items$ADLB$get_pull_callable()$get_call(), "radlb(ADSL = ADSL)")

  testthat::expect_error(
    x$get_ui("main-app"),
    "No UI set yet"
  )

  x$set_ui(function(id, ...) {
    ns <- NS(id)
    tagList(
      numericInput(ns("seed"), "Choose seed", min = 1, max = 100, value = 1),
      sliderInput(ns("N"), "Choose number of observations", min = 1, max = 400, value = 10),
      uiOutput(ns("pull_validate"))
    )
  })

  testthat::expect_equal(
    as.character(x$get_ui("main-app")),
    as.character(
      tags$div(
        h3("Data Connector for:", list(code("ADSL"), code("ADLB"))),
        tags$div(
          id = "main-app-data_input",
          numericInput("main-app-data_input-seed", "Choose seed", min = 1, max = 100, value = 1),
          sliderInput("main-app-data_input-N", "Choose number of observations", min = 1, max = 400, value = 10),
          tags$div(id = "main-app-data_input-pull_validate", class = "shiny-html-output")
        )
      )
    )
  )

  x$set_server(function(input, output, session, connectors, connection) {
    output$pull_validate <- renderUI({
      raw_datasets <- lapply(connectors, function(connector) {
        if (get_dataname(connector) == "ADSL") {
          set_args(connector, args = list(seed = input$seed, N = input$N))
        } else {
          set_args(connector, args = list(seed = input$seed))
        }
        connector$pull(try = TRUE)

        get_raw_data(connector)
      })
      validate(need(nrow(raw_datasets[[1]]) > 100, "ADSL needs more than 100 observations"))
      NULL
    })
  })
  set_server <- x$get_server()
  testthat::expect_false(is.null(set_server))

  testthat::expect_error(get_datasets(x), regexp = "Not all datasets have been pulled yet")
  x$pull()
  testthat::expect_true(is_pulled(x))

  datasets <- get_datasets(x)
  testthat::expect_true(all(vapply(datasets, inherits, logical(1), "Dataset")))

  testthat::expect_equal(
    get_code(x, "ADSL"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)"
  )
  testthat::expect_equal(
    get_code(x, "ADLB"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)\nADLB <- radlb(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(x),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)\nADLB <- radlb(ADSL = ADSL)"
  )
})
