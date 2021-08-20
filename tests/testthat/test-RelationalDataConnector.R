library(scda)

testthat::test_that("RelationalDataConnector with DataConnection", {
  open_fun <- callable_function(data.frame)
  open_fun$set_args(list(x = 1:5))

  close_fun <- callable_function(data.frame)
  set_args(x = close_fun, list(x = 1:2))

  con <- DataConnection$new(open_fun = open_fun, close_fun = close_fun)
  con$set_open_args(args = list(y = letters[1:5]))
  con$open()

  code <- "ADSL$x <- 1"
  check <- TRUE

  adsl_cf <- callable_function(function() synthetic_cdisc_data("rcd_2021_05_05")$adsl)
  adlb_cf <- callable_function(function() synthetic_cdisc_data("rcd_2021_05_05")$adlb)

  rcd1 <- cdisc_dataset_connector(dataname = "ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
  rcd2 <- cdisc_dataset_connector(dataname = "ADLB", adlb_cf, keys = get_cdisc_keys("ADLB"))

  x <- RelationalDataConnector$new(connection = con, connectors = list(rcd1, rcd2))
  testthat::expect_true(is(x, "RelationalDataConnector"))

  x$set_ui(function(id, ...) {
    ns <- NS(id)
    tagList(
      numericInput(ns("seed"), "Choose seed", min = 1, max = 1000, value = 1),
      sliderInput(ns("N"), "Choose number of observations", min = 1, max = 400, value = 10)
    )
  })
  x$set_server(function(input, output, session, connectors, connection) {
    lapply(connectors, function(connector) {
      if (get_dataname(connector) == "ADSL") {
        set_args(connector, args = list(seed = input$seed, N = input$N))
      } else {
        set_args(connector, args = list(seed = input$seed))
      }
      connector$pull(try = TRUE)
    })
  })

  testthat::expect_true(is(x, c("RelationalDataConnector", "R6")))

  testthat::expect_true(is(x$get_server(), "function"))
  testthat::expect_true(is(x$get_ui(id = ""), c("shiny.tag")))
})

# RelationalDataConnector with custom UI and server ----
testthat::test_that("RelationalDataConnector with custom UI and server", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, N = 1)
  adlb <- rcd_cdisc_dataset_connector("ADLB", radlb, ADSL = adsl)
  con <- teal:::rcd_connection()
  x <- teal:::RelationalDataConnector$new(connection = con, connectors = list(adsl, adlb))

  items <- x$get_items()
  testthat::expect_true(inherits(x, "RelationalDataConnector"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))

  testthat::expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl(N = 1)")
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
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)"
  )
  testthat::expect_equal(
    get_code(x, "ADLB"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADLB <- radlb(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(x),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADLB <- radlb(ADSL = ADSL)"
  )
})
