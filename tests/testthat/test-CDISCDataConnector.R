adsl_cf <- CallableFunction$new(function() as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL")))))
adae_cf <- CallableFunction$new(function() as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE")))))
adsl  <- CDISCDatasetConnector$new("ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"), parent = character(0))
adae <- CDISCDatasetConnector$new("ADAE", adae_cf, keys = get_cdisc_keys("ADAE"), parent = "ADSL")

testthat::test_that("get_code returns the correct code for two CDISCDatasetConnector objects", {
  data <- CDISCDataConnector$new(
    connection = DataConnection$new(open_fun = CallableFunction$new(function() "open function")),
    connectors = list(adsl, adae)
  )

  items <- data$get_items()
  testthat::expect_true(inherits(data, "RelationalDataConnector"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))

  testthat::expect_equal(
    items$ADSL$get_code(),
    "ADSL <- (function() as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\")))))()"
  )
  testthat::expect_equal(
    items$ADAE$get_code(),
    "ADAE <- (function() as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADAE\")))))()"
  )
  data$pull()

  testthat::expect_equal(
    get_code(data, "ADSL"),
    paste(
      "(function() \"open function\")()",
      "ADSL <- (function() as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\")))))()",
      sep = "\n"
    )
  )
  testthat::expect_equal(
    get_code(data, "ADAE"),
    paste(
      "(function() \"open function\")()",
      "ADAE <- (function() as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADAE\")))))()",
      sep = "\n"
    )
  )
  testthat::expect_equal(
    get_code(data),
    paste(
      "(function() \"open function\")()",
      "ADSL <- (function() as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\")))))()",
      "ADAE <- (function() as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADAE\")))))()",
      sep = "\n"
    )
  )
})


# RelationalDataConnector with custom UI and server ----Ś
testthat::test_that("RelationalDataConnector with custom UI and server", {
  adsl_cf <- CallableFunction$new(function(test) {
    test
    as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  })
  adsl  <- CDISCDatasetConnector$new("ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"), parent = character(0))
  con <- DataConnection$new(open_fun = CallableFunction$new(function() "open function"))
  cdisc_data_connector <- CDISCDataConnector$new(connection = con, connectors = list(adsl))

  items <- cdisc_data_connector$get_items()
  testthat::expect_true(inherits(cdisc_data_connector, "RelationalDataConnector"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))

  testthat::expect_equal(
    items$ADSL$get_pull_callable()$get_call(),
    paste(
      "(function(test) {",
      "    test",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "})()",
      sep = "\n"
    )
  )

  testthat::expect_error(
    cdisc_data_connector$get_ui("main-app"),
    "No UI set yet"
  )

  cdisc_data_connector$set_ui(function(id, ...) {
    ns <- NS(id)
    tagList(
      numericInput(ns("test"), "Choose test", min = 1, max = 100, value = 1)
    )
  })

  testthat::expect_equal(
    as.character(cdisc_data_connector$get_ui("main-app")),
    as.character(
      tags$div(
        h3("Data Connector for:", list(code("ADSL"))),
        tags$div(
          id = "main-app-data_input",
          numericInput("main-app-data_input-test", "Choose test", min = 1, max = 100, value = 1)
        )
      )
    )
  )

  cdisc_data_connector$set_server(function(id, connectors, connection) {
      raw_datasets <- lapply(connectors, function(connector) {
        set_args(connector, args(test = input$test))
        connector$pull(try = TRUE)

        get_raw_data(connector)
      })
  })
  set_server <- cdisc_data_connector$get_server()
  testthat::expect_false(is.null(set_server))

  testthat::expect_error(get_datasets(cdisc_data_connector), regexp = "Not all datasets have been pulled yet")
  cdisc_data_connector$set_pull_args(args = list(test = 7))
  cdisc_data_connector$pull()
  testthat::expect_true(is_pulled(cdisc_data_connector))

  datasets <- get_datasets(cdisc_data_connector)
  testthat::expect_true(all(vapply(datasets, inherits, logical(1), "Dataset")))

  testthat::expect_equal(
    get_code(cdisc_data_connector, "ADSL"),
    paste(
      "(function() \"open function\")()",
      "ADSL <- (function(test) {",
      "    test",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "})(test = 7)",
      sep = "\n"
    )
  )
  testthat::expect_equal(
    get_code(cdisc_data_connector),
    paste(
      "(function() \"open function\")()",
      "ADSL <- (function(test) {",
      "    test",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "})(test = 7)",
      sep = "\n"
    )
  )
})

testthat::test_that("cdisc_data_connector returns a CDISCDataConnector object on basic input", {
  data <- cdisc_data_connector(
    connection = DataConnection$new(open_fun = CallableFunction$new(function() "open function")),
    connectors = list(adsl, adae)
  )
  testthat::expect_true(is(data, c("CDISCDataConnector", "RelationalDataConnector", "DataAbstract", "R6")))
})

testthat::test_that("cdisc_data_connector validates the 'connection' and 'connectors' arguments", {
  testthat::expect_error(cdisc_data_connector(
    connection = 1,
    connectors = list(adsl, adae)))
  testthat::expect_error(cdisc_data_connector(
    connection =  DataConnection$new(open_fun = CallableFunction$new(function() "open function")),
    connectors = "a"))
})
