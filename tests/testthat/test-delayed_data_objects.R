context("delayed_data_objects")

library(random.cdisc.data)

# Single rcd dataset connector ----

test_that("Single rcd dataset connector", {
  # create object
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl)
  default_ui <- adsl$get_ui("main-app")
  adsl$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "ADSL seed", min = 0, value = 1),
      optionalSliderInput(inputId = ns("study_duration"),
                          label = "Duration of study in years",
                          min = 0,
                          max = 5,
                          value = 2,
                          step = 1)
    )
  }
  )
  set_ui <- adsl$get_ui("main-app")
  expect_false(isTRUE(all.equal(default_ui, set_ui)))



  # check UI
  expect_equal(
    as.character(set_ui),
    as.character(
      tags$div(
        tags$div(
          id = "main-app-inputs",
          h4("Dataset Connector for ", code("ADSL")),
          numericInput(inputId = "main-app-seed", label = "ADSL seed", min = 0, value = 1),
          optionalSliderInput(inputId = "main-app-study_duration",
                              label = "Duration of study in years",
                              min = 0,
                              max = 5,
                              value = 2,
                              step = 1)
        )
      )
    )
  )

  expect_error(get_raw_data(adsl), regexp = "'ADSL' has not been pulled yet")
  adsl$pull()

  # check reproducible code
  expect_equal(
    get_code(adsl), "ADSL <- radsl()"
  )
})

# Single rcd_data connector ----

test_that("One cached and one dependent connector wrapped in a single rcd data connector", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  data <- rcd_data(adsl, adae)

  items <- data$get_items()
  expect_true(inherits(data, "RelationalDataConnector"))
  expect_true(all(vapply(items, inherits, logical(1), "RelationalDatasetConnector")))

  expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl(cached = TRUE)")
  expect_equal(items$ADAE$get_pull_callable()$get_call(), "radae(ADSL = ADSL)")
  # pull args supplied this way does not persist to the reproducible code
  data$pull(args = list(seed = 2, na_percentage = .10))

  expect_equal(
    get_code(data, "ADSL"), "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)"
  )
  expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)\nADAE <- radae(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data), "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)\nADAE <- radae(ADSL = ADSL)"
  )
})

# Single rice_data connector ----

test_that("Single rice_data connector with two rice dataset connectors", {
  adsl <- rice_cdisc_dataset_connector(dataname = "ADSL", "/path/to/ADSL")
  adlb <- rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
  adsl_adlb <- rice_data(adsl, adlb)

  items <- adsl_adlb$get_items()
  expect_true(inherits(adsl_adlb, "RelationalDataConnector"))
  expect_true(all(vapply(items, inherits, logical(1), "RelationalDatasetConnector")))

  expect_equal(
    items$ADSL$get_pull_callable()$get_call(),
    "rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE, quiet = TRUE)"
  )
  expect_equal(
    items$ADLB$get_pull_callable()$get_call(),
    "rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE, quiet = TRUE)"
  )

  expect_equal(
    get_code(adsl_adlb, "ADSL"),
    "rice::rice_session_open(password = askpass::askpass())\nADSL <- rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE, quiet = TRUE)\nrice::rice_session_close(message = FALSE)" # nolint
  )
  expect_equal(
    get_code(adsl_adlb, "ADLB"),
    "rice::rice_session_open(password = askpass::askpass())\nADLB <- rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE, quiet = TRUE)\nrice::rice_session_close(message = FALSE)" # nolint
  )
  expect_equal(
    get_code(adsl_adlb),
    "rice::rice_session_open(password = askpass::askpass())\nADSL <- rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE, quiet = TRUE)\nADLB <- rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE, quiet = TRUE)\nrice::rice_session_close(message = FALSE)" # nolint
  )
})

# RelationalDataConnector with custom UI and server ----

test_that("RelationalDataConnector with custom UI and server", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl)
  adlb <- rcd_cdisc_dataset_connector("ADLB", radlb, ADSL = adsl)
  con <- teal:::rcd_connection()
  x <- teal:::RelationalDataConnector$new(connection = con, connectors = list(adsl, adlb))

  items <- x$get_items()
  expect_true(inherits(x, "RelationalDataConnector"))
  expect_true(all(vapply(items, inherits, logical(1), "RelationalDatasetConnector")))

  expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl()")
  expect_equal(items$ADLB$get_pull_callable()$get_call(), "radlb(ADSL = ADSL)")

  expect_error(
    x$get_ui("main-app"),
    "No UI set yet"
  )

  x$set_ui(function(id) {
    ns <- NS(id)
    tagList(
      numericInput(ns("seed"), "Choose seed", min = 1, max = 100, value = 1),
      sliderInput(ns("N"), "Choose number of observations", min = 1, max = 400, value = 10),
      uiOutput(ns("pull_validate"))
    )
  })

  expect_equal(
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

  default_server <- x$get_server()

  expect_equal(default_server, NULL)
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
  expect_false(is.null(set_server))

  expect_error(get_datasets(x), regexp = "Not all datasets have been pulled yet")
  x$pull()
  expect_true(is_pulled(x))

  datasets <- get_datasets(x)
  expect_true(all(vapply(datasets, inherits, logical(1), "RelationalDataset")))

  expect_equal(
    get_code(x, "ADSL"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()"
  )
  expect_equal(
    get_code(x, "ADLB"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADLB <- radlb(ADSL = ADSL)"
  )
  expect_equal(
    get_code(x),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADLB <- radlb(ADSL = ADSL)"
  )
})

# Multiple rcd_data connectors ----

test_that("Multiple rcd_data connectors wrapped in cdisc_data", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  advs <- rcd_cdisc_dataset_connector("ADVS", radvs, ADSL = adsl)
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl)
  adsl_adae <- rcd_data(adsl, adae)
  advs_adtte <- rcd_data(advs, adtte)
  data <- cdisc_data(adsl_adae, advs_adtte)

  items <- data$get_items()
  expect_true(inherits(data, "RelationalData"))
  expect_true(all(vapply(items, inherits, logical(1), "RelationalDatasetConnector")))
  expect_true(all(vapply(data$get_connectors(), inherits, logical(1), "RelationalDataConnector")))

  expect_equal(unname(get_dataname(data)), c("ADSL", "ADAE", "ADVS", "ADTTE"))

  expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl()")
  expect_equal(items$ADAE$get_pull_callable()$get_call(), "radae(ADSL = ADSL)")
  expect_equal(items$ADVS$get_pull_callable()$get_call(), "radvs(ADSL = ADSL)")
  expect_equal(items$ADTTE$get_pull_callable()$get_call(), "radtte(ADSL = ADSL)")

  expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()"
  )
  expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADAE <- radae(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data, "ADVS"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADVS <- radvs(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data, "ADTTE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADTTE <- radtte(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADAE <- radae(ADSL = ADSL)\nADVS <- radvs(ADSL = ADSL)\nADTTE <- radtte(ADSL = ADSL)" # nolint
  )
})

# RelationalData with single dataset and connector ----

test_that("RelationalData with single dataset and connector", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl)
  adsl_data <- rcd_data(adsl)

  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    data = radtte(cached = TRUE),
    code = "ADTTE <- radtte(cached = TRUE)"
  )

  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  adae$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "ADSL seed", min = 0, value = 2),
      optionalSliderInput(
        inputId = ns("max_n_aes"),
        label = "Maximum number of AEs per patient",
        min = 0,
        max = 10,
        value = 10,
        step = 1)
    )
  }
  )

  data <- cdisc_data(adsl_data, adtte, adae)
  items <- data$get_items()
  expect_length(items, 3)
  expect_true(inherits(data, "RelationalData"))
  expect_true(inherits(items$ADSL, "RelationalDatasetConnector"))
  expect_true(inherits(items$ADTTE, "RelationalDataset"))
  expect_true(inherits(items$ADAE, "RelationalDatasetConnector"))

  connectors <- data$get_connectors()
  expect_length(connectors, 2)
  expect_true(
    inherits(connectors[[1]], "RelationalDataConnector") &&
      inherits(connectors[[2]], "RelationalDatasetConnector")
  )

  expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl()")
  expect_equal(items$ADAE$get_pull_callable()$get_call(), "radae(ADSL = ADSL)")
  expect_identical(adtte$get_raw_data, items$ADTTE$get_raw_data)

  # simulate pull with a click of the submit button
  for (connector in data$get_connectors()) {
    connector$pull()
  }

  expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()"
  )
  expect_equal(
    get_code(data, "ADTTE"),
    "library(package = \"random.cdisc.data\")\nADTTE <- radtte(cached = TRUE)"
  )
  expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADAE <- radae(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data), "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADTTE <- radtte(cached = TRUE)\nADAE <- radae(ADSL = ADSL)" # nolint
  )
})

# RelationalData with mutliple datasets and connectors ----

test_that("RelationalData with mutliple datasets and connectors", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl)
  adsl_data <- rcd_data(adsl)

  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    data = radtte(cached = TRUE),
    code = "ADTTE <- radtte(cached = TRUE)"
  )

  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  # add custom input
  adae$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "ADSL seed", min = 0, value = 2),
      optionalSliderInput(inputId = ns("max_n_aes"),
                          label = "Maximum number of AEs per patient",
                          min = 0,
                          max = 5,
                          value = 3,
                          step = 1)
      )
    }
  )

  advs <- rcd_cdisc_dataset_connector("ADVS", radvs, ADSL = adsl)
  advs$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "ADSL seed", min = 0, value = 4),
      optionalSliderInput(inputId = ns("max_n_aes"),
                          label = "Number of weeks or cycles",
                          min = 0,
                          max = 10,
                          value = 5,
                          step = 1)
      )
    }
  )

  adlb <- rcd_cdisc_dataset_connector("ADLB", radlb, ADSL = adsl)

  advs_adlb_data <- rcd_data(advs, adlb)

  temp_file <- tempfile()
  saveRDS(radrs(cached = TRUE), file = temp_file)
  adrs <- rds_cdisc_dataset_connector(dataname = "ADRS", file = temp_file)

  adsamp <- script_dataset_connector("ADSAMP",
    keys = get_cdisc_keys("ADVS"),
    file = "delayed_data_script/asdamp_with_adsl.R",
    ADSL = adsl,
    ADVS = advs
  )

  data <- cdisc_data(adsl_data, adtte, adae, advs_adlb_data, adrs, adsamp)

  expect_true(inherits(data, "RelationalData"))
  items <- data$get_items()
  expect_true(all(vapply(items[-2], inherits, logical(1), "RelationalDatasetConnector")))
  expect_true(inherits(items$ADTTE, "RelationalDataset"))

  expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl()")
  expect_equal(items$ADAE$get_pull_callable()$get_call(), "radae(ADSL = ADSL)")
  expect_equal(items$ADVS$get_pull_callable()$get_call(), "radvs(ADSL = ADSL)")
  expect_equal(items$ADLB$get_pull_callable()$get_call(), "radlb(ADSL = ADSL)")
  expect_equal(
    items$ADSAMP$get_pull_callable()$get_call(),
    "source(file = \"delayed_data_script/asdamp_with_adsl.R\", local = TRUE)$value"
  )
  expect_identical(adtte$get_raw_data, items$ADTTE$get_raw_data)

  expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()"
  )
  expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADAE <- radae(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data, "ADVS"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADVS <- radvs(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data, "ADLB"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADLB <- radlb(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data, "ADSAMP"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl()\nADVS <- radvs(ADSL = ADSL)\nADSAMP <- source(file = \"delayed_data_script/asdamp_with_adsl.R\", local = TRUE)$value" # nolint
  )
  expect_equal(
    get_code(data, "ADTTE"),
    "library(package = \"random.cdisc.data\")\nADTTE <- radtte(cached = TRUE)"
  )

  # can the shiny app be initialized without error?
  mods <- teal:::get_dummy_modules()
  expect_message(
    init(data = data, modules = mods)
  )
})
