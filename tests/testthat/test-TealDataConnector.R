library(scda)

adsl_cf <- CallableFunction$new(function() as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL")))))
adae_cf <- CallableFunction$new(function() as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE")))))
adsl <- CDISCTealDatasetConnector$new("ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"), parent = character(0))
adae <- CDISCTealDatasetConnector$new("ADAE", adae_cf, keys = get_cdisc_keys("ADAE"), parent = "ADSL")

testthat::test_that("TealDataConnector with TealDataConnection", {
  open_fun <- callable_function(data.frame)
  open_fun$set_args(list(x = 1:5))

  close_fun <- callable_function(data.frame)
  set_args(x = close_fun, list(x = 1:2))

  con <- TealDataConnection$new(open_fun = open_fun, close_fun = close_fun)
  con$set_open_args(args = list(y = letters[1:5]))
  con$open()

  code <- "ADSL$x <- 1"
  check <- TRUE

  adsl_cf <- callable_function(function(scda_name) synthetic_cdisc_data(scda_name)$adsl)
  adlb_cf <- callable_function(function(scda_name) synthetic_cdisc_data(scda_name)$adlb)

  scda1 <- cdisc_dataset_connector(dataname = "ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
  scda2 <- cdisc_dataset_connector(dataname = "ADLB", adlb_cf, keys = get_cdisc_keys("ADLB"))

  x <- TealDataConnector$new(connection = con, connectors = list(scda1, scda2))
  testthat::expect_true(is(x, "TealDataConnector"))

  x$set_ui(function(id, ...) {
    ns <- NS(id)
    tagList(
      textInput(ns("scda_name"), label = "Example", value = "latest")
    )
  })
  x$set_server(function(id, connectors, connection) {
    lapply(connectors, function(connector) {
      set_args(connector, args = list(scda_name = input$scda_name))
      connector$pull(try = TRUE)
    })
  })

  testthat::expect_true(is(x, c("TealDataConnector", "R6")))

  testthat::expect_true(is(x$get_server(), "function"))
  testthat::expect_true(is(x$get_ui(id = ""), c("shiny.tag")))
})

testthat::test_that("TealDataConnector$print prints out expected output on basic input", {
  data <- CDISCTealDataConnector$new(
    connection = TealDataConnection$new(open_fun = CallableFunction$new(function() "open function")),
    connectors = list(adsl, adae)
  )

  out <- capture.output(print(data))
  testthat::expect_equal(
    out,
    c(
      paste0(
        "A currently not yet opened CDISCTealDataConnector object containing ",
        "2 TealDataset/TealDatasetConnector object(s) as element(s)."
      ),
      "0 of which is/are loaded/pulled:",
      "--> Element 1:",
      "A CDISCTealDatasetConnector object, named ADSL, containing a TealDataset object that has not been loaded/pulled",
      "--> Element 2:",
      "A CDISCTealDatasetConnector object, named ADAE, containing a TealDataset object that has not been loaded/pulled"
    )
  )
})

testthat::test_that("relational_data_connector returns a TealDataConnector object on basic input", {
  data <- cdisc_data_connector(
    connection = TealDataConnection$new(open_fun = CallableFunction$new(function() "open function")),
    connectors = list(adsl, adae)
  )
  testthat::expect_true(is(data, c("TealDataConnector", "TealDataAbstract", "R6")))
})

testthat::test_that("relational_data_connector has input validation", {
  testthat::expect_error(cdisc_data_connector(
    connection = 1,
    connectors = list(adsl, adae)
  ))
  testthat::expect_error(cdisc_data_connector(
    connection = TealDataConnection$new(open_fun = CallableFunction$new(function() "open function")),
    connectors = "a"
  ))
})
