library(scda)

test_that("RelationalDataConnector with DataConnection", {
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
  expect_true(is(x, "RelationalDataConnector"))

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

  expect_true(is(x, c("RelationalDataConnector", "R6")))

  expect_true(is(x$get_server(), "function"))
  expect_true(is(x$get_ui(id = ""), c("shiny.tag")))
})
