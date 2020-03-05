context("Data Connector")
library(random.cdisc.data)

test_that("data connector", {

  con <- rcd_connection()
  code <- "ADSL$x <- 1"
  check <- TRUE
  connectors <- list(
    rcd_dataset("ADSL", radsl),
    rcd_dataset("ADLB", radlb)
  )

  x <- DataConnector$new()
  expect_true(is(x, "DataConnector"))

  expect_silent(x$set_connection(con))
  expect_silent(x$set_connectors(connectors))
  expect_silent(x$set_code(code))
  expect_silent(
    x$set_ui(
      function(id) {
        ns <- NS(id)
        tagList(
          numericInput(ns("seed"), "Choose seed", min = 1, max = 1000, value = 1),
          actionButton(ns("submit"), "Submit")
        )
      }
    )
  )

  expect_silent(
    x$set_server_helper(
      submit_id = "submit",
      check = check,
      fun_args_fixed = list(seed = quote(input$seed))
    )
  )

  expect_true(is(x, c("DataConnector", "R6")))
  expect_true(is(x$get_server(), "function"))
  expect_true(is(x$get_ui(id = ""), c("shiny.tag.list", "list")))
  expect_true(is(x$get_cdisc_data(), "cdisc_data"))

  cdisc_code <- strsplit(attr(x$get_cdisc_data(), "code"), "\n")[[1]]
  expected_code <- c(
    "library(package = \"random.cdisc.data\")",
    "ADSL <- radsl(cached = TRUE, seed = 1)",
    "ADLB <- radlb(cached = TRUE, seed = 1)",
    "",
    "",
    "ADSL$x <- 1"
  )
  expect_identical(cdisc_code[1:6], expected_code)
  expect_equal(
    x$get_cdisc_data()$ADSL$data,
    dplyr::mutate(connectors[[1]]$get_data(), x = 1)
  )

  expect_identical(
    x$get_cdisc_data()$ADLB$data,
    connectors[[2]]$get_data()
  )

})


test_that("rcd data", {
  c1 <- rcd_dataset("ADSL", radsl)
  c2 <- rcd_dataset("ADLB", radlb)
  x <- rcd_data(c1, c2, code = "ADSL <- mutate(x = 1)", check = TRUE)

  expect_true(is(x, c("DataConnector", "R6")))
  expect_true(is(x$get_server(), "function"))
  expect_true(is(x$get_ui(id = ""), c("shiny.tag.list", "list")))
})

test_that("rice data", {
  c1 <- rice_dataset(dataname = "ADSL", path = "/path/to/ADSL")
  c2 <- rice_dataset(dataname = "ADLB", path = "/path/to/ADLB")
  x <- rice_cdisc_data(c1, c2, code = "ADSL <- mutate(x = 1)")

  expect_true(is(x, c("DataConnector", "R6")))
  expect_true(is(x$get_server(), "function"))
  expect_true(is(x$get_ui(id = ""), c("shiny.tag.list", "list")))
})
