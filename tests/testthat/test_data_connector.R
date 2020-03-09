context("Data Connector")
library(random.cdisc.data)


test_that("data connection", {
  open_fun <- CallableFunction$new(data.frame)
  open_fun$set_args(list(x = 1:5))

  close_fun <- CallableFunction$new(data.frame)
  close_fun$set_args(list(x = 1:2))

  con <- DataConnection$new()
  con$set_open_fun(open_fun)
  con$set_close_fun(close_fun)
  con$set_open_args(args = list(y = letters[1:5]))

  expect_identical(
    as.list(con$get_open_call(deparse = FALSE)),
    list(as.name("data.frame"), x = 1:5, y = letters[1:5])
  )

  expect_identical(
    con$get_open_call(),
    "data.frame(x = 1:5, y = c(\"a\", \"b\", \"c\", \"d\", \"e\"))"
  )


  expect_false(con$.__enclos_env__$private$opened)
  con$open()
  expect_true(con$.__enclos_env__$private$opened)

  # passing arguments to open doesn't overwrite args
  con$open(args = list(x = 1:5, y = LETTERS[1:5]))
  expect_identical(
    as.list(con$get_open_call(deparse = FALSE)),
    list(as.name("data.frame"), x = 1:5, y = letters[1:5])
  )


  expect_identical(
    con$get_open_call(),
    "data.frame(x = 1:5, y = c(\"a\", \"b\", \"c\", \"d\", \"e\"))"
  )



  expect_null(
    con$close(silent = TRUE)
  )

})

test_that("DataConnector with DataConnection", {
  open_fun <- CallableFunction$new(data.frame)
  open_fun$set_args(list(x = 1:5))

  close_fun <- CallableFunction$new(data.frame)
  close_fun$set_args(list(x = 1:2))

  con <- DataConnection$new()
  con$set_open_fun(open_fun)
  con$set_close_fun(close_fun)
  con$set_open_args(args = list(y = letters[1:5]))
  con$open()


  code <- "ADSL$x <- 1"
  check <- TRUE
  connectors <- list(
    rcd_dataset("ADSL", radsl, cached = TRUE),
    rcd_dataset("ADLB", radlb, cached = TRUE)
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
    "data.frame(x = 1:5, y = c(\"a\", \"b\", \"c\", \"d\", \"e\"))", # from connection open
    "ADSL <- radsl(cached = TRUE)",
    "ADLB <- radlb(cached = TRUE)",
    "data.frame(x = 1:2)", # from connection$close()
    "",
    "",
    "ADSL$x <- 1"
  )
  expect_identical(cdisc_code[1:7], expected_code)
  expect_equal(
    x$get_cdisc_data()$ADSL$data,
    dplyr::mutate(connectors[[1]]$get_data(), x = 1)
  )

  expect_identical(
    x$get_cdisc_data()$ADLB$data,
    connectors[[2]]$get_data()
  )
})


test_that("data connector with rcd connection", {

  con <- rcd_connection()
  code <- "ADSL$x <- 1"
  check <- TRUE
  connectors <- list(
    rcd_dataset("ADSL", radsl, cached = TRUE),
    rcd_dataset("ADLB", radlb, cached = TRUE)
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
    "ADSL <- radsl(cached = TRUE)",
    "ADLB <- radlb(cached = TRUE)",
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
  c1 <- rcd_dataset("ADSL", radsl, cached = TRUE)
  c2 <- rcd_dataset("ADLB", radlb, cached = TRUE)
  x <- rcd_cdisc_data(c1, c2, code = "ADSL <- mutate(x = 1)", check = TRUE)

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
