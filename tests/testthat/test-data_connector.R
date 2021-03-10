library(random.cdisc.data)

test_that("data connection", {
  open_fun <- callable_function(data.frame)
  open_fun$set_args(list(x = 1:5))

  close_fun <- callable_function(data.frame)
  close_fun$set_args(list(x = 1:2))

  con <- DataConnection$new(open_fun = open_fun, close_fun = close_fun)
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

  rcd1 <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  rcd2 <- rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)

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
