context("Dataset Connector")
library(testthat)
library(random.cdisc.data)

# Test DatasetConnector -----
test_that("get call - pass function", {
  dc <- DatasetConnector$new()
  expect_true(is(dc, c("DatasetConnector", "R6")))

  dc$set_dataname("ADSL")
  expect_identical(
    dc$get_dataname(),
    "ADSL"
  )

  x_fun <- CallableFunction$new(read.table)
  x_fun$set_args(list(file = "./data_connectors/table.csv", nrows = 1L, header = TRUE))
  dc$set_pull_fun(x_fun)
  expect_equal(
    dc$get_call(),
    "ADSL <- read.table(file = \"./data_connectors/table.csv\", nrows = 1L, header = TRUE)"
  )
  expect_identical(
    dc$get_data(),
    read.table(file = "./data_connectors/table.csv", nrows = 1L, header = TRUE)
  )


  dc$set_keys(keys = keys(primary = "Species", foreign = "Species", parent = "species"))
  expect_equal(
    dc$get_keys(),
    keys(primary = "Species", foreign = "Species", parent = "species")
  )


  dc$set_pull_arg_value(name = "nrows", value = 2L)
  expect_identical(
    dc$get_call(),
    "ADSL <- read.table(file = \"./data_connectors/table.csv\", nrows = 2L, header = TRUE)"
  )
})


test_that("get call and header - new function name", {
  dc <- DatasetConnector$new()
  expect_true(is(dc, c("DatasetConnector", "R6")))

  dc$set_dataname("ADSL")
  expect_identical(
    dc$get_dataname(),
    "ADSL"
  )

  x_fun <- CallableFunction$new(readLines)
  x_fun$set_args(list(con = "./test_dataset_connector.R", n = 1L))
  dc$set_pull_fun(x_fun)

  expect_identical(
    dc$get_call(),
    "ADSL <- readLines(con = \"./test_dataset_connector.R\", n = 1L)"
  )
})


test_that("Setting class elements in wrong order", {
  dc <- DatasetConnector$new()
  expect_true(is(dc, c("DatasetConnector", "R6")))

  expect_error(
    dc$set_pull_arg_value(name = "n", value = 2L),
    "Pull function not set"
  )

  expect_error(
    dc$set_pull_args(list(con = "./test_dataset_connector.R", n = 1L)),
    "Pull function not set"
  )

  expect_error(
    dc$set_pull_fun(readLines),
    "CallableFunction"
  )

})


test_that("rcd_dataset", {
  x <- rcd_dataset(dataname = "ADSL", radsl, cached = TRUE, N = 400)
  expect_true(is(x, c("DatasetConnector", "R6")))

  expect_identical(
    x$.__enclos_env__$private$pull_fun$.__enclos_env__$private$fun_name,
    "radsl"
  )

  expect_identical(
    x$get_dataname(),
    "ADSL"
  )

  expect_equal(
    x$get_call(),
    "ADSL <- radsl(cached = TRUE, N = 400)"
  )

  expect_identical(
    x$get_data(),
    radsl(cached = TRUE, seed = 1, N = 400)
  )
})

test_that("rds_cdisc_dataset", {
  x <- rds_cdisc_dataset(dataname = "ADSL", file = "./data_connectors/table.RDS")
  expect_true(is(x, c("DatasetConnector", "R6")))

  expect_equal(
    x$get_call(),
    "ADSL <- readRDS(file = \"./data_connectors/table.RDS\")"
  )

})

test_that("rice_dataset", {
  x <- rice_cdisc_data(
    ADSL = rice_dataset("ADSL", "/path/to/ADSL"),
    ADLB = rice_dataset("ADLB", "/path/to/ADLB"),
    code = "ADSL$x <- 1"
  )

  expect_equal(
    x$.__enclos_env__$private$code,
    "ADSL$x <- 1"
  )

})
