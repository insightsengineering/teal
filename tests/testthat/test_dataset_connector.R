context("Dataset Connector")
library(testthat)
library(random.cdisc.data)

# Test RawDatasetConnector ---
test_that("RawDatasetConnector", {
  fun <- callable_function(data.frame)
  fun$set_args(list(n = 5, seed = 1, cached = TRUE))

  x <- raw_dataset_connector(pull_fun = fun)

  expect_identical(
    x$get_code(deparse = TRUE),
    "data.frame(n = 5, seed = 1, cached = TRUE)"
  )

  expect_identical(
    x$get_code(deparse = FALSE),
    as.call(parse(text = "data.frame(n = 5, seed = 1, cached = TRUE)"))[[1]]
  )

  expect_true(
    is.call(x$get_code(deparse = FALSE))
  )

  expect_error(
    x$dataset,
    "dataset has not been pulled yet"
  )

  expect_error(
    x$get_dataset(),
    "dataset has not been pulled yet"
  )

  expect_error(
    x$get_raw_data(),
    "dataset has not been pulled yet"
  )


  expect_silent(x$pull())

  expect_true(
    is(x$dataset, "RawDataset")
  )

  expect_true(
    is(x$pull_fun, "CallableFunction")
  )

  expect_identical(
    x$dataset$get_raw_data(),
    data.frame(n = 5, seed = 1, cached = TRUE)
  )

  expect_identical(
    x$get_raw_data(),
    data.frame(n = 5, seed = 1, cached = TRUE)
  )

  # arguments used in pull doesn't change code - only data
  expect_silent(
    x$pull(args = list(n = 50, seed = 1, cached = TRUE))
  )

  expect_identical(
    x$dataset$raw_data,
    data.frame(n = 50, seed = 1, cached = TRUE)
  )

  expect_identical(
    x$get_code(deparse = TRUE),
    "data.frame(n = 5, seed = 1, cached = TRUE)"
  )


  # arguments used in pull doesn't change code - only data
  expect_silent(x$pull(args = list(n = 100)))

  expect_identical(
    x$dataset$raw_data,
    data.frame(n = 100, seed = 1, cached = TRUE)
  )

  expect_identical(
    x$get_code(deparse = TRUE),
    "data.frame(seed = 1, cached = TRUE, n = 5)"
  )


  expect_error(
    raw_dataset_connector(paste),
    "is not TRUE"
  )


  fun2 <- callable_function(paste)
  x2 <- raw_dataset_connector(pull_fun = fun2)

  expect_error(
    x2$pull(),
    "is.data.frame"
  )

  expect_error(
    expect_identical(
      x2$dataset,
      NULL
    ),
    "dataset has not been pulled yet"
  )


  expect_identical(
    x2$get_code(deparse = FALSE),
    as.call(parse(text = "paste()"))[[1]]
  )

})

# Test RelationalDatasetConnector ------
test_that("RelationalDatasetConnector", {
  fun <- callable_function(data.frame)
  fun$set_args(list(n = 5, seed = 1, cached = TRUE))

  expect_error(
    relational_dataset_connector(pull_fun = fun),
    "dataname"
  )

  expect_error(
    relational_dataset_connector(
      pull_fun = fun,
      dataname = "ADSL"
    ),
    "keys"
  )

  expect_silent(
    x1 <- relational_dataset_connector(
      pull_fun = fun,
      dataname = "ADSL",
      keys = get_cdisc_keys("ADSL")
    )
  )


  expect_true(
    is(x1$pull_fun, "CallableFunction")
  )

  expect_identical(
    x1$get_code(deparse = TRUE),
    "ADSL <- data.frame(n = 5, seed = 1, cached = TRUE)"
  )


  expect_identical(
    x1$get_code(deparse = FALSE),
    as.list(as.call(parse(text = "ADSL <- data.frame(n = 5, seed = 1, cached = TRUE)")))
  )

  expect_error(
    x1$dataset,
    "dataset has not been pulled yet"
  )

  expect_error(
    x1$get_dataset(),
    "dataset has not been pulled yet"
  )

  expect_error(
    x1$get_raw_data(),
    "dataset has not been pulled yet"
  )


  expect_silent(x1$pull())

  expect_true(
    is(x1$dataset, "RelationalDataset")
  )

  expect_identical(
    x1$dataset$get_keys(),
    x1$get_keys()
  )


  expect_identical(
    x1$dataset$get_raw_data(),
    data.frame(n = 5, seed = 1, cached = TRUE)
  )

  expect_identical(
    x1$get_raw_data(),
    data.frame(n = 5, seed = 1, cached = TRUE)
  )


  expect_silent(
    x2 <- relational_dataset_connector(
      pull_fun = fun,
      dataname = "ADSL",
      keys = get_cdisc_keys("ADSL")
    )
  )

  expect_identical(
    x2$get_keys(),
    get_cdisc_keys("ADSL")
  )

  expect_silent(x2$pull())
  expect_identical(
    x2$get_keys(),
    x2$dataset$get_keys()
  )
})

# Test conversions
test_that("conversions", {
  fun <- callable_function(data.frame)
  fun$set_args(list(n = 5, seed = 1, cached = TRUE))

  x <- raw_dataset_connector(pull_fun = fun)

  expect_silent(
    x1 <- as_relational(
      x,
      dataname = "ADSL",
      keys = get_cdisc_keys("ADSL")
    )
  )

  expect_true(
    is(x1, "RelationalDatasetConnector")
  )

  expect_error(
    x1$dataset,
    "dataset has not been pulled yet"
  )

  expect_silent(load_dataset(x1))

  expect_true(
    is(x1, "RelationalDatasetConnector")
  )

  expect_silent(load_dataset(x))
  expect_identical(x1$dataset$get_raw_data(),
                   x$dataset$get_raw_data())


  expect_warning(
    x2 <- as_relational(
      x,
      dataname = "ADSL",
      keys = get_cdisc_keys("ADSL")
    ),
    "Avoid pulling before conversion"
  )

  expect_true(
    is(x2, "RelationalDatasetConnector")
  )
})

test_that("as_relational", {
  fun <- callable_function(data.frame)
  fun$set_args(list(n = 5, seed = 1, cached = TRUE))

  x <- raw_dataset_connector(pull_fun = fun)

  expect_silent(
    x1 <- as_relational(
      x,
      dataname = "ADSL",
      code = "ADSL$test_col <- seq_len(nrow(ADSL))",
      keys = get_cdisc_keys("ADSL")
    )
  )


  expect_identical(
    x1$get_code(),
    "ADSL <- data.frame(n = 5, seed = 1, cached = TRUE)\nADSL$test_col <- seq_len(nrow(ADSL))"
  )

  expect_error(
    x1$dataset$get_code(),
    "dataset has not been pulled yet"
  )

  expect_silent(
    load_dataset(x1)
  )


  expect_identical(
    as.call(parse(text = x1$get_code())),
    as.call(parse(text = x1$dataset$get_code()))
  )


  expect_identical(
    colnames(x1$dataset$get_raw_data()),
    c("n", "seed", "cached", "test_col")
  )
})

# Test DatasetConnector -----
test_that("get call - pass function", {
  dc <- DatasetConnector$new()
  expect_true(is(dc, c("DatasetConnector", "R6")))

  dc$set_dataname("ADSL")
  expect_identical(
    dc$get_dataname(),
    "ADSL"
  )

  x_fun <- callable_function(read.table)
  x_fun$set_args(list(file = "./data_connectors/table.csv", nrows = 1L, header = TRUE))
  dc$set_pull_fun(x_fun)
  expect_equal(
    dc$get_call(),
    "ADSL <- read.table(file = \"./data_connectors/table.csv\", nrows = 1L, header = TRUE)"
  )
  expect_identical(
    dc$get_dataset(),
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

  x_fun <- callable_function(readLines)
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


test_that("rcd_dataset_connector", {
  x <- rcd_dataset_connector(dataname = "ADSL", radsl, cached = TRUE, N = 400)
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
    x$get_dataset(),
    radsl(cached = TRUE, seed = 1, N = 400)
  )
})

test_that("rds_dataset_connector", {
  x <- rds_dataset_connector(dataname = "ADSL", file = "./data_connectors/table.RDS")
  expect_true(is(x, c("DatasetConnector", "R6")))

  expect_equal(
    x$get_call(),
    "ADSL <- readRDS(file = \"./data_connectors/table.RDS\")"
  )

})

test_that("rice_dataset", {
  x <- rice_cdisc_data(
    ADSL = rice_dataset_connector("ADSL", "/path/to/ADSL"),
    ADLB = rice_dataset_connector("ADLB", "/path/to/ADLB"),
    code = "ADSL$x <- 1"
  )

  expect_equal(
    x$.__enclos_env__$private$code,
    "ADSL$x <- 1"
  )

})
