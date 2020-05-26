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
    x$get_code(deparse = TRUE),
    get_code(x, deparse = TRUE)
  )

  expect_identical(
    x$get_code(deparse = FALSE),
    as.call(parse(text = "data.frame(n = 5, seed = 1, cached = TRUE)"))[[1]]
  )

  expect_identical(
    x$get_code(deparse = FALSE),
    get_code(x, deparse = FALSE)
  )

  expect_true(
    is.call(x$get_code(deparse = FALSE))
  )

  expect_error(
    x$get_dataset(),
    "dataset has not been pulled yet"
  )
  expect_error(
    get_dataset(x),
    "dataset has not been pulled yet"
  )

  expect_error(
    x$get_raw_data(),
    "dataset has not been pulled yet"
  )

  expect_error(
    get_raw_data(x),
    "dataset has not been pulled yet"
  )

  expect_silent(x$pull_dataset())

  expect_true(
    is(get_dataset(x), "RawDataset")
  )

  expect_true(
    is(x$get_pull_fun(), "CallableFunction")
  )

  expect_identical(
    get_raw_data(get_dataset(x)),
    data.frame(n = 5, seed = 1, cached = TRUE)
  )

  expect_identical(
    get_raw_data(x),
    data.frame(n = 5, seed = 1, cached = TRUE)
  )

  expect_identical(
    x$get_raw_data(),
    data.frame(n = 5, seed = 1, cached = TRUE)
  )

  # arguments used in pull doesn't change code - only data
  expect_silent(
    x$pull_dataset(args = list(n = 50, seed = 1, cached = TRUE))
  )

  expect_identical(
    get_raw_data(x),
    data.frame(n = 50, seed = 1, cached = TRUE)
  )

  expect_identical(
    x$get_code(deparse = TRUE),
    "data.frame(n = 5, seed = 1, cached = TRUE)"
  )


  # arguments used in pull_dataset doesn't change code - only data
  expect_silent(x$pull_dataset(args = list(n = 100)))

  expect_identical(
    get_raw_data(x),
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
    x2$pull_dataset(),
    "is.data.frame"
  )

  expect_error(
    expect_identical(
      get_dataset(x2),
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


  expect_identical(
    x1$get_code(deparse = TRUE),
    "ADSL <- data.frame(n = 5, seed = 1, cached = TRUE)"
  )


  expect_identical(
    x1$get_code(deparse = FALSE),
    as.list(as.call(parse(text = "ADSL <- data.frame(n = 5, seed = 1, cached = TRUE)")))
  )

  expect_error(
    x1$get_dataset(),
    "dataset has not been pulled yet"
  )

  expect_error(
    get_dataset(x1),
    "dataset has not been pulled yet"
  )

  expect_error(
    x1$get_raw_data(),
    "dataset has not been pulled yet"
  )


  expect_silent(x1$pull_dataset())

  expect_true(
    is(x1$get_dataset(), "RelationalDataset")
  )

  expect_identical(
    get_dataset(x1)$get_keys(),
    x1$get_keys()
  )


  expect_identical(
    get_raw_data(x1),
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

  expect_silent(x2$pull_dataset())
  expect_identical(
    x2$get_keys(),
    get_dataset(x2)$get_keys()
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
    x1$get_dataset(),
    "dataset has not been pulled yet"
  )

  expect_silent(load_dataset(x1))

  expect_true(
    is(x1, "RelationalDatasetConnector")
  )

  expect_silent(load_dataset(x))
  expect_identical(get_raw_data(x1),
                   x$get_dataset()$get_raw_data())


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
    get_dataset(x1),
    "dataset has not been pulled yet"
  )

  expect_silent(
    load_dataset(x1)
  )


  expect_identical(
    as.call(parse(text = x1$get_code())),
    as.call(parse(text = get_dataset(x1)$get_code()))
  )


  expect_identical(
    colnames(get_raw_data(x1)),
    c("n", "seed", "cached", "test_col")
  )
})

test_that("rcd_dataset_connector", {
  x <- rcd_cdisc_dataset_connector(dataname = "ADSL", radsl, cached = TRUE, N = 400)
  x2 <- rcd_dataset_connector(dataname = "ADSL", radsl, cached = TRUE, N = 400,
                              keys = get_cdisc_keys("ADSL"))
  expect_equal(x, x2)
  expect_true(is(x, c("DatasetConnector", "R6")))

  expect_identical(
    x$.__enclos_env__$private$pull_fun$.__enclos_env__$private$.fun_name,
    "radsl"
  )

  expect_identical(
    x$get_dataname(),
    "ADSL"
  )

  expect_equal(
    x$get_code(),
    "ADSL <- radsl(cached = TRUE, N = 400)"
  )

  expect_silent(
    load_dataset(x)
  )

  expect_identical(
    x$get_raw_data(),
    radsl(cached = TRUE, seed = 1, N = 400)
  )
})

test_that("rds_dataset_connector", {
  x <- rds_cdisc_dataset_connector(dataname = "ADSL", file = "./data_connectors/table.rds")
  x2 <- rds_dataset_connector(dataname = "ADSL", file = "./data_connectors/table.rds",
                             keys = get_cdisc_keys("ADSL"))
  expect_equal(x, x2)
  expect_true(is(x, c("DatasetConnector", "R6")))

  expect_equal(
    x$get_code(),
    "ADSL <- readRDS(file = \"./data_connectors/table.rds\")"
  )

})

test_that("rice_dataset", {
  x <- rice_cdisc_data(
    ADSL = rice_dataset_connector("ADSL", "/path/to/ADSL", keys = get_cdisc_keys("ADSL")),
    ADLB = rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB"),
    code = "ADSL$x <- 1"
  )

  expect_equal(
    x$get_connectors()[[1]],
    rice_dataset_connector("ADSL", "/path/to/ADSL", keys = get_cdisc_keys("ADSL"))
  )
  expect_equal(
    x$get_connectors()[[2]],
    rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
  )

  expect_identical(
    x$get_connectors()[[1]]$get_code(),
    "ADSL <- rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE, quiet = TRUE)"
  )


  expect_equal(
    x$.__enclos_env__$private$code,
    "ADSL$x <- 1"
  )
})
