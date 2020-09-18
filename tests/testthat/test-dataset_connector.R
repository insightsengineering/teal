context("Dataset Connector")
library(testthat)
library(random.cdisc.data)

# Test RawDatasetConnector ---
test_that("RawDatasetConnector", {
  fun <- callable_function(data.frame)
  fun$set_args(list(n = 5, seed = 1, cached = TRUE))

  x <- raw_dataset_connector(pull_callable = fun)

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
    as.list(as.call(parse(text = "data.frame(n = 5, seed = 1, cached = TRUE)")))
  )

  expect_identical(
    x$get_code(deparse = FALSE),
    get_code(x, deparse = FALSE)
  )

  expect_true(
    is.call(x$get_code(deparse = FALSE)[[1]])
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

  expect_silent(x$pull())

  expect_true(
    is(get_dataset(x), "RawDataset")
  )

  expect_true(
    is(x$get_pull_callable(), "CallableFunction")
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
    x$pull(args = list(n = 50, seed = 1, cached = TRUE))
  )

  expect_identical(
    get_raw_data(x),
    data.frame(n = 50, seed = 1, cached = TRUE)
  )

  expect_identical(
    x$get_code(deparse = TRUE),
    "data.frame(n = 5, seed = 1, cached = TRUE)"
  )


  # arguments used in pull doesn't change code - only data
  expect_silent(x$pull(args = list(n = 100)))

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
  x2 <- raw_dataset_connector(pull_callable = fun2)

  expect_error(
    x2$pull(),
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
    list(as.call(parse(text = "paste")))
  )
})

# Test RelationalDatasetConnector ------
test_that("RelationalDatasetConnector", {
  fun <- callable_function(radsl)
  fun$set_args(list(N = 5, seed = 1, cached = TRUE))

  expect_error(
    relational_dataset_connector(pull_callable = fun),
    "dataname"
  )

  expect_error(
    relational_dataset_connector(
      dataname = "ADSL",
      pull_callable = fun
    ),
    "keys"
  )

  expect_silent(
    x1 <- relational_dataset_connector(
      dataname = "ADSL",
      pull_callable = fun,
      keys = get_cdisc_keys("ADSL")
    )
  )


  expect_identical(
    x1$get_code(deparse = TRUE),
    "ADSL <- radsl(N = 5, seed = 1, cached = TRUE)"
  )


  expect_identical(
    x1$get_code(deparse = FALSE),
    as.list(as.call(parse(text = "ADSL <- radsl(N = 5, seed = 1, cached = TRUE)")))
  )

  expect_error(
    x1$get_dataset(),
    "'ADSL' has not been pulled yet"
  )

  expect_error(
    get_dataset(x1),
    "'ADSL' has not been pulled yet"
  )

  expect_error(
    x1$get_raw_data(),
    "'ADSL' has not been pulled yet"
  )


  expect_silent(x1$pull())

  expect_true(
    is(x1$get_dataset(), "RelationalDataset")
  )

  expect_identical(
    get_dataset(x1)$get_keys(),
    x1$get_keys()
  )


  expect_identical(
    get_raw_data(x1),
    radsl(N = 5, seed = 1, cached = TRUE)
  )

  expect_silent(
    x2 <- relational_dataset_connector(
      dataname = "ADSL",
      pull_callable = fun,
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
    get_dataset(x2)$get_keys()
  )



  fun <- callable_function(data.frame)
  fun$set_args(list(id = 1:3, marker = c(100, 1, 10), alive = TRUE))
  fun$set_args(list(new_feature = c(3, 4, 1)))

  expect_silent(
    x3 <- relational_dataset_connector(
      dataname = "ADSL",
      pull_callable = fun,
      keys = keys(primary = "id", foreign = NULL, parent = NULL)
    )
  )

  expect_identical(
    get_code(x3),
    "ADSL <- data.frame(id = 1:3, marker = c(100, 1, 10), alive = TRUE, new_feature = c(3, 4, 1))"
  )

  expect_silent(
    m <- mutate_dataset(x3, "ADSL$newest <- 'xxx'")
  )

  expect_silent(load_dataset(m))

  expect_silent(
    m <- mutate_dataset(x3, "ADSL$newest2 <- 'best'")
  )

  expect_true(
    is(get_dataset(m), "RelationalDataset")
  )

  expect_identical(
    get_raw_data(m),
    data.frame(
      id = 1:3, marker = c(100, 1, 10), alive = TRUE, new_feature = c(3, 4, 1),
      newest = "xxx", newest2 = "best", stringsAsFactors = FALSE
    )
  )
})

# Test conversions
test_that("conversions", {
  fun <- callable_function(radsl)
  fun$set_args(list(N = 5, seed = 1, cached = TRUE))

  x <- raw_dataset_connector(pull_callable = fun)

  expect_silent(
    x1 <- as_relational(
      dataname = "ADSL",
      x = x,
      keys = get_cdisc_keys("ADSL")
    )
  )

  expect_true(
    is(x1, "RelationalDatasetConnector")
  )

  expect_error(
    x1$get_dataset(),
    "'ADSL' has not been pulled yet"
  )

  expect_silent(load_dataset(x1))

  expect_true(
    is(x1, "RelationalDatasetConnector")
  )

  expect_silent(load_dataset(x))
  expect_identical(
    get_raw_data(x1),
    x$get_dataset()$get_raw_data()
  )


  expect_warning(
    x2 <- as_relational(
      dataname = "ADSL",
      x = x,
      keys = get_cdisc_keys("ADSL")
    ),
    "Avoid pulling before conversion"
  )

  expect_true(
    is(x2, "RelationalDatasetConnector")
  )
})

test_that("as_relational", {
  fun <- callable_function(radsl)
  fun$set_args(list(N = 5, seed = 1, cached = TRUE))

  x <- raw_dataset_connector(pull_callable = fun)

  expect_silent(
    x1 <- as_relational(
      dataname = "ADSL",
      x = x,
      code = "ADSL$test_col <- seq_len(nrow(ADSL))",
      keys = get_cdisc_keys("ADSL")
    )
  )


  expect_identical(
    x1$get_code(),
    "ADSL <- radsl(N = 5, seed = 1, cached = TRUE)\nADSL$test_col <- seq_len(nrow(ADSL))"
  )

  expect_error(
    get_dataset(x1),
    "'ADSL' has not been pulled yet"
  )

  expect_silent(
    load_dataset(x1)
  )


  expect_identical(
    as.call(parse(text = x1$get_code())),
    as.call(parse(text = get_dataset(x1)$get_code()))
  )
})

test_that("rcd_dataset_connector", {
  x <- rcd_cdisc_dataset_connector(
    dataname = "ADSL",
    radsl,
    cached = TRUE,
    N = 400
  )
  x2 <- rcd_dataset_connector(
    dataname = "ADSL",
    radsl,
    cached = TRUE,
    N = 400,
    keys = get_cdisc_keys("ADSL")
  )
  expect_equal(x, x2)
  expect_true(is(x, c("DatasetConnector", "R6")))

  expect_identical(
    x$.__enclos_env__$private$pull_callable$.__enclos_env__$private$fun_name,
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
  x <- rds_cdisc_dataset_connector(
    dataname = "ADSL",
    file = "./data_connectors/table.rds"
  )
  x2 <- rds_dataset_connector(
    dataname = "ADSL",
    file = "./data_connectors/table.rds",
    keys = get_cdisc_keys("ADSL")
  )

  expect_error(
    rds_cdisc_dataset_connector(dataname = "ADSL", file = "./data_connectors/table_notexists.rds")
  )

  expect_equal(x, x2)
  expect_true(is(x, c("DatasetConnector", "R6")))

  expect_equal(
    x$get_code(),
    "ADSL <- readRDS(file = \"./data_connectors/table.rds\")"
  )
})

test_that("csv_dataset_connector", {

  # check error if csv file doesn't exist
  expect_error(
    csv_dataset_connector("ADSL", file = "not_exists.csv", keys = get_cdisc_keys("ADSL"))
  )

  # create csv file
  ADSL <- radsl(cached = TRUE) # nolint
  temp_file <- tempfile()
  on.exit(unlink(tempfile()))
  write.csv(ADSL, file = temp_file, row.names = FALSE)

  # check can pull data and get code
  x <- csv_dataset_connector("ADSL", file = temp_file, keys = get_cdisc_keys("ADSL"))
  x$pull()
  expect_equal(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file, "\", delim = \",\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_equal(nrow(data), nrow(ADSL))
  expect_equal(ncol(data), ncol(ADSL))


  # next check can pass arguments to read_delim (e.g. delim)
  write.table(ADSL, file = temp_file, row.names = FALSE, sep = "|")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file, delim = "|")
  x$pull()
  expect_equal(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file, "\", delim = \"|\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_equal(nrow(data), nrow(ADSL))
  expect_equal(ncol(data), ncol(ADSL))
})

test_that("script_dataset_connector", {
  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "
    library(random.cdisc.data)
    ADSL <- radsl(cache = TRUE)
    ADSL"
    ),
    con = file_example
  )

  x <- script_dataset_connector(
    dataname = "ADSL",
    file = file_example,
    keys = get_cdisc_keys("ADSL")
  )

  wrong_file <- "notexists.R"
  expect_error(
    script_dataset_connector(
      dataname = "ADSL",
      file = wrong_file,
      keys = get_cdisc_keys("ADSL")
    ),
    sprintf("File %s does not exist.", wrong_file)
  )

  expect_silent(load_dataset(x))

  expect_true(is(get_dataset(x), c("RelationalDataset", "R6")))

  expect_true(is(get_raw_data(x), c("data.frame")))
})

test_that("script_cdisc_dataset_connector", {
  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "
    library(random.cdisc.data)
    ADSL <- radsl(cache = TRUE)
    ADSL"
    ),
    con = file_example
  )

  x <- script_cdisc_dataset_connector(
    dataname = "ADSL",
    file = file_example
  )

  expect_silent(load_dataset(x))

  expect_true(is(get_dataset(x), c("RelationalDataset", "R6")))

  expect_true(is(get_raw_data(x), c("data.frame")))
})

test_that("rice_dataset", {
  x <- rice_data(
    rice_dataset_connector("ADSL", "/path/to/ADSL", keys = get_cdisc_keys("ADSL")),
    rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
  )

  expect_equal(
    x$get_items()[[1]],
    rice_dataset_connector("ADSL", "/path/to/ADSL", keys = get_cdisc_keys("ADSL"))
  )
  expect_equal(
    x$get_items()[[2]],
    rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
  )

  expect_identical(
    x$get_items()[[1]]$get_code(),
    "ADSL <- rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE, quiet = TRUE)"
  )


  x <- mutate_dataset(rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB"), code = "ADLB$x <- 1")
  expect_equal(
    get_code(x),
    "ADLB <- rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE, quiet = TRUE)\nADLB$x <- 1"
  )
})

test_that("fun_cdisc_dataset_connector", {
  my_data_1 <- function() {
    set.seed(1234)
    # whatever code
    require(dplyr)
    x <- data.frame(
      STUDYID = 1,
      USUBJID = 1:40,
      z = stats::rnorm(40),
      zz = factor(sample(letters[1:3], 40, replace = T)),
      NAs = rep(NA, 40)
    )
    x$w <- as.numeric(mvrnorm(40, 0, 1))
    x$ww <- as.numeric(mvrnorm(40, 0, 1))
    rtables::var_labels(x) <- c("STUDYID", "USUBJID", "z", "zz", "NAs", "w", "ww")
    x
  }

  global_var <- 40
  my_data_wrong <- function() {
    # whatever code
    set.seed(1234)
    x <- data.frame(
      STUDYID = 1,
      USUBJID = 1:global_var,
      z = stats::rnorm(40),
      zz = factor(sample(letters[1:3], 40, replace = T)),
      NAs = rep(NA, 40)
    )
    x$w <- as.numeric(mvrnorm(40, 0, 1))
    x$ww <- as.numeric(mvrnorm(40, 0, 1))
    rtables::var_labels(x) <- c("STUDYID", "USUBJID", "z", "zz", "NAs", "w", "ww")
    x
  }

  y_1 <- fun_cdisc_dataset_connector(
    dataname = "ADSL",
    func = my_data_1
  )

  y_wrong <- fun_cdisc_dataset_connector(
    dataname = "ADSL",
    func = my_data_wrong
  )

  expect_message(y_1$pull())

  expect_error(y_wrong$pull())

  expect_identical(get_raw_data(y_1), my_data_1())

})
