library(scda)

# Test TealDatasetConnector ------
testthat::test_that("TealDatasetConnector", {
  fun <- callable_function(function() synthetic_cdisc_data("latest")$adsl)

  testthat::expect_error(
    dataset_connector(pull_callable = fun),
    "dataname"
  )

  testthat::expect_silent(
    x1 <- dataset_connector(
      dataname = "ADSL",
      pull_callable = fun,
      keys = get_cdisc_keys("ADSL")
    )
  )


  testthat::expect_identical(
    x1$get_code(deparse = TRUE),
    "ADSL <- (function() synthetic_cdisc_data(\"latest\")$adsl)()"
  )

  testthat::expect_equal(
    x1$get_code(deparse = FALSE),
    as.list(as.call(parse(text = 'ADSL <- (function() synthetic_cdisc_data("latest")$adsl)()')))
  )

  testthat::expect_error(
    x1$get_dataset(),
    "'ADSL' has not been pulled yet"
  )

  testthat::expect_error(
    get_dataset(x1),
    "'ADSL' has not been pulled yet"
  )

  testthat::expect_error(
    x1$get_raw_data(),
    "'ADSL' has not been pulled yet"
  )


  testthat::expect_silent(x1$pull())

  testthat::expect_true(
    is(x1$get_dataset(), "TealDataset")
  )

  testthat::expect_identical(
    get_keys(get_dataset(x1)),
    get_keys(x1)
  )

  testthat::expect_identical(
    x1$get_raw_data(),
    synthetic_cdisc_data("latest")$adsl
  )

  testthat::expect_silent(
    x2 <- dataset_connector(
      dataname = "ADSL",
      pull_callable = fun,
      keys = get_cdisc_keys("ADSL")
    )
  )

  testthat::expect_identical(
    get_keys(x2),
    get_cdisc_keys("ADSL")
  )

  testthat::expect_silent(x2$pull())
  testthat::expect_identical(
    get_keys(x2),
    get_keys(get_dataset(x2))
  )



  fun <- callable_function(data.frame)
  fun$set_args(list(id = 1:3, marker = c(100, 1, 10), alive = TRUE))
  fun$set_args(list(new_feature = c(3, 4, 1)))

  testthat::expect_silent(
    x3 <- dataset_connector(
      dataname = "ADSL",
      pull_callable = fun,
      keys = "id"
    )
  )

  testthat::expect_identical(
    x3$get_code(),
    "ADSL <- data.frame(id = 1:3, marker = c(100, 1, 10), alive = TRUE, new_feature = c(3, 4, 1))"
  )

  m <- mutate_dataset(x3, "ADSL$newest <- 'xxx'")

  testthat::expect_silent(load_dataset(m))

  testthat::expect_silent(
    m <- mutate_dataset(x3, "ADSL$newest2 <- 'best'")
  )

  testthat::expect_true(
    is(get_dataset(m), "TealDataset")
  )

  testthat::expect_identical(
    m$get_raw_data(),
    data.frame(
      id = 1:3, marker = c(100, 1, 10), alive = TRUE, new_feature = c(3, 4, 1),
      newest = "xxx", newest2 = "best", stringsAsFactors = FALSE
    )
  )
})

testthat::test_that("metadata for TealDatasetConnector can be Callable, list or NULL", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(a = c(1, 2, 3)))
  metadata_fun <- callable_function(function(a, b) list(A = a, B = b))
  metadata_fun$set_args(args = list(a = TRUE, b = 12))

  testthat::expect_error(
    dataset_connector("test", pull_fun, metadata = list(A = TRUE)),
    NA
  )

  testthat::expect_error(
    dataset_connector("test", pull_fun, metadata = NULL),
    NA
  )

  testthat::expect_error(
    dataset_connector("test", pull_fun, metadata = metadata_fun),
    NA
  )
})

# Test conversions
testthat::test_that("scda_dataset_connector", {
  x <- scda_cdisc_dataset_connector(
    dataname = "ADSL",
    "adsl"
  )
  x2 <- scda_dataset_connector(
    dataname = "ADSL",
    "adsl",
    keys = get_cdisc_keys("ADSL")
  ) %>%
    as_cdisc()
  testthat::expect_equal(x, x2)
  testthat::expect_true(is(x, c("TealDatasetConnector", "R6")))

  testthat::expect_identical(
    x$.__enclos_env__$private$pull_callable$.__enclos_env__$private$fun_name,
    "scda::synthetic_cdisc_dataset"
  )

  testthat::expect_identical(
    x$get_dataname(),
    "ADSL"
  )

  testthat::expect_equal(
    x$get_code(),
    "ADSL <- scda::synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")"
  )

  testthat::expect_silent(
    load_dataset(x)
  )

  testthat::expect_identical(
    x$get_raw_data(),
    synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest")
  )

  testthat::expect_equal(
    x$get_dataset()$get_metadata(),
    list(type = "scda", version = "latest")
  )
})

testthat::test_that("rds_dataset_connector", {
  x <- rds_cdisc_dataset_connector(
    dataname = "ADSL",
    file = "./data_connectors/table.rds"
  )
  x2 <- rds_dataset_connector(
    dataname = "ADSL",
    file = "./data_connectors/table.rds",
    keys = get_cdisc_keys("ADSL")
  ) %>%
    as_cdisc()

  testthat::expect_error(
    rds_cdisc_dataset_connector(dataname = "ADSL", file = "./data_connectors/table_notexists.rds")
  )

  testthat::expect_equal(x, x2)
  testthat::expect_true(is(x, c("TealDatasetConnector", "R6")))

  testthat::expect_equal(
    x$get_code(),
    "ADSL <- readRDS(file = \"./data_connectors/table.rds\")"
  )
})

# test with unexpected input
testthat::test_that("csv_dataset_connector not expected input", {

  # check error if csv file doesn't exist
  testthat::expect_error(
    csv_dataset_connector("ADSL", file = "not_exists.csv", keys = get_cdisc_keys("ADSL"))
  )

  # check error if args are named
  testthat::expect_error(
    csv_dataset_connector("ADSL",
      file = temp_file_csv,
      keys = get_cdisc_keys("ADSL"),
      code = character(0),
      script = character(0),
      label = character(0),
      "a"
    )
  )
  testthat::expect_error(
    csv_dataset_connector("ADSL", file = c("a", "b")),
    "Assertion on 'file'"
  )
  testthat::expect_error(
    csv_dataset_connector("ADSL", file = 1),
    "Assertion on 'file'"
  )
})

# test with cdisc data input
testthat::test_that("csv_dataset_connector scda", {
  # create csv file
  adsl <- synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest")
  temp_file_csv <- tempfile(fileext = ".csv")
  write.csv(adsl, file = temp_file_csv, row.names = FALSE)

  # check can pull data and get code without delimiter assigned
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv)
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(),
    paste0("ADSL <- readr::read_delim(file = \"", encodeString(temp_file_csv), "\", delim = \",\")")
  )
  data <- x$get_raw_data()
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(adsl))
  testthat::expect_identical(colnames(data), colnames(adsl))

  # next check can pass arguments to read_delim (e.g. delim = '|')
  temp_file_csv <- tempfile(fileext = ".csv")
  write.table(adsl, file = temp_file_csv, row.names = FALSE, sep = "|")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "|")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(),
    paste0("ADSL <- readr::read_delim(file = \"", encodeString(temp_file_csv), "\", delim = \"|\")")
  )
  data <- x$get_raw_data()
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(adsl))
  testthat::expect_identical(ncol(data), ncol(adsl))
  testthat::expect_identical(colnames(data), colnames(adsl))

  # next check can pass arguments to read_delim (using '\t')
  temp_file_csv <- tempfile(fileext = ".csv")
  write.table(adsl, file = temp_file_csv, row.names = FALSE, sep = "\t")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "\t")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(),
    paste0("ADSL <- readr::read_delim(file = \"", encodeString(temp_file_csv), "\", delim = \"\\t\")")
  )
  data <- x$get_raw_data()
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(adsl))
  testthat::expect_identical(ncol(data), ncol(adsl))
  testthat::expect_identical(colnames(data), colnames(adsl))

  # next check can pass arguments to read_delim (using ';')
  temp_file_csv <- tempfile(fileext = ".csv")
  write.table(adsl, file = temp_file_csv, row.names = FALSE, sep = ";")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = ";")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(),
    paste0("ADSL <- readr::read_delim(file = \"", encodeString(temp_file_csv), "\", delim = \";\")")
  )
  data <- x$get_raw_data()
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(adsl))
  testthat::expect_identical(ncol(data), ncol(adsl))
  testthat::expect_identical(colnames(data), colnames(adsl))
})

# non-standard dataset
testthat::test_that("csv_dataset_connector non-standard datasets multi/space character delim", {
  test_adsl <- synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest")
  test_adsl_ns <- data.frame(
    STUDYID = "A",
    USUBJID = paste0("A", 1:3),
    SUBJID = 1:3,
    RACE = c("sth1|sth2", "sth", "sth"),
    stringsAsFactors = FALSE
  )

  # next check can pass arguments to read_delim (using '$')
  temp_file_csv <- tempfile(fileext = ".csv")
  write.table(test_adsl_ns, file = temp_file_csv, row.names = FALSE, sep = "$")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "$")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(),
    paste0("ADSL <- readr::read_delim(file = \"", encodeString(temp_file_csv), "\", delim = \"$\")")
  )
  data <- x$get_raw_data()
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(test_adsl_ns))
  testthat::expect_equal(colnames(x$get_raw_data()), colnames(test_adsl_ns))

  # next check can pass arguments to read_delim (using space ' ')
  temp_file_csv <- tempfile(fileext = ".csv")
  write.table(test_adsl, file = temp_file_csv, row.names = FALSE, sep = " ")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, keys = get_cdisc_keys("ADSL"), delim = " ")
  testthat::expect_warning(x$pull())
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(), paste0(
      "ADSL <- readr::read_delim(file = \"",
      encodeString(temp_file_csv), "\", delim = \" \")"
    )
  )
  data <- x$get_raw_data()
  testthat::expect_true(is.data.frame(data))
  testthat::expect_false(identical(data, test_adsl))
})

# column names attributes
testthat::test_that("csv_dataset_connector attritubes", {
  ADSL_ns <- data.frame( # nolint
    STUDYID = "A",
    USUBJID = paste0("A", 1:3),
    SUBJID = 1:3,
    RACE = c("sth1|sth2", "sth", "sth"),
    stringsAsFactors = FALSE
  )
  variable_labels(ADSL_ns) <- letters[1:4] # nolint
  temp_file_csv <- tempfile(fileext = ".csv")
  write.table(ADSL_ns, file = temp_file_csv, row.names = FALSE, sep = ",")

  # check can pull data and get code
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = ",")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(),
    paste0("ADSL <- readr::read_delim(file = \"", encodeString(temp_file_csv), "\", delim = \",\")")
  )
  data <- x$get_raw_data()
  testthat::expect_null(attributes(data[[1]])$label)

  # we should use mutate_dataset
  data <- (x %>% mutate_dataset("variable_labels(ADSL) <- letters[1:4]"))$get_raw_data()
  testthat::expect_identical(attributes(data[[1]])$label, "a")
})

# test csv_cdisc_dataset_connector
testthat::test_that("csv_cdisc_dataset_connector scda", {
  # create csv file
  adsl <- synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest")
  temp_file_csv <- tempfile(fileext = ".csv")
  write.csv(adsl, file = temp_file_csv, row.names = FALSE)

  # check can pull data and get code without delimiter assigned
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv)
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(),
    paste0("ADSL <- readr::read_delim(file = \"", encodeString(temp_file_csv), "\", delim = \",\")")
  )
  data <- x$get_raw_data()
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(adsl))
  testthat::expect_identical(colnames(data), colnames(adsl))

  # next check can pass arguments to read_delim (e.g. delim = '|')
  temp_file_csv <- tempfile(fileext = ".csv")
  write.table(adsl, file = temp_file_csv, row.names = FALSE, sep = "|")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "|")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    x$get_code(),
    paste0("ADSL <- readr::read_delim(file = \"", encodeString(temp_file_csv), "\", delim = \"|\")")
  )
  data <- x$get_raw_data()
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(adsl))
  testthat::expect_identical(ncol(data), ncol(adsl))
  testthat::expect_identical(colnames(data), colnames(adsl))
})

testthat::test_that("script_dataset_connector", {
  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "
    library(scda)
    ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")
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
  testthat::expect_error(
    script_dataset_connector(
      dataname = "ADSL",
      file = wrong_file,
      keys = get_cdisc_keys("ADSL")
    ),
    sprintf("File %s does not exist.", wrong_file)
  )

  testthat::expect_silent(load_dataset(x))

  testthat::expect_true(is(get_dataset(x), c("TealDataset", "R6")))

  testthat::expect_true(is(x$get_raw_data(), c("data.frame")))
})

testthat::test_that("script_cdisc_dataset_connector", {
  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "
    library(scda)
    ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")
    ADSL"
    ),
    con = file_example
  )

  x <- script_cdisc_dataset_connector(
    dataname = "ADSL",
    file = file_example
  )

  testthat::expect_silent(load_dataset(x))

  testthat::expect_true(is(get_dataset(x), c("TealDataset", "R6")))

  testthat::expect_true(is(x$get_raw_data(), c("data.frame")))
})

testthat::test_that("fun_cdisc_dataset_connector", {
  my_data_1 <- function() {
    set.seed(1234)
    # whatever code
    require(dplyr)
    x <- data.frame(
      STUDYID = 1,
      USUBJID = 1:40,
      z = stats::rnorm(40),
      zz = factor(sample(letters[1:3], 40, replace = TRUE)),
      NAs = rep(NA, 40)
    )
    x$w <- as.numeric(rnorm(40, 0, 1))
    x$ww <- as.numeric(rnorm(40, 0, 1))
    variable_labels(x) <- c("STUDYID", "USUBJID", "z", "zz", "NAs", "w", "ww")
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
      zz = factor(sample(letters[1:3], 40, replace = TRUE)),
      NAs = rep(NA, 40)
    )
    x$w <- as.numeric(rnorm(40, 0, 1))
    x$ww <- as.numeric(rnorm(40, 0, 1))
    variable_labels(x) <- c("STUDYID", "USUBJID", "z", "zz", "NAs", "w", "ww")
    x
  }

  y_1 <- fun_cdisc_dataset_connector(
    dataname = "ADSL",
    fun = my_data_1
  )

  y_wrong <- fun_cdisc_dataset_connector(
    dataname = "ADSL",
    fun = my_data_wrong
  )

  y_1$pull()

  expect_equal(environmentName(environment(my_data_wrong)), environmentName(environment(my_data_1)))

  expect_error(y_wrong$pull())

  expect_identical(y_1$get_raw_data(), my_data_1())

  fun_direct <- fun_cdisc_dataset_connector(
    dataname = "ADSL",
    fun = synthetic_cdisc_dataset,
    fun_args = list(dataset_name = "adsl", name = "latest")
  )

  fun_direct2 <- fun_cdisc_dataset_connector(
    dataname = "ADSL",
    fun = scda::synthetic_cdisc_dataset,
    fun_args = list(dataset_name = "adsl", name = "latest")
  )
  fun_direct$pull()

  fun_direct2$pull()

  data_1 <- fun_direct$get_raw_data()
  data_2 <- fun_direct2$get_raw_data()

  testthat::expect_true(is.data.frame(data_1))
  testthat::expect_true(is.data.frame(data_2))
  expect_identical(data_1, data_2)
})

testthat::test_that("code_dataset_connector - Test various inputs", {
  adsl <- synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest")

  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c("ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\nADSL"),
    con = file_example
  )

  from_file <- code_dataset_connector(
    dataname = "ADSL",
    code = paste0(readLines(file_example), collapse = "\n")
  )

  expect_equal(
    from_file$get_code(),
    "ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\nADSL <- ADSL"
  )
  expect_identical(from_file$pull()$get_raw_data(), adsl)

  adsl <- synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest")

  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "mtcars
      # code ADSL>
      library(scda)
      ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")
      ADSL
      # <ADSL code
      ADAE <- synthetic_cdisc_dataset(dataset_name = \"adae\", name = \"latest\")"
    ),
    con = file_example
  )

  get_code_file <- code_dataset_connector(
    dataname = "ADSL",
    # This get_code is the method for a character data type,
    # which is unrelated and not a wrapper to any of the R6 classes' $get_code.
    # So it does not break convention to leave it here instead.
    # It needs to be called here.
    code = get_code(file_example, dataname = "ADSL")
  )

  expect_equal(
    get_code_file$get_code(),
    "library(scda)\nADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\nADSL <- ADSL"
  )
  expect_identical(get_code_file$pull()$get_raw_data(), adsl)
})

testthat::test_that("code_dataset_connector - Modify vars", {
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest"),
    keys = get_cdisc_keys("ADSL"),
    code = "synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")",
    label = "ADSL dataset"
  )

  adtte <- dataset_connector(
    dataname = "ADTTE",
    pull_callable = callable_code(
      "ADSL <- dplyr::filter(ADSL, SEX == 'F')
      radtte(
        cached = TRUE
      )"
    ),
    keys = get_cdisc_keys("ADTTE"),
    label = "ADTTE dataset",
    vars = list(ADSL = adsl)
  )

  expect_error(
    adtte$pull(try = FALSE),
    "Modification of the local variable 'ADSL' is not allowed."
  )

  testthat::expect_output(adtte$pull(try = TRUE), "\\[ERROR\\] .* TealDatasetConnector\\$pull failed to pull")

  testthat::expect_true(
    grepl("Modification of the local variable", adtte$get_error_message())
  )
})

testthat::test_that("code_dataset_connector - library calls", {
  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(synthetic_cdisc_dataset) %>%
      set_args(args = list(dataset_name = "adsl", name = "latest")),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL dataset"
  )

  adtte <- dataset_connector(
    dataname = "ADTTE",
    pull_callable = callable_code(
      "library(dplyr)
      synthetic_cdisc_dataset(dataset_name = \"adtte\", name = \"latest\") %>%
        filter(SEX == 'F')"
    ),
    keys = get_cdisc_keys("ADTTE"),
    label = "ADTTE dataset"
  )

  adrs <- dataset_connector(
    dataname = "ADRS",
    pull_callable = callable_code(
      "library(dplyr)
      synthetic_cdisc_dataset(dataset_name = \"adrs\", name = \"latest\") %>%
        filter(SEX == 'F')"
    ),
    keys = get_cdisc_keys("ADRS"),
    label = "ADRS dataset"
  )

  data <- cdisc_data(adsl, adtte, adrs, check = TRUE)
  expect_silent(
    lapply(
      data$get_items(),
      load_dataset
    )
  )

  datasets <- get_datasets(data)
  expect_identical(
    datasets[[1]]$get_raw_data(),
    synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest")
  )

  expect_identical(
    unique(datasets[[2]]$get_raw_data()$SEX),
    factor("F", levels = c("F", "M"))
  )

  expect_identical(
    unique(datasets[[3]]$get_raw_data()$SEX),
    factor("F", levels = c("F", "M"))
  )
})

testthat::test_that("TealDatasetConnector mutate method with delayed logic", {
  test_ds1 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  test_ds2 <- TealDataset$new("head_iris", head(iris), code = "head_iris <- head(iris)")
  testthat::expect_true(all(test_ds1$check(), test_ds2$check()))

  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun, vars = list(test_ds1 = test_ds1))

  pull_fun2 <- callable_function(data.frame)
  pull_fun2$set_args(args = list(head_integers = 1:6))
  t_dc2 <- dataset_connector("test_dc2", pull_fun2, vars = list(test_ds2 = test_ds2))

  testthat::expect_false(t_dc$is_mutate_delayed())
  # mutation is delayed when data hasn't been loaded/pulled yet.
  mutate_dataset(t_dc, code = "test_dc$tail_letters <- tail(letters)")
  testthat::expect_true(t_dc$is_mutate_delayed())
  testthat::expect_equal(
    pretty_code_string(t_dc$get_code()),
    c(
      "head_mtcars <- head(mtcars)",
      "test_ds1 <- head_mtcars",
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "test_dc$tail_letters <- tail(letters)"
    )
  )
  testthat::expect_false(t_dc$is_pulled())
  load_dataset(t_dc)
  testthat::expect_false(t_dc$is_mutate_delayed())
  testthat::expect_true(all(c("head_letters", "tail_letters") %in% names(t_dc$get_raw_data())))

  testthat::expect_equal(
    pretty_code_string(t_dc$get_code()),
    c(
      "head_mtcars <- head(mtcars)",
      "test_ds1 <- head_mtcars",
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "test_dc$tail_letters <- tail(letters)"
    )
  )

  # mutation is delayed because t_dc2 hasn't been loaded yet
  mutate_dataset(t_dc, code = "test_dc$head_integers <- t_dc2$head_integers", vars = list(t_dc2 = t_dc2))
  testthat::expect_true(t_dc$is_mutate_delayed())
  testthat::expect_equal(
    pretty_code_string(t_dc$get_code()),
    c(
      "head_mtcars <- head(mtcars)",
      "test_ds1 <- head_mtcars",
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "head_iris <- head(iris)",
      "test_ds2 <- head_iris",
      "test_dc2 <- data.frame(head_integers = 1:6)",
      "t_dc2 <- test_dc2",
      "test_dc$tail_letters <- tail(letters)",
      "test_dc$head_integers <- t_dc2$head_integers"
    )
  )
  # mutation is delayed even, though it could be executed, because it had already been delayed
  mutate_dataset(t_dc, code = "test_dc$one <- 1")
  testthat::expect_true(t_dc$is_mutate_delayed())
  testthat::expect_equal(
    pretty_code_string(t_dc$get_code()),
    c(
      "head_mtcars <- head(mtcars)",
      "test_ds1 <- head_mtcars",
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "head_iris <- head(iris)",
      "test_ds2 <- head_iris",
      "test_dc2 <- data.frame(head_integers = 1:6)",
      "t_dc2 <- test_dc2",
      "test_dc$tail_letters <- tail(letters)",
      "test_dc$head_integers <- t_dc2$head_integers",
      "test_dc$one <- 1"
    )
  )

  load_dataset(t_dc2)
  # testing t_dc$pull, which re-runs all (already executed and staged) mutate code
  # "head_letters" and "tail_letters" columns had already been executed
  # "head_integers" and "one" columns are delayed
  load_dataset(t_dc)
  testthat::expect_true(all(c("head_letters", "tail_letters", "head_integers", "one") %in% names(t_dc$get_raw_data())))
  testthat::expect_false(t_dc$is_mutate_delayed())

  # mutate should again be eager
  mutate_dataset(t_dc2, code = "test_dc2$five <- 5")
  testthat::expect_equal(t_dc2$get_raw_data()$five, rep(5, 6))

  mutate_dataset(t_dc, code = "test_dc$five <- t_dc2$five", vars = list(t_dc2 = t_dc2))
  testthat::expect_equal(t_dc$get_raw_data()$five, rep(5, 6))
  testthat::expect_false(t_dc$is_mutate_delayed())

  # multiple lines of identical code
  mutate_dataset(t_dc, code = "test_dc$five <- 2 * test_dc$five")
  mutate_dataset(t_dc, code = "test_dc$five <- 2 * test_dc$five")
  mutate_dataset(t_dc, code = "test_dc$five <- 2 * test_dc$five")
  testthat::expect_equal(t_dc$get_raw_data()$five, rep(40, 6))
  testthat::expect_false(t_dc$is_mutate_delayed())

  # multi layer dependencies
  pull_fun3 <- callable_function(data.frame)
  pull_fun3$set_args(args = list(neg_integers = -(1:6))) # nolint
  t_dc3 <- dataset_connector("test_dc3", pull_fun3)

  mutate_dataset(t_dc2, code = "test_dc2$neg_integers <- t_dc3$neg_integers", vars = list(t_dc3 = t_dc3))
  testthat::expect_true(t_dc2$is_mutate_delayed())

  # t_dc doesn't know that t_dc2 is delayed
  testthat::expect_false(t_dc$is_mutate_delayed())
  # delayed, even though the column is ready, because t_dc2 is delayed by t_dc3
  mutate_dataset(t_dc, code = "test_dc$six <- test_dc$five + 1", vars = list(t_dc2 = t_dc2))
  testthat::expect_true(t_dc$is_mutate_delayed())

  testthat::expect_true(
    all(c("test_dc2$neg_integers <- t_dc3$neg_integers", "test_dc$six <- test_dc$five + 1") %in%
      pretty_code_string(t_dc$get_code()))
  )
  testthat::expect_true(t_dc$is_mutate_delayed())

  mutate_dataset(t_dc, code = "test_dc$seven <- 7")
  testthat::expect_true("test_dc$seven <- 7" %in% pretty_code_string(t_dc$get_code()))
  testthat::expect_true(t_dc$is_mutate_delayed())
  # confirming that mutation has not happened
  testthat::expect_silent(t_dc$get_raw_data())
  load_dataset(t_dc3)
  testthat::expect_false(any(c("six", "seven") %in% names(t_dc$get_raw_data())))

  # current state
  testthat::expect_true(all(
    names(t_dc$get_raw_data()) %in% c("head_letters", "tail_letters", "head_integers", "one", "five")
  ))

  # load_dataset, which calls pull method, will reset to original state because dependencies have changed
  load_dataset(t_dc)

  testthat::expect_true(t_dc$is_mutate_delayed())
  # original state. all columns resulting from mutations have been removed
  testthat::expect_true(all(names(t_dc$get_raw_data()) %in% c("head_letters")))
  # still it must return code from all previously inputted mutate statements
  testthat::expect_true(
    "test_dc$seven <- 7" %in% pretty_code_string(t_dc$get_code())
  )

  # confirming that mutation has not happened
  testthat::expect_false(any(c("six", "seven") %in% names(t_dc$get_raw_data())))
  # confirming that mutation is delayed
  testthat::expect_true(t_dc2$is_mutate_delayed())

  # confirming get_raw_data will eager mutate t_dc2 because t_dc3 has been loaded
  testthat::expect_true(all(c("head_integers", "five", "neg_integers") %in% names(t_dc2$get_raw_data())))

  # re running all mutation statements
  load_dataset(t_dc)
  testthat::expect_false(t_dc$is_mutate_delayed())
  testthat::expect_true(all(c(
    "head_integers", "tail_letters", "head_integers", "one", "five", "six", "seven"
  ) %in% names(t_dc$get_raw_data())))

  testthat::expect_equal(t_dc$get_raw_data()$seven, rep(7, 6))
  testthat::expect_equal(t_dc$get_raw_data()$six, rep(41, 6))
  testthat::expect_equal(t_dc$get_raw_data()$five, rep(40, 6))
  # back to eager mutate
  mutate_dataset(t_dc, code = "test_dc$eight <- 8")
  testthat::expect_equal(t_dc$get_raw_data()$eight, rep(8, 6))
})

testthat::test_that("TealDatasetConnector mutate method edge cases", {
  # edge because test_ds1 does not contain the code to recreate head_mtcars
  test_ds1 <- TealDataset$new("head_mtcars", head(mtcars))

  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun)
  load_dataset(t_dc)
  testthat::expect_silent(
    mutate_dataset(t_dc, code = "test_dc$new_var <- head_mtcars$carb", vars = list(head_mtcars = test_ds1))
  )
  testthat::expect_equal(t_dc$get_raw_data()$new_var, c(4, 4, 1, 1, 2, 1))
})

testthat::test_that("get_code_class returns the correct CodeClass object", {
  cc1 <- CodeClass$new(code = "iris <- (function() head(iris))()", dataname = "iris")
  cf1 <- CallableFunction$new(function() head(iris))
  dc1 <- TealDatasetConnector$new("iris", cf1)
  testthat::expect_equal(dc1$get_code_class(), cc1)
})

testthat::test_that("Pulled TealDatasetConnector returns the same CodeClass as before pulling", {
  cf1 <- CallableFunction$new(function() head(iris))
  dc1 <- TealDatasetConnector$new("iris", cf1)
  pre_pull_cc <- dc1$get_code_class()
  dc1$pull()
  post_pull_cc <- dc1$get_code_class()

  testthat::expect_equal(post_pull_cc, pre_pull_cc)
})

testthat::test_that("Pulled dependent TealDatasetConnector returns the same CodeClass as before pulling", {
  ds <- TealDataset$new("iris", head(iris), code = "iris <- head(iris)")
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf, vars = list(iris = ds))
  pre_pull_code_class <- dc$get_code_class()
  dc$pull()
  post_pull_code_class <- dc$get_code_class()
  testthat::expect_equal(post_pull_code_class, pre_pull_code_class)
})

testthat::test_that("Pulling twice doesn't change the returned TealDatasetConnector's CodeClass", {
  ds <- TealDataset$new("iris", head(iris), code = "iris <- head(iris)")
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf, vars = list(iris = ds))
  dc$pull()
  pre_pull_code_class <- dc$get_code_class()
  dc$pull()
  post_pull_code_class <- dc$get_code_class()
  testthat::expect_equal(post_pull_code_class, pre_pull_code_class)
})

testthat::test_that("Identical mutation expressions are added to the mutation code", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$mutate("mtcars$test <- 1")
  dc$mutate("mtcars$test <- 1")
  testthat::expect_equal(dc$get_code(), "mtcars <- (function() head(mtcars))()\nmtcars$test <- 1\nmtcars$test <- 1")
})

testthat::test_that("Identical mutation expressions are executed upon pulling the Connector object", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$mutate("mtcars$test <- 1")
  dc$mutate("mtcars$test <- mtcars$test * 2")
  dc$mutate("mtcars$test <- mtcars$test * 2")
  dc$pull()
  testthat::expect_equal(dc$get_raw_data()$test, rep(4, 6))
})

testthat::test_that("Identical mutation expressions are shown in the returned code after pulling", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$mutate("mtcars$test <- 1")
  dc$mutate("mtcars$test <- mtcars$test * 2")
  dc$mutate("mtcars$test <- mtcars$test * 2")
  dc$pull()
  testthat::expect_equal(
    dc$get_code(),
    paste(
      "mtcars <- (function() head(mtcars))()",
      "mtcars$test <- 1",
      "mtcars$test <- mtcars$test * 2",
      "mtcars$test <- mtcars$test * 2",
      sep = "\n"
    )
  )
})

testthat::test_that("TealDatasetConnector$is_mutate_delayed is FALSE if not yet pulled and not mutated", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  testthat::expect_false(dc$is_mutate_delayed())
})

testthat::test_that("TealDatasetConnector$is_mutate_delayed returns FALSE if pulled and not mutated", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$pull()
  testthat::expect_false(dc$is_mutate_delayed())
})

testthat::test_that("TealDatasetConnector$is_mutate_delayed returns TRUE if not pulled and mutated", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$mutate(code = "test")
  testthat::expect_true(dc$is_mutate_delayed())
})

testthat::test_that("TealDatasetConnector$is_mutate_delayed returns TRUE if mutated with no delayed objects and pulled", { # nolint
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$mutate(code = "")
  dc$pull()
  testthat::expect_false(dc$is_mutate_delayed())
})

testthat::test_that("TealDatasetConnector$is_mutate_delayed returns FALSE if mutated with no vars after pulling", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$pull()
  dc$mutate(code = "")
  testthat::expect_false(dc$is_mutate_delayed())
})

testthat::test_that("TealDatasetConnector returns the correct code when mutated with no vars after pulling", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$pull()
  dc$mutate(code = "1")
  testthat::expect_equal(dc$get_code_class()$get_code(), "mtcars <- (function() head(mtcars))()\n1")
})

testthat::test_that("Pulling an already pulled TealDatasetConnector after mutating it with a delayed object
  undoes any eager pre-pull mutations", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$pull()
  dc$mutate(code = "mtcars[1] <- NULL")
  dc$mutate(
    code = "",
    vars = list(delayed = TealDatasetConnector$new("iris", CallableFunction$new(function() head(iris))))
  )
  dc$pull()
  testthat::expect_equal(dc$get_raw_data(), head(mtcars))
})

testthat::test_that("Pulling an already pulled TealDatasetConnector after mutating it with a delayed object
  does not change the returned code", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- TealDatasetConnector$new("mtcars", cf)
  dc$pull()
  dc$mutate(code = "mtcars[1] <- NULL")
  dc$mutate(
    code = "",
    vars = list(delayed = TealDatasetConnector$new("iris", CallableFunction$new(function() head(iris))))
  )
  pre_pull_code <- dc$get_code()
  dc$pull()
  testthat::expect_equal(dc$get_code(), pre_pull_code)
})

testthat::test_that("Initializing TealDatasetConnector with code argument works", {
  test_ds1 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")

  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector(
    "test_dc",
    pull_fun,
    code = "test_dc$tail_letters = tail(letters)",
    vars = list(test_ds1 = test_ds1)
  )
  testthat::expect_equal(
    t_dc$get_code(),
    "head_mtcars <- head(mtcars)\ntest_ds1 <- head_mtcars\ntest_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))\ntest_dc$tail_letters = tail(letters)" # nolint
  )
  testthat::expect_equal(
    attr(t_dc$get_code_class()$code[[1]], "dataname"),
    "head_mtcars"
  )
  testthat::expect_equal(
    attr(t_dc$get_code_class()$code[[2]], "dataname"),
    "head_mtcars"
  )
  testthat::expect_equal(
    attr(t_dc$get_code_class()$code[[3]], "dataname"),
    "test_dc"
  )
  # mutate code passed in as string values will have dataset name as its dataname attribute
  testthat::expect_equal(
    attr(t_dc$get_code_class()$code[[4]], "dataname"),
    "test_dc"
  )
  t_dc$pull()
  testthat::expect_equal(
    t_dc$get_raw_data(),
    data.frame(head_letters = head(letters), tail_letters = tail(letters))
  )
})

testthat::test_that("TealDatasetConnector$get_join_keys returns an empty JoinKeys object", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector(
    "test_dc",
    pull_fun,
    code = "test_dc$tail_letters = tail(letters)"
  )
  testthat::expect_true(is(t_dc$get_join_keys(), "JoinKeys"))
  testthat::expect_equal(length(t_dc$get_join_keys()$get()), 0)
})

testthat::test_that("TealDatasetConnector$set_join_keys works independently", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector(
    "test_dc",
    pull_fun,
    code = "test_dc$tail_letters = tail(letters)"
  )
  testthat::expect_silent(
    t_dc$set_join_keys(join_key("test_dc", "other_dataset", c("Species" = "some_col")))
  )
  testthat::expect_error(
    t_dc$set_join_keys(join_key("test_dc", "other_dataset", c("Sepal.Length" = "some_col2")))
  )
  testthat::expect_true(is(t_dc$get_join_keys(), "JoinKeys"))
  testthat::expect_equal(length(t_dc$get_join_keys()$get()), 2)
})

testthat::test_that("TealDatasetConnector$mutate_join_keys works independently", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector(
    "test_dc",
    pull_fun,
    code = "test_dc$tail_letters = tail(letters)"
  )
  testthat::expect_silent(
    t_dc$mutate_join_keys("other_dataset", c("Sepal.Length" = "some_col2"))
  )
  testthat::expect_true(is(t_dc$get_join_keys(), "JoinKeys"))
  testthat::expect_equal(length(t_dc$get_join_keys()$get()), 2)
})

testthat::test_that("TealDatasetConnector$set_join_keys works with TealDatasetConnector$mutate_join_keys", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector(
    "test_dc",
    pull_fun,
    code = "test_dc$tail_letters = tail(letters)"
  )
  testthat::expect_silent(
    t_dc$set_join_keys(join_key("iris", "other_dataset", c("Species" = "some_col")))
  )
  testthat::expect_identical(
    t_dc$get_join_keys()$get()$iris$other_dataset, c("Species" = "some_col")
  )
  testthat::expect_silent(
    t_dc$mutate_join_keys("other_dataset", c("Sepal.Length" = "some_col2"))
  )
  testthat::expect_silent(
    t_dc$mutate_join_keys("iris", "unique_id")
  )
  testthat::expect_silent(
    join_keys_list <- t_dc$get_join_keys()$get()
  )

  testthat::expect_identical(
    t_dc$get_join_keys()$get()$iris$other_dataset, c("Species" = "some_col")
  )
  testthat::expect_identical(
    t_dc$get_join_keys()$get()$test_dc$other_dataset, c("Sepal.Length" = "some_col2")
  )
  testthat::expect_identical(
    t_dc$get_join_keys()$get()$test_dc$iris, c("unique_id" = "unique_id")
  )
  testthat::expect_identical(
    t_dc$get_join_keys()$get()$other_dataset$test_dc, c("some_col2" = "Sepal.Length")
  )
  testthat::expect_identical(
    t_dc$get_join_keys()$get()$other_dataset$iris, c("some_col" = "Species")
  )
})

testthat::test_that("TealDatasetConnector$get_dataset calls dataset$merge_join_keys before returning", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector(
    "test_dc",
    pull_fun,
    code = "test_dc$tail_letters = tail(letters)"
  )
  t_dc$pull()

  testthat::expect_equal(t_dc$get_dataset()$get_join_keys()$get(), list())
  # initial call
  t_dc$mutate_join_keys("other_dataset", c("Sepal.Length" = "some_col2"))
  testthat::expect_equal(t_dc$get_dataset()$get_join_keys(), t_dc$get_join_keys())

  # subsequent calls
  t_dc$mutate_join_keys("other_dataset", c("Sepal.Length" = "some_other_col"))
  testthat::expect_equal(t_dc$get_dataset()$get_join_keys(), t_dc$get_join_keys())
})

testthat::test_that("TealDatasetConnector$print does not print dataset when not yet pulled", {
  test_ds1 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun, vars = list(test_ds1 = test_ds1))

  out <- capture.output(print(t_dc))

  testthat::expect_equal(
    out,
    "A TealDatasetConnector object, named test_dc, containing a TealDataset object that has not been loaded/pulled"
  )
})

testthat::test_that("TealDatasetConnector$print prints dataset when it is pulled", {
  test_ds1 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun, vars = list(test_ds1 = test_ds1))
  t_dc$pull()
  out <- capture.output(print(t_dc))

  testthat::expect_equal(
    out,
    c(
      "A TealDatasetConnector object, named test_dc, containing a TealDataset object that has been loaded/pulled:",
      "A TealDataset object containing the following data.frame (6 rows and 1 columns):",
      "  head_letters",
      "1            a",
      "2            b",
      "3            c",
      "4            d",
      "5            e",
      "6            f"
    )
  )
})

testthat::test_that("get_var_r6 returns identical objects as these passed to the vars argument in
                    the constructor", {
  test_ds0 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  test_ds1 <- TealDatasetConnector$new(
    dataname = "test_dc",
    pull_callable = CallableFunction$new(data.frame),
    vars = list(test_ds0 = test_ds0)
  )

  vars <- test_ds1$get_var_r6()
  testthat::expect_identical(vars$test_ds0, test_ds0)
})

testthat::test_that("clone(deep = TRUE) deep clones dependencies, which are TealDataset objects", {
  test_ds0 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  test_ds1 <- TealDatasetConnector$new(
    dataname = "test_dc",
    pull_callable = CallableFunction$new(data.frame),
    vars = list(test_ds0 = test_ds0)
  )
  test_ds1_cloned <- test_ds1$clone(deep = TRUE)
  testthat::expect_false(
    identical(test_ds1_cloned$get_var_r6()$test_ds0, test_ds0)
  )
})

testthat::test_that("reassign_datasets_vars updates the references of the vars to
                    addresses of passed objects", {
  test_ds0 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  test_ds1 <- TealDatasetConnector$new(
    dataname = "test_dc",
    pull_callable = CallableFunction$new(data.frame),
    vars = list(test_ds0 = test_ds0)
  )

  # after reassignment vars_r6, vars and muatate_vars match new reference
  test_ds0_cloned <- test_ds0$clone(deep = TRUE)
  test_ds1$reassign_datasets_vars(datasets = list(test_ds0 = test_ds0_cloned))

  vars <- test_ds1$.__enclos_env__$private$pull_vars
  testthat::expect_identical(vars$test_ds0, test_ds0_cloned)
})

testthat::test_that("reassign_datasets_vars updates the references of the vars_r6 to
                    addresses of passed objects", {
  test_ds0 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  test_ds1 <- TealDatasetConnector$new(
    dataname = "test_dc",
    pull_callable = CallableFunction$new(data.frame),
    vars = list(test_ds0 = test_ds0)
  )

  # after reassignment vars_r6, vars and muatate_vars match new reference
  test_ds0_cloned <- test_ds0$clone(deep = TRUE)
  test_ds1$reassign_datasets_vars(datasets = list(test_ds0 = test_ds0_cloned))

  vars_r6 <- test_ds1$get_var_r6()
  testthat::expect_identical(vars_r6$test_ds0, test_ds0_cloned)
})

testthat::test_that("reassign_datasets_vars does not change `vars` elements of
                    class different than TealDataset and TealDatasetConnector", {
  test_ds0 <- mtcars
  test_ds1 <- TealDataset$new("mtcars", mtcars)
  test_ds2 <- TealDatasetConnector$new(
    dataname = "iris",
    pull_callable = callable_function(data.frame),
    vars = list(test_ds0 = test_ds0, test_ds1 = test_ds1)
  )

  test_ds2$reassign_datasets_vars(list(test_ds1 = test_ds1))
  testthat::expect_identical(
    test_ds2$.__enclos_env__$private$pull_vars$test_ds0,
    test_ds0
  )
})

testthat::test_that("reassign_datasets_vars does not change any `vars` while
                    empty list is provided", {
  test_ds0 <- mtcars
  test_ds1 <- TealDataset$new("mtcars", mtcars)
  test_ds2 <- TealDataset$new("iris", iris)
  test_ds2 <- TealDatasetConnector$new(
    dataname = "iris",
    pull_callable = callable_function(data.frame),
    vars = list(test_ds0 = test_ds0, test_ds1 = test_ds1)
  )

  test_ds2$reassign_datasets_vars(list())
  testthat::expect_identical(
    test_ds2$.__enclos_env__$private$pull_vars$test_ds0,
    test_ds0
  )
  testthat::expect_identical(
    test_ds2$.__enclos_env__$private$pull_vars$test_ds1,
    test_ds1
  )
})


testthat::test_that("Callable metadata is pulled when data is pulled", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(a = c(1, 2, 3)))
  metadata_fun <- callable_function(function(a, b) list(A = a, B = b))
  metadata_fun$set_args(args = list(a = TRUE, b = 12))
  x <- dataset_connector("test", pull_fun, metadata = metadata_fun)
  x$pull()

  testthat::expect_equal(
    x$get_dataset()$get_metadata(),
    list(A = TRUE, B = 12)
  )
})

testthat::test_that("list metadata is passed to dataset when data is pulled", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(a = c(1, 2, 3)))
  x <- dataset_connector("test", pull_fun, metadata = list(foo = "bar"))
  x$pull()

  testthat::expect_equal(
    x$get_dataset()$get_metadata(),
    list(foo = "bar")
  )
})

testthat::test_that("if pulling metadata fails, dataset is still created but metadata is NULL", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(a = c(1, 2, 3)))
  metadata_fun <- callable_function(function(a, b) stop("An error"))
  metadata_fun$set_args(args = list(a = TRUE, b = 12))
  x <- dataset_connector("test", pull_fun, metadata = metadata_fun)

  testthat::expect_output(
    load_dataset(x),
    "TealDatasetConnector\\$pull pulling metadata failed for dataset: test"
  )

  testthat::expect_null(x$get_dataset()$get_metadata())
  testthat::expect_true(x$is_pulled())
})


testthat::test_that("if pulled metadata is invalid, dataset is still created but metadata is NULL", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(a = c(1, 2, 3)))
  metadata_fun <- callable_function(function(a, b) 1:10)
  metadata_fun$set_args(args = list(a = TRUE, b = 12))
  x <- dataset_connector("test", pull_fun, metadata = metadata_fun)

  testthat::expect_output(
    load_dataset(x),
    "TealDatasetConnector\\$pull invalid metadata for dataset: test"
  )

  testthat::expect_null(x$get_dataset()$get_metadata())
  testthat::expect_true(x$is_pulled())
})
