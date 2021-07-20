library(random.cdisc.data)
# Test DatasetConnector ------
test_that("DatasetConnector", {
  fun <- callable_function(radsl)
  fun$set_args(list(N = 5, seed = 1, cached = TRUE))

  expect_error(
    dataset_connector(pull_callable = fun),
    "dataname"
  )

  expect_silent(
    x1 <- dataset_connector(
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
    is(x1$get_dataset(), "Dataset")
  )

  expect_identical(
    get_keys(get_dataset(x1)),
    get_keys(x1)
  )

  expect_identical(
    get_raw_data(x1),
    radsl(N = 5, seed = 1, cached = TRUE)
  )

  expect_silent(
    x2 <- dataset_connector(
      dataname = "ADSL",
      pull_callable = fun,
      keys = get_cdisc_keys("ADSL")
    )
  )

  expect_identical(
    get_keys(x2),
    get_cdisc_keys("ADSL")
  )

  expect_silent(x2$pull())
  expect_identical(
    get_keys(x2),
    get_keys(get_dataset(x2))
  )



  fun <- callable_function(data.frame)
  fun$set_args(list(id = 1:3, marker = c(100, 1, 10), alive = TRUE))
  fun$set_args(list(new_feature = c(3, 4, 1)))

  expect_silent(
    x3 <- dataset_connector(
      dataname = "ADSL",
      pull_callable = fun,
      keys = "id"
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
    is(get_dataset(m), "Dataset")
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
test_that("rcd_dataset_connector", {
  x <- rcd_cdisc_dataset_connector(
    dataname = "ADSL",
    radsl,
    cached = TRUE
  )
  x2 <- rcd_dataset_connector(
    dataname = "ADSL",
    radsl,
    cached = TRUE,
    keys = get_cdisc_keys("ADSL")
  ) %>%
    as_cdisc()
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
    "ADSL <- radsl(cached = TRUE)"
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
  ) %>%
    as_cdisc()

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

# test csv_dataset_connector
temp_file_csv <- tempfile(fileext = ".csv")
on.exit(unlink(temp_file_csv))

# test with unexpected input
test_that("csv_dataset_connector not expected input", {

  # check error if csv file doesn't exist
  expect_error(
    csv_dataset_connector("ADSL", file = "not_exists.csv", keys = get_cdisc_keys("ADSL"))
  )

  # check error if args are named
  expect_error(
    csv_dataset_connector("ADSL",
      file = temp_file_csv,
      keys = get_cdisc_keys("ADSL"),
      code = character(0),
      script = character(0),
      label = character(0),
      "a"
    )
  )

  # check error if is_character_single(file)
  expect_error(
    csv_dataset_connector("ADSL", file = c("a", "b"))
  )
  expect_error(
    csv_dataset_connector("ADSL", file = 1)
  )
})

# test with cdisc data input
test_that("csv_dataset_connector random.cdisc.data", {
  # create csv file
  ADSL <- radsl(cached = TRUE) # nolint
  write.csv(ADSL, file = temp_file_csv, row.names = FALSE)

  # check can pull data and get code without delimiter assigned
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv)
  x$pull()
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \",\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_identical(nrow(data), nrow(ADSL))
  expect_identical(colnames(data), colnames(ADSL))

  # next check can pass arguments to read_delim (e.g. delim = '|')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = "|")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "|")
  x$pull()
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \"|\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_identical(nrow(data), nrow(ADSL))
  expect_identical(ncol(data), ncol(ADSL))
  expect_identical(colnames(data), colnames(ADSL))

  # next check can pass arguments to read_delim (using '\t')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = "\t")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "\t")
  x$pull()
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \"\\t\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_identical(nrow(data), nrow(ADSL))
  expect_identical(ncol(data), ncol(ADSL))
  expect_identical(colnames(data), colnames(ADSL))

  # next check can pass arguments to read_delim (using ';')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = ";")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = ";")
  x$pull()
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \";\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_identical(nrow(data), nrow(ADSL))
  expect_identical(ncol(data), ncol(ADSL))
  expect_identical(colnames(data), colnames(ADSL))
})

# non-standard dataset
test_that("csv_dataset_connector non-standard datasets multi/space character delim", {
  ADSL <- radsl(cached = TRUE) # nolint
  ADSL_ns <- data.frame( # nolint
    STUDYID = "A",
    USUBJID = paste0("A", 1:3),
    SUBJID = 1:3,
    RACE = c("sth1|sth2", "sth", "sth"),
    stringsAsFactors = FALSE
  )

  # next check can pass arguments to read_delim (using '|||')
  write.table(ADSL_ns, file = temp_file_csv, row.names = FALSE, sep = "|||")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "|||")
  expect_warning(x$pull())
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \"|||\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_identical(nrow(data), nrow(ADSL_ns))
  expect_false(identical(colnames(data), colnames(ADSL_ns)))

  # next check can pass arguments to read_delim (using space ' ')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = " ")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, keys = get_cdisc_keys("ADSL"), delim = " ")
  expect_warning(x$pull())
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \" \")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_false(identical(data, ADSL))
})

# column names attributes
test_that("csv_dataset_connector attritubes", {
  ADSL_ns <- data.frame( # nolint
    STUDYID = "A",
    USUBJID = paste0("A", 1:3),
    SUBJID = 1:3,
    RACE = c("sth1|sth2", "sth", "sth"),
    stringsAsFactors = FALSE
  )
  rtables::var_labels(ADSL_ns) <- letters[1:4]
  write.table(ADSL_ns, file = temp_file_csv, row.names = FALSE, sep = ",")

  # check can pull data and get code
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = ",")
  x$pull()
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \",\")"))
  data <- get_raw_data(x)
  expect_null(attributes(data[[1]])$label)

  # we should use mutate_dataset
  data <- get_raw_data(x %>% mutate_dataset("rtables::var_labels(ADSL) <- letters[1:4]"))
  expect_identical(attributes(data[[1]])$label, "a")
})

# test csv_cdisc_dataset_connector
test_that("csv_cdisc_dataset_connector random.cdisc.data", {
  # create csv file
  ADSL <- radsl(cached = TRUE) # nolint
  write.csv(ADSL, file = temp_file_csv, row.names = FALSE)

  # check can pull data and get code without delimiter assigned
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv)
  x$pull()
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \",\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_identical(nrow(data), nrow(ADSL))
  expect_identical(colnames(data), colnames(ADSL))

  # next check can pass arguments to read_delim (e.g. delim = '|')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = "|")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "|")
  x$pull()
  expect_true(is_pulled(x))
  expect_identical(get_dataname(x), "ADSL")
  expect_identical(get_code(x), paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \"|\")"))
  data <- get_raw_data(x)
  expect_true(is.data.frame(data))
  expect_identical(nrow(data), nrow(ADSL))
  expect_identical(ncol(data), ncol(ADSL))
  expect_identical(colnames(data), colnames(ADSL))
})

test_that("script_dataset_connector", {
  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "
    library(random.cdisc.data)
    ADSL <- radsl(cached = TRUE)
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
    ADSL <- radsl(cached = TRUE)
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
    "ADSL <- rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE)"
  )


  x <- mutate_dataset(rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB"), code = "ADLB$x <- 1")
  expect_equal(
    get_code(x),
    "ADLB <- rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE)\nADLB$x <- 1"
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
    x$w <- as.numeric(rnorm(40, 0, 1))
    x$ww <- as.numeric(rnorm(40, 0, 1))
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
    x$w <- as.numeric(rnorm(40, 0, 1))
    x$ww <- as.numeric(rnorm(40, 0, 1))
    rtables::var_labels(x) <- c("STUDYID", "USUBJID", "z", "zz", "NAs", "w", "ww")
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

  expect_identical(get_raw_data(y_1), my_data_1())

  fun_direct <- fun_cdisc_dataset_connector(
    dataname = "ADSL",
    fun = radsl,
    fun_args = list(cached = TRUE)
  )

  fun_direct2 <- fun_cdisc_dataset_connector(
    dataname = "ADSL",
    fun = random.cdisc.data::radsl,
    fun_args = list(cached = TRUE)
  )
  fun_direct$pull()

  fun_direct2$pull()

  data_1 <- get_raw_data(fun_direct)
  data_2 <- get_raw_data(fun_direct2)

  expect_true(is.data.frame(data_1))
  expect_true(is.data.frame(data_2))
  expect_identical(data_1, data_2)
})

test_that("code_dataset_connector - Test various inputs", {
  ADSL <- radsl(cached = TRUE) # nolint

  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c("ADSL <- radsl(cached = TRUE)\nADSL"),
    con = file_example
  )

  from_file <- code_dataset_connector(
    dataname = "ADSL",
    code = paste0(readLines(file_example), collapse = "\n")
  )

  expect_equal(from_file$get_code(), "ADSL <- radsl(cached = TRUE)\nADSL <- ADSL")
  expect_identical(from_file$pull()$get_raw_data(), ADSL)

  ADSL <- radsl(cached = TRUE) # nolint

  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "mtcars
      # code ADSL>
      library(random.cdisc.data)
      ADSL <- radsl(cached = TRUE)
      ADSL
      # <ADSL code
      ADAE <- radae(cached = TRUE)"
    ),
    con = file_example
  )

  get_code_file <- code_dataset_connector(
    dataname = "ADSL",
    code = get_code(file_example, dataname = "ADSL")
  )

  expect_equal(get_code_file$get_code(), "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\nADSL <- ADSL")
  expect_identical(get_code_file$pull()$get_raw_data(), ADSL)

})

test_that("code_dataset_connector - Modify vars", {
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = radsl(cached = TRUE),
    keys = get_cdisc_keys("ADSL"),
    code = "ADSL <- radsl(cached = TRUE)",
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

  expect_silent(adtte$pull(try = TRUE))

  expect_true(
    grepl("Modification of the local variable", adtte$get_error_message())
  )
})

test_that("code_dataset_connector - library calls", {
  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(radsl) %>% set_args(args = list(cached = TRUE)),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL dataset"
  )

  adtte <- dataset_connector(
    dataname = "ADTTE",
    pull_callable = callable_code(
      "library(dplyr)
      radtte(cached = TRUE) %>%
        filter(SEX == 'F')"
    ),
    keys = get_cdisc_keys("ADTTE"),
    label = "ADTTE dataset"
  )

  adrs <- dataset_connector(
    dataname = "ADRS",
    pull_callable = callable_code(
      "library(dplyr)
      radrs(cached = TRUE) %>%
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
    get_raw_data(datasets[[1]]),
    radsl(cached = TRUE)
  )

  expect_identical(
    unique(get_raw_data(datasets[[2]])$SEX),
    factor("F", levels = c("F", "M"))
  )

  expect_identical(
    unique(get_raw_data(datasets[[3]])$SEX),
    factor("F", levels = c("F", "M"))
  )
})

testthat::test_that("DatasetConnector mutate method with delayed logic", {
  test_ds0 <- Dataset$new("head_mtcars", head(mtcars))
  test_ds1 <- Dataset$new("head_iris", head(iris))
  test_ds2 <- Dataset$new("head_rock", head(rock))

  pull_fun2 <- callable_function(data.frame)
  pull_fun2$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun2, vars = list(test_ds1 = test_ds1))
})
