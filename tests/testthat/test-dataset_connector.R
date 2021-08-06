library(random.cdisc.data)
# test csv_dataset_connector
temp_file_csv <- tempfile(fileext = ".csv")
on.exit(unlink(temp_file_csv))

# Test DatasetConnector ------
testthat::test_that("DatasetConnector", {
  fun <- callable_function(radsl)
  fun$set_args(list(N = 5, seed = 1, cached = TRUE))

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
    "ADSL <- radsl(N = 5, seed = 1, cached = TRUE)"
  )

  testthat::expect_identical(
    x1$get_code(deparse = FALSE),
    as.list(as.call(parse(text = "ADSL <- radsl(N = 5, seed = 1, cached = TRUE)")))
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
    is(x1$get_dataset(), "Dataset")
  )

  testthat::expect_identical(
    get_keys(get_dataset(x1)),
    get_keys(x1)
  )

  testthat::expect_identical(
    get_raw_data(x1),
    radsl(N = 5, seed = 1, cached = TRUE)
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
    get_code(x3),
    "ADSL <- data.frame(id = 1:3, marker = c(100, 1, 10), alive = TRUE, new_feature = c(3, 4, 1))"
  )

  m <- mutate_dataset(x3, "ADSL$newest <- 'xxx'")

  testthat::expect_silent(load_dataset(m))

  testthat::expect_silent(
    m <- mutate_dataset(x3, "ADSL$newest2 <- 'best'")
  )

  testthat::expect_true(
    is(get_dataset(m), "Dataset")
  )

  testthat::expect_identical(
    get_raw_data(m),
    data.frame(
      id = 1:3, marker = c(100, 1, 10), alive = TRUE, new_feature = c(3, 4, 1),
      newest = "xxx", newest2 = "best", stringsAsFactors = FALSE
    )
  )
})

# Test conversions
testthat::test_that("rcd_dataset_connector", {
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
  testthat::expect_equal(x, x2)
  expect_true(is(x, c("DatasetConnector", "R6")))

  testthat::expect_identical(
    x$.__enclos_env__$private$pull_callable$.__enclos_env__$private$fun_name,
    "radsl"
  )

  testthat::expect_identical(
    x$get_dataname(),
    "ADSL"
  )

  testthat::expect_equal(
    x$get_code(),
    "ADSL <- radsl(cached = TRUE)"
  )

  testthat::expect_silent(
    load_dataset(x)
  )

  testthat::expect_identical(
    x$get_raw_data(),
    radsl(cached = TRUE, seed = 1, N = 400)
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
  testthat::expect_true(is(x, c("DatasetConnector", "R6")))

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

  # check error if is_character_single(file)
  testthat::expect_error(
    csv_dataset_connector("ADSL", file = c("a", "b"))
  )
  testthat::expect_error(
    csv_dataset_connector("ADSL", file = 1)
  )
})

# test with cdisc data input
testthat::test_that("csv_dataset_connector random.cdisc.data", {
  # create csv file
  ADSL <- radsl(cached = TRUE) # nolint
  write.csv(ADSL, file = temp_file_csv, row.names = FALSE)

  # check can pull data and get code without delimiter assigned
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv)
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x),
    paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \",\")")
  )
  data <- get_raw_data(x)
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(ADSL))
  testthat::expect_identical(colnames(data), colnames(ADSL))

  # next check can pass arguments to read_delim (e.g. delim = '|')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = "|")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "|")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x),
    paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \"|\")")
  )
  data <- get_raw_data(x)
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(ADSL))
  testthat::expect_identical(ncol(data), ncol(ADSL))
  testthat::expect_identical(colnames(data), colnames(ADSL))

  # next check can pass arguments to read_delim (using '\t')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = "\t")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "\t")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x),
    paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \"\\t\")")
  )
  data <- get_raw_data(x)
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(ADSL))
  testthat::expect_identical(ncol(data), ncol(ADSL))
  testthat::expect_identical(colnames(data), colnames(ADSL))

  # next check can pass arguments to read_delim (using ';')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = ";")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = ";")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x),
    paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \";\")")
  )
  data <- get_raw_data(x)
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(ADSL))
  testthat::expect_identical(ncol(data), ncol(ADSL))
  testthat::expect_identical(colnames(data), colnames(ADSL))
})

# non-standard dataset
testthat::test_that("csv_dataset_connector non-standard datasets multi/space character delim", {
  test_adsl <- radsl(cached = TRUE)
  test_adsl_ns <- data.frame(
    STUDYID = "A",
    USUBJID = paste0("A", 1:3),
    SUBJID = 1:3,
    RACE = c("sth1|sth2", "sth", "sth"),
    stringsAsFactors = FALSE
  )

  # next check can pass arguments to read_delim (using '|||')
  write.table(test_adsl_ns, file = temp_file_csv, row.names = FALSE, sep = "|||")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "|||")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x),
    paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \"|||\")")
  )
  data <- get_raw_data(x)
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(test_adsl_ns))
  testthat::expect_equal(colnames(x$get_raw_data()), colnames(test_adsl_ns))

  # next check can pass arguments to read_delim (using space ' ')
  write.table(test_adsl, file = temp_file_csv, row.names = FALSE, sep = " ")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, keys = get_cdisc_keys("ADSL"), delim = " ")
  testthat::expect_warning(x$pull())
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x), paste0("ADSL <- readr::read_delim(file = \"",
    temp_file_csv, "\", delim = \" \")")
  )
  data <- get_raw_data(x)
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
  rtables::var_labels(ADSL_ns) <- letters[1:4]
  write.table(ADSL_ns, file = temp_file_csv, row.names = FALSE, sep = ",")

  # check can pull data and get code
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = ",")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x),
    paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \",\")")
  )
  data <- get_raw_data(x)
  testthat::expect_null(attributes(data[[1]])$label)

  # we should use mutate_dataset
  data <- get_raw_data(x %>% mutate_dataset("rtables::var_labels(ADSL) <- letters[1:4]"))
  testthat::expect_identical(attributes(data[[1]])$label, "a")
})

# test csv_cdisc_dataset_connector
testthat::test_that("csv_cdisc_dataset_connector random.cdisc.data", {
  # create csv file
  ADSL <- radsl(cached = TRUE) # nolint
  write.csv(ADSL, file = temp_file_csv, row.names = FALSE)

  # check can pull data and get code without delimiter assigned
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv)
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x),
    paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \",\")")
  )
  data <- get_raw_data(x)
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(ADSL))
  testthat::expect_identical(colnames(data), colnames(ADSL))

  # next check can pass arguments to read_delim (e.g. delim = '|')
  write.table(ADSL, file = temp_file_csv, row.names = FALSE, sep = "|")
  x <- csv_cdisc_dataset_connector("ADSL", file = temp_file_csv, delim = "|")
  x$pull()
  testthat::expect_true(is_pulled(x))
  testthat::expect_identical(get_dataname(x), "ADSL")
  testthat::expect_identical(
    get_code(x),
    paste0("ADSL <- readr::read_delim(file = \"", temp_file_csv, "\", delim = \"|\")")
  )
  data <- get_raw_data(x)
  testthat::expect_true(is.data.frame(data))
  testthat::expect_identical(nrow(data), nrow(ADSL))
  testthat::expect_identical(ncol(data), ncol(ADSL))
  testthat::expect_identical(colnames(data), colnames(ADSL))
})

testthat::test_that("script_dataset_connector", {
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
  testthat::expect_error(
    script_dataset_connector(
      dataname = "ADSL",
      file = wrong_file,
      keys = get_cdisc_keys("ADSL")
    ),
    sprintf("File %s does not exist.", wrong_file)
  )

  testthat::expect_silent(load_dataset(x))

  testthat::expect_true(is(get_dataset(x), c("RelationalDataset", "R6")))

  testthat::expect_true(is(get_raw_data(x), c("data.frame")))
})

testthat::test_that("script_cdisc_dataset_connector", {
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

  testthat::expect_silent(load_dataset(x))

  testthat::expect_true(is(get_dataset(x), c("RelationalDataset", "R6")))

  testthat::expect_true(is(get_raw_data(x), c("data.frame")))
})

test_that("rice_dataset", {
  if (!"rice" %in% installed.packages()) {
    testthat::skip("rice package not available")
  }
  x <- rice_data(
    rice_dataset_connector("ADSL", "/path/to/ADSL", keys = get_cdisc_keys("ADSL")),
    rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
  )

  testthat::expect_equal(
    x$get_items()[[1]],
    rice_dataset_connector("ADSL", "/path/to/ADSL", keys = get_cdisc_keys("ADSL"))
  )
  testthat::expect_equal(
    x$get_items()[[2]],
    rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
  )

  testthat::expect_identical(
    x$get_items()[[1]]$get_code(),
    "ADSL <- rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE)"
  )


  x <- rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
  mutate_dataset(x, code = "ADLB$x <- 1")

  testthat::expect_equal(get_code(x), "ADLB <- rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE)\nADLB$x <- 1")
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

testthat::test_that("code_dataset_connector - Test various inputs", {
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

testthat::test_that("code_dataset_connector - Modify vars", {
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

testthat::test_that("code_dataset_connector - library calls", {
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
  test_ds1 <- Dataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  test_ds2 <- Dataset$new("head_iris", head(iris), code = "head_iris <- head(iris)")
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
    c("head_mtcars <- head(mtcars)",
      "test_ds1 <- head_mtcars",
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "test_dc$tail_letters <- tail(letters)"
    )
  )
  testthat::expect_false(t_dc$is_pulled())
  load_dataset(t_dc)
  testthat::expect_false(t_dc$is_mutate_delayed())
  testthat::expect_true(all(c("head_letters", "tail_letters") %in% names(get_raw_data(t_dc))))

  testthat::expect_equal(
    pretty_code_string(t_dc$get_code()),
    c("head_mtcars <- head(mtcars)",
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
    c("head_mtcars <- head(mtcars)",
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
    c("head_mtcars <- head(mtcars)",
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
  testthat::expect_true(all(c("head_letters", "tail_letters", "head_integers", "one") %in% names(get_raw_data(t_dc))))
  testthat::expect_false(t_dc$is_mutate_delayed())

  # mutate should again be eager
  mutate_dataset(t_dc2, code = "test_dc2$five <- 5")
  testthat::expect_equal(get_raw_data(t_dc2)$five, rep(5, 6))

  mutate_dataset(t_dc, code = "test_dc$five <- t_dc2$five", vars = list(t_dc2 = t_dc2))
  testthat::expect_equal(get_raw_data(t_dc)$five, rep(5, 6))
  testthat::expect_false(t_dc$is_mutate_delayed())

  # multiple lines of identical code
  mutate_dataset(t_dc, code = "test_dc$five <- 2 * test_dc$five")
  mutate_dataset(t_dc, code = "test_dc$five <- 2 * test_dc$five")
  mutate_dataset(t_dc, code = "test_dc$five <- 2 * test_dc$five")
  testthat::expect_equal(get_raw_data(t_dc)$five, rep(40, 6))
  testthat::expect_false(t_dc$is_mutate_delayed())

  # multi layer dependencies
  pull_fun3 <- callable_function(data.frame)
  pull_fun3$set_args(args = list(neg_integers = - (1:6)))
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
  testthat::expect_silent(get_raw_data(t_dc))
  load_dataset(t_dc3)
  testthat::expect_false(any(c("six", "seven") %in% names(get_raw_data(t_dc))))

  # current state
  testthat::expect_true(all(
    names(get_raw_data(t_dc)) %in% c("head_letters", "tail_letters", "head_integers", "one", "five"))
  )

  # load_dataset, which calls pull method, will reset to original state because dependencies have changed
  load_dataset(t_dc)

  testthat::expect_true(t_dc$is_mutate_delayed())
  # original state. all columns resulting from mutations have been removed
  testthat::expect_true(all(names(get_raw_data(t_dc)) %in% c("head_letters")))
  # still it must return code from all previously inputted mutate statements
  testthat::expect_true(
    "test_dc$seven <- 7" %in% pretty_code_string(t_dc$get_code()),
  )

  # confirming that mutation has not happened
  testthat::expect_false(any(c("six", "seven") %in% names(get_raw_data(t_dc))))
  # confirming that mutation is delayed
  testthat::expect_true(t_dc2$is_mutate_delayed())

  # confirming get_raw_data will eager mutate t_dc2 because t_dc3 has been loaded
  testthat::expect_true(all(c("head_integers", "five", "neg_integers") %in% names(get_raw_data(t_dc2))))

  # re running all mutation statements
  load_dataset(t_dc)
  testthat::expect_false(t_dc$is_mutate_delayed())
  testthat::expect_true(all(c(
    "head_integers", "tail_letters", "head_integers", "one", "five", "six", "seven") %in% names(get_raw_data(t_dc)))
  )

  testthat::expect_equal(get_raw_data(t_dc)$seven, rep(7, 6))
  testthat::expect_equal(get_raw_data(t_dc)$six, rep(41, 6))
  testthat::expect_equal(get_raw_data(t_dc)$five, rep(40, 6))
  # back to eager mutate
  mutate_dataset(t_dc, code = "test_dc$eight <- 8")
  testthat::expect_equal(get_raw_data(t_dc)$eight, rep(8, 6))
})

testthat::test_that("DatasetConnector mutate method edge cases", {
  # edge because test_ds1 does not contain the code to recreate head_mtcars
  test_ds1 <- Dataset$new("head_mtcars", head(mtcars))

  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun)
  load_dataset(t_dc)
  testthat::expect_silent(
    mutate_dataset(t_dc, code = "test_dc$new_var <- head_mtcars$carb", vars = list(head_mtcars = test_ds1))
  )
  testthat::expect_equal(get_raw_data(t_dc)$new_var, c(4, 4, 1, 1, 2, 1))
})

testthat::test_that("get_code_class returns the correct CodeClass object", {
  cc1 <- CodeClass$new(code = "iris <- (function() head(iris))()", dataname = "iris")
  cf1 <- CallableFunction$new(function() head(iris))
  dc1 <- DatasetConnector$new("iris", cf1)
  testthat::expect_equal(dc1$get_code_class(), cc1)
})

testthat::test_that("Pulled DatasetConnector returns the same CodeClass as before pulling", {
  cf1 <- CallableFunction$new(function() head(iris))
  dc1 <- DatasetConnector$new("iris", cf1)
  pre_pull_cc <- dc1$get_code_class()
  dc1$pull()
  post_pull_cc <- dc1$get_code_class()

  testthat::expect_equal(post_pull_cc, pre_pull_cc)
})

testthat::test_that("Pulled dependent DatasetConnector returns the same CodeClass as before pulling", {
  ds <- Dataset$new("iris", head(iris), code = "iris <- head(iris)")
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf, vars = list(iris = ds))
  pre_pull_code_class <- dc$get_code_class()
  dc$pull()
  post_pull_code_class <- dc$get_code_class()
  testthat::expect_equal(post_pull_code_class, pre_pull_code_class)
})

testthat::test_that("Pulling twice doesn't change the returned DatasetConnector's CodeClass", {
  ds <- Dataset$new("iris", head(iris), code = "iris <- head(iris)")
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf, vars = list(iris = ds))
  dc$pull()
  pre_pull_code_class <- dc$get_code_class()
  dc$pull()
  post_pull_code_class <- dc$get_code_class()
  testthat::expect_equal(post_pull_code_class, pre_pull_code_class)
})

testthat::test_that("Identical mutation expressions are added to the mutation code", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$mutate("mtcars$test <- 1")
  dc$mutate("mtcars$test <- 1")
  testthat::expect_equal(dc$get_code(), "mtcars <- (function() head(mtcars))()\nmtcars$test <- 1\nmtcars$test <- 1")
})

testthat::test_that("Identical mutation expressions are executed upon pulling the Connector object", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$mutate("mtcars$test <- 1")
  dc$mutate("mtcars$test <- mtcars$test * 2")
  dc$mutate("mtcars$test <- mtcars$test * 2")
  dc$pull()
  testthat::expect_equal(dc$get_raw_data()$test, rep(4, 6))
})

testthat::test_that("Identical mutation expressions are shown in the returned code after pulling", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
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

testthat::test_that("DatasetConnector$is_mutate_delayed is FALSE if not yet pulled and not mutated", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  testthat::expect_false(dc$is_mutate_delayed())
})

testthat::test_that("DatasetConnector$is_mutate_delayed returns FALSE if pulled and not mutated", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$pull()
  testthat::expect_false(dc$is_mutate_delayed())
})

testthat::test_that("DatasetConnector$is_mutate_delayed returns TRUE if not pulled and mutated", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$mutate(code = "test")
  testthat::expect_true(dc$is_mutate_delayed())
})

testthat::test_that("DatasetConnector$is_mutate_delayed returns TRUE if mutated with no delayed objects and pulled", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$mutate(code = "")
  dc$pull()
  testthat::expect_false(dc$is_mutate_delayed())
})

testthat::test_that("DatasetConnector$is_mutate_delayed returns FALSE if mutated with no vars after pulling", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$pull()
  dc$mutate(code = "")
  testthat::expect_false(dc$is_mutate_delayed())
})

testthat::test_that("DatasetConnector returns the correct code when mutated with no vars after pulling", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$pull()
  dc$mutate(code = "1")
  testthat::expect_equal(dc$get_code_class()$get_code(), "mtcars <- (function() head(mtcars))()\n1")
})

testthat::test_that("Pulling an already pulled DatasetConnector after mutating it with a delayed object
  undoes any eager pre-pull mutations", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$pull()
  dc$mutate(code = "mtcars[1] <- NULL")
  dc$mutate(code = "", vars = list(delayed = DatasetConnector$new("iris", CallableFunction$new(function() head(iris)))))
  dc$pull()
  testthat::expect_equal(dc$get_raw_data(), head(mtcars))
})

testthat::test_that("Pulling an already pulled DatasetConnector after mutating it with a delayed object
  does not change the returned code", {
  cf <- CallableFunction$new(function() head(mtcars))
  dc <- DatasetConnector$new("mtcars", cf)
  dc$pull()
  dc$mutate(code = "mtcars[1] <- NULL")
  dc$mutate(code = "", vars = list(delayed = DatasetConnector$new("iris", CallableFunction$new(function() head(iris)))))
  pre_pull_code <- dc$get_code()
  dc$pull()
  testthat::expect_equal(dc$get_code(), pre_pull_code)
})
