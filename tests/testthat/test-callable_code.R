context("CallableCode")

library(random.cdisc.data)

test_that("Objects can be generated from the code", {
  ADSL <- radsl(cached = TRUE) # nolint
  x_code <- callable_code("ADSL$new <- 1; ADSL")

  expect_error(x_code$run(), "object 'ADSL' not found")

  y_code <- callable_code("library(random.cdisc.data); ADSL <- radsl(cached = TRUE)\nADSL")
  expect_equal(y_code$get_call(), c("library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\nADSL"))
  expect_identical(y_code$run(), ADSL)

  z_code <- callable_code("ADSL <- radsl(cached = TRUE)\nADSL")
  expect_equal(z_code$get_call(), c("ADSL <- radsl(cached = TRUE)\nADSL"))
  expect_identical(z_code$run(), ADSL)

  a_code <- callable_code("library(dplyr); starwars")
  expect_message(a_code$run(return = FALSE))
  expect_s3_class(a_code$run(), "data.frame")

  expect_error(callable_code("'"), "Code supplied is not valid")
})

test_that("Connector objects contain the correct code", {
  library(random.cdisc.data)
  x <- code_dataset_connector(
    dataname = "ADSL",
    keys = get_cdisc_keys("ADSL"),
    code = "x <- radsl(cached = TRUE); x"
  )

  expect_equal(x$get_code(), "x <- radsl(cached = TRUE)\nADSL <- x")
})

test_that("Test various inputs", {
  ADSL <- radsl(seed = 1) # nolint
  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c("seed <- 1; ADSL <- radsl(seed = seed)\nADSL"),
    con = file_example
  )

  from_file <- code_dataset_connector(
    dataname = "ADSL",
    keys = get_cdisc_keys("ADSL"),
    code = paste0(readLines(file_example), collapse = "\n")
  )

  expect_equal(from_file$get_code(), "seed <- 1\nADSL <- radsl(seed = seed)\nADSL <- ADSL")
  expect_identical(from_file$pull()$get_raw_data(), ADSL)

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
    keys = get_cdisc_keys("ADSL"),
    code = get_code(file_example, dataname = "ADSL")
  )
})

test_that("Modify vars", {
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    data = radsl(cached = TRUE),
    keys = get_cdisc_keys("ADSL"),
    code = "ADSL <- radsl(cached = TRUE)",
    label = "ADSL dataset"
  )

  adtte <- relational_dataset_connector(
    dataname = "ADTTE",
    pull_callable = callable_code(
      "ADSL <- dplyr::filter(ADSL, SEX == 'F')
      radtte(
        ADSL = ADSL,
        seed = 1
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

test_that("library calls", {
  adsl <- relational_dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(radsl) %>% set_args(args = list(cached = TRUE)),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL dataset"
  )

  adtte <- relational_dataset_connector(
    dataname = "ADTTE",
    pull_callable = callable_code(
      "library(dplyr)
      ADSL_local <- filter(ADSL, SEX == 'F')
      radtte(
        ADSL = ADSL_local,
        seed = 1
      )"
    ),
    keys = get_cdisc_keys("ADTTE"),
    label = "ADTTE dataset",
    vars = list(ADSL = adsl)
  )

  adrs <- relational_dataset_connector(
    dataname = "ADRS",
    pull_callable = callable_code(
      "library(dplyr)
      radrs(
        ADSL = filter(ADSL, SEX == 'F'),
        seed = 1
      )"
      ),
      keys = get_cdisc_keys("ADRS"),
      label = "ADRS dataset",
      vars = list(ADSL = adsl)
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
