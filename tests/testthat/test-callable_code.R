context("CallableCode")

library(random.cdisc.data)

test_that("Objects can be generated from the code", {
  ADSL <- radsl(cached = TRUE) # nolint
  x_code <- callable_code("ADSL$new <- 1; ADSL")

  expect_error(x_code$run(),
               "object 'ADSL' not found")

  y_code <- callable_code("library(random.cdisc.data); ADSL <- radsl(cached = TRUE)\nADSL")
  expect_equal(y_code$get_call(),
               c("library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\nADSL"))
  expect_identical(y_code$run(), ADSL)

  z_code <- callable_code("ADSL <- radsl(cached = TRUE)\nADSL")
  expect_equal(z_code$get_call(),
               c("ADSL <- radsl(cached = TRUE)\nADSL"))
  expect_identical(z_code$run(), ADSL)

  a_code <- callable_code("library(dplyr); starwars")
  expect_message(a_code$run(return = FALSE))
  expect_s3_class(a_code$run(), "data.frame")

  expect_error(callable_code("'"),
               "Code supplied is not valid")
})

test_that("Connector objects contain the correct code", {
  library(random.cdisc.data)
  x <- code_dataset_connector(
    dataname = "ADSL",
    keys = get_cdisc_keys("ADSL"),
    code = "x <- radsl(cached = TRUE); x"
  )

  expect_equal(x$get_code(),
               "x <- radsl(cached = TRUE)\nADSL <- x")
})

test_that("Test various inputs", {
  ADSL <- radsl(seed = 1) # nolint
  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "seed <- 1; ADSL <- radsl(seed = seed)\nADSL"
    ),
    con = file_example
  )

  from_file <- code_dataset_connector(
    dataname = "ADSL",
    keys = get_cdisc_keys("ADSL"),
    code = paste0(readLines(file_example), collapse = "\n")
  )

  expect_equal(from_file$get_code(),
               "seed <- 1\nADSL <- radsl(seed = seed)\nADSL <- ADSL")
  expect_identical(from_file$pull()$get_raw_data(),
                   ADSL)

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
