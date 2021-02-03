library(random.cdisc.data)

ADSL <- radsl(cached = TRUE) # nolint

test_that("Objects can be generated from the code", {
  y_code <- callable_code("library(random.cdisc.data); ADSL <- radsl(cached = TRUE)\nADSL")
  expect_equal(y_code$get_call(), c("library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\nADSL"))
  expect_identical(y_code$run(), ADSL)

  y2_code <- callable_code("library(random.cdisc.data)\n\nADSL <- radsl(cached = TRUE)\nADSL")
  # we losing one \n character - code is parsed then joind with \n
  expect_equal(y2_code$get_call(), c("library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\nADSL"))
  expect_identical(y2_code$run(), ADSL)

  z_code <- callable_code("ADSL <- radsl(cached = TRUE)\nADSL")
  expect_equal(z_code$get_call(), c("ADSL <- radsl(cached = TRUE)\nADSL"))
  expect_identical(z_code$run(), ADSL)

  a_code <- callable_code("library(dplyr); starwars")
  expect_message(a_code$run(return = FALSE))
  expect_s3_class(a_code$run(), "data.frame")

  x <- code_dataset_connector(
    dataname = "ADSL",
    keys = get_cdisc_keys("ADSL"),
    code = "x <- radsl(cached = TRUE); x"
  )

  expect_equal(x$get_code(), "x <- radsl(cached = TRUE)\nADSL <- x")

  # direct usage of function from package
  y_code <- callable_code("ADSL <- random.cdisc.data::radsl(cached = TRUE)\nADSL")
  expect_equal(y_code$get_call(), c("ADSL <- random.cdisc.data::radsl(cached = TRUE)\nADSL"))
  expect_identical(y_code$run(), ADSL)

})

test_that("Connector objects contain the incorrect code", {
  x_code <- callable_code("ADSL$new <- 1; ADSL")
  expect_error(x_code$run(), "object 'ADSL' not found")
  expect_error(callable_code("'"), "Code supplied is not valid")
  expect_error(callable_code(""), "Code supplied is not valid")
  # double ;;
  expect_error(
    callable_code("library(random.cdisc.data);; ADSL <- radsl(cached = TRUE)\nADSL;"),
    "Code supplied is not valid"
  )
  # we have to use newline or ; to separate the code lines
  expect_error(
    callable_code("library(random.cdisc.data) ADSL <- radsl(cached = TRUE)\nADSL"),
    "Code supplied is not valid"
  )
})
