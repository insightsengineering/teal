testthat::test_that("Objects can be generated from the code", {
  y_code <- callable_code("7")
  testthat::expect_equal(y_code$get_call(), c("7"))
  testthat::expect_identical(y_code$run(), 7)

  # we are losing one \n character - code is parsed then joind with \n
  y2_code <- callable_code("13\n\n7")
  testthat::expect_equal(y2_code$get_call(), c("13\n7"))
  testthat::expect_identical(y2_code$run(), 7)

  a_code <- callable_code("library(dplyr); starwars")
  testthat::expect_s3_class(a_code$run(), "data.frame")

  # direct usage of function from package
  y_code <- callable_code("datasets::iris")
  expect_equal(y_code$get_call(), c("datasets::iris"))
  expect_identical(y_code$run(), datasets::iris)
})

test_that("Connector objects contain the incorrect code", {
  x_code <- callable_code("ADSL$new <- 1; ADSL")
  expect_error(x_code$run(), "object 'ADSL' not found")
  expect_error(callable_code("'"), "Code supplied is not valid")
  expect_error(callable_code(""), "Code supplied is not valid")
  # double ;;
  expect_error(
    callable_code("library(scda);; ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nADSL;"),
    "Code supplied is not valid"
  )
  # we have to use newline or ; to separate the code lines
  expect_error(
    callable_code("library(scda) ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nADSL"),
    "Code supplied is not valid"
  )
})
