testthat::test_that("Objects can be generated from the code", {
  y_code <- callable_code("7")
  testthat::expect_equal(y_code$get_call(), c("7"))
  testthat::expect_identical(y_code$run(), 7)

  a_code <- callable_code("library(dplyr); starwars")
  testthat::expect_s3_class(a_code$run(), "data.frame")
})

testthat::test_that("CallableCode can use objects from namespaces other than global", {
  # direct usage of function from package
  y_code <- callable_code("datasets::iris")
  expect_equal(y_code$get_call(), c("datasets::iris"))
  expect_identical(y_code$run(), datasets::iris)
})

testthat::test_that("get_call transforms double new lines into one new line", {
  y2_code <- callable_code("13\n\n7")
  testthat::expect_equal(y2_code$get_call(), c("13\n7"))
  testthat::expect_identical(y2_code$run(), 7)
})

testthat::test_that("callable_code throws an error when supplied code is not valid", {
  testthat::expect_error(callable_code("'"), "Code supplied is not valid")
  testthat::expect_error(callable_code(""), "Code supplied is not valid")
  # double ;;
  testthat::expect_error(
    callable_code("library(scda);; ADSL <- synthetic_cdisc_data(\"latest\")$adsl\nADSL;"),
    "Code supplied is not valid"
  )
  # we have to use newline or ; to separate the code lines
  testthat::expect_error(
    callable_code("library(scda) ADSL <- synthetic_cdisc_data(\"latest\")$adsl\nADSL"),
    "Code supplied is not valid"
  )
})

testthat::test_that("run throws an error when an object referenced in the code is not found", {
  x_code <- callable_code("ADSL$new <- 1; ADSL")
  testthat::expect_error(x_code$run(), "object 'ADSL' not found")
})
