context("cdisc_data")

package_path <- path.package("teal")
if ("inst" %in% list.dirs(package_path, full.names = F, recursive = F)) {
  filename <- file.path(package_path, "inst", "preprocessing_empty_string.txt")
} else {
  filename <- file.path(package_path, "preprocessing_empty_string.txt")
}
code_empty <- readChar(filename, file.info(filename)$size)

library(tern)
library(random.cdisc.data)
ASL <- ARG1 <- ARG2 <- cadsl # nolint
ADTE <- cadtte # nolint
ADRS <- cadrs # nolint

test_that("Basic example - without code and check", {
  expect_silent(cdisc_data(ASL, code = "", check = FALSE))
  expect_silent(cdisc_data(ASL, ARG1 = ARG1, ARG2 = ARG2, code = "", check = FALSE))
})

test_that("Basic example - with code and check", {
  expect_true(is.data.frame(ASL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))
  keys(ASL) <- keys(ARG1) <- keys(ARG2) <- c("STUDYID", "USUBJID")

  expect_silent(cdisc_data(ASL = ASL, code = "ASL <- cadsl; keys(ASL) <- c('STUDYID','USUBJID')", check = TRUE))
  expect_silent(cdisc_data(
    ASL = ASL, ARG1 = ARG1, ARG2 = ARG2,
    code = "ASL <- ARG1 <- ARG2 <- cadsl; keys(ASL) <- keys(ARG1) <- keys(ARG2) <- c('STUDYID','USUBJID')",
    check = TRUE
  ))
})

test_that("Basic example - with vector code and check", {
  expect_true(is.data.frame(ASL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))
  keys(ASL) <- keys(ARG1) <- keys(ARG2) <- c("STUDYID", "USUBJID")

  expect_silent(cdisc_data(ASL = ASL, code = c("ASL <- cadsl", "keys(ASL) <- c('STUDYID','USUBJID')"), check = TRUE))
  expect_silent(cdisc_data(
    ASL = ASL, ARG1 = ARG1, ARG2 = ARG2,
    code = c("ASL <- ARG1 <- ARG2 <- cadsl", "keys(ASL) <- keys(ARG1) <- keys(ARG2) <- c('STUDYID','USUBJID')"),
    check = TRUE
  ))
})

test_that("Basic example - with line break code and check", {
  expect_true(is.data.frame(ASL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))
  keys(ASL) <- keys(ARG1) <- keys(ARG2) <- c("STUDYID", "USUBJID")

  expect_silent(cdisc_data(ASL = ASL, code = "ASL <- cadsl\nkeys(ASL) <- c('STUDYID','USUBJID')", check = TRUE))
  expect_silent(cdisc_data(
    ASL = ASL, ARG1 = ARG1, ARG2 = ARG2,
    code = "ASL <- ARG1 <- ARG2 <- cadsl\nkeys(ASL) <- keys(ARG1) <- keys(ARG2) <- c('STUDYID','USUBJID')",
    check = TRUE
  ))
})

test_that("Naming list elements", {
  keys(ADTE) <- c("STUDYID", "USUBJID", "PARAMCD", "ARMCD")
  keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")

  expect_identical(names(cdisc_data(ASL)), "ASL")
  expect_identical(names(cdisc_data(ASL = ASL, ADTE = ADTE, ADRS = ADRS)), c("ASL", "ADTE", "ADRS"))
})

test_that("List values", {
  keys(ASL) <- c("STUDYID", "USUBJID")
  keys(ADTE) <- c("STUDYID", "USUBJID", "PARAMCD", "ARMCD")
  keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")

  result <- cdisc_data(ASL)

  result_to_compare <- list(ASL = cadsl)
  class(result_to_compare) <- "cdisc_data"
  keys(result_to_compare[["ASL"]]) <- c("STUDYID", "USUBJID")
  attr(result_to_compare[["ASL"]], "dataname") <- "ASL"

  attr(result_to_compare, "code") <- code_empty

  expect_identical(result, result_to_compare)

  result <- cdisc_data(ASL, ADTE = ADTE, ADRS = ADRS)

  result_to_compare <- list(ASL = cadsl, ADTE = cadtte, ADRS = cadrs)
  class(result_to_compare) <- "cdisc_data"
  keys(result_to_compare[["ASL"]]) <- c("STUDYID", "USUBJID")
  keys(result_to_compare[["ADTE"]]) <- c("STUDYID", "USUBJID", "PARAMCD", "ARMCD")
  keys(result_to_compare[["ADRS"]]) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
  attr(result_to_compare[["ASL"]], "dataname") <- "ASL"
  attr(result_to_compare[["ADTE"]], "dataname") <- "ADTE"
  attr(result_to_compare[["ADRS"]], "dataname") <- "ADRS"
  attr(result_to_compare, "code") <- code_empty

  expect_identical(result, result_to_compare)
})

test_that("Empty code", {
  keys(ASL) <- c("STUDYID", "USUBJID")

  # missing code
  result <- cdisc_data(ASL, check = FALSE)
  expect_identical(attr(result, "code"), code_empty)

  # NULL code
  result <- cdisc_data(ASL, code = "", check = FALSE)
  expect_identical(attr(result, "code"), code_empty)

  # empty code
  result <- cdisc_data(ASL, code = "", check = FALSE)
  expect_identical(attr(result, "code"), code_empty)
})


test_that("Arguments created by code", {
  keys(ASL) <- c("STUDYID", "USUBJID")
  result <- cdisc_data(ASL, code = "ASL <- cadsl; keys(ASL) <- 'STUDYID'", check = FALSE)
  expect_silent(result)

  result_to_compare <- list(ASL = cadsl)
  class(result_to_compare) <- "cdisc_data"
  keys(result_to_compare[["ASL"]]) <- c("STUDYID", "USUBJID")
  attr(result_to_compare[["ASL"]], "dataname") <- "ASL"
  attr(result_to_compare, "code") <- "ASL <- cadsl; keys(ASL) <- 'STUDYID'"

  expect_identical(result, result_to_compare)
})

test_that("Error - objects differs", {
  expect_error(
    cdisc_data(ASL, code = "ASL <- 2", check = TRUE),
    "Cannot reproduce object"
  )

  keys(ASL) <- c("STUDYID", "USUBJID")
  expect_error(
    cdisc_data(ASL, code = "ASL <- cadsl; keys(ASL) <- c('USUBJID', 'STUDYID', 'AGE')", check = TRUE),
    "Cannot reproduce object"
  )
})

test_that("Error - ASL is missing", {
  expect_error(cdisc_data(ARG1 = 1, code = "", check = FALSE), "ASL and code arguments are missing")
  expect_error(cdisc_data(code = "x <- 2", check = FALSE), "ASL is missing and cannot be generated by code")
})

test_that("Error - checking is forbidden if any argument is call", {
  expect_error(
    cdisc_data(1 + 2, code = "test code", check = TRUE),
    "Automatic checking is not supported if arguments provided as calls"
  )

  keys(ASL) <- c("STUDYID", "USUBJID")

  expect_error(
    cdisc_data(1 + 2, code = "test code", check = TRUE),
    "Automatic checking is not supported if arguments provided as calls"
  )


  expect_error(
    cdisc_data(foo(1), code = "test code", check = TRUE),
    "Automatic checking is not supported if arguments provided as calls"
  )
})

test_that("Error - not named arguments", {
  expect_error(
    cdisc_data(ASL, ASL, code = "", check = FALSE),
    "All arguments passed to '...' should be named"
  )
  expect_error(
    cdisc_data(ADTE, ADTE, code = "ADTE <- 1", check = FALSE),
    "All arguments passed to '...' should be named"
  )
})


test_that("Error - data names can not be changed via arguments", {
  asl <- cadsl
  adte <- cadtte
  expect_error(
    cdisc_data(ASL = asl, code = "", check = FALSE),
    "Data names should not be changed via argument\nASL != asl"
  )

  expect_error(
    cdisc_data(ASL = asl, ADTE = adte, code = "", check = FALSE),
    "Data names should not be changed via argument\nASL != asl\nADTE != adte"
  )
})

test_that("Error - Data arguments should be capitalized.", {
  ADRS <- cadrs # nolint
  adte <- cadtte

  expect_error(
    cdisc_data(ASL = ASL, adte = adte, code = "", check = FALSE),
    "Data arguments should be capitalized. Please change"
  )

  expect_error(
    cdisc_data(ASL = ASL, adte = adte, ADRS = ADRS, code = "", check = FALSE),
    "Data arguments should be capitalized. Please change"
  )
})
