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
ADSL <- ARG1 <- ARG2 <- cadsl # nolint
ADTTE <- cadtte # nolint
ADRS <- cadrs # nolint

test_that("Basic example cdisc dataset", {
  expect_identical(ADSL, cdisc_dataset("ADSL", ADSL)$data)
  expect_identical("ADSL", cdisc_dataset("ADSL", ADSL)$dataname)
  expect_true(all(class(cdisc_dataset("ADSL", ADSL)) %in% list("cdisc_dataset", "dataset")))
})

test_that("Basic example - without code and check", {
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE))
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           dataset("ARG1", ARG1),
                           dataset("ARG2", ARG2), code = "", check = FALSE))
})

test_that("Basic example - with code and check", {
  expect_true(is.data.frame(ADSL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))
  keys(ADSL) <- keys(ARG1) <- keys(ARG2) <- c("STUDYID", "USUBJID")

  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           code = "ADSL <- cadsl; keys(ADSL) <- c('STUDYID','USUBJID')",
                           check = TRUE))
  expect_silent(cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    dataset("ARG1", ARG1),
    dataset("ARG2", ARG2),
    code = "ADSL <- ARG1 <- ARG2 <- cadsl; keys(ADSL) <- keys(ARG1) <- keys(ARG2) <- c('STUDYID','USUBJID')",
    check = TRUE
  ))
})

test_that("Basic example - with vector code and check", {
  expect_true(is.data.frame(ADSL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))
  keys(ADSL) <- keys(ARG1) <- keys(ARG2) <- c("STUDYID", "USUBJID")

  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           code = c("ADSL <- cadsl; keys(ADSL) <- c('STUDYID','USUBJID')"),
                           check = TRUE))
  expect_silent(cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    dataset("ARG1", ARG1),
    dataset("ARG2", ARG2),
    code = c("ADSL <- ARG1 <- ARG2 <- cadsl; keys(ADSL) <- keys(ARG1) <- keys(ARG2) <- c('STUDYID','USUBJID')"),
    check = TRUE
  ))
})

test_that("Basic example - with line break code and check", {
  expect_true(is.data.frame(ADSL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))
  keys(ADSL) <- keys(ARG1) <- keys(ARG2) <- c("STUDYID", "USUBJID")

  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           code = "ADSL <- cadsl\nkeys(ADSL) <- c('STUDYID','USUBJID')",
                           check = TRUE))
  expect_silent(cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    dataset("ARG1", ARG1),
    dataset("ARG2", ARG2),
    code = "ADSL <- ARG1 <- ARG2 <- cadsl\nkeys(ADSL) <- keys(ARG1) <- keys(ARG2) <- c('STUDYID','USUBJID')",
    check = TRUE
  ))
})

test_that("Naming list elements", {
  keys(ADTTE) <- c("STUDYID", "USUBJID", "PARAMCD", "ARMCD")
  keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")

  expect_identical(names(cdisc_data(cdisc_dataset("ADSL", ADSL))), "ADSL")
  expect_identical(names(cdisc_data(cdisc_dataset("ADSL", ADSL),
                                    cdisc_dataset("ADTTE", ADTTE),
                                    cdisc_dataset("ADRS", ADRS))),
                   c("ADSL", "ADTTE", "ADRS"))
})

test_that("List values", {
  keys(ADSL) <- c("STUDYID", "USUBJID")
  keys(ADTTE) <- c("STUDYID", "USUBJID", "PARAMCD", "ARMCD")
  keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL))

  result_to_compare <- list(ADSL = cadsl)
  class(result_to_compare) <- "cdisc_data"
  keys(result_to_compare[["ADSL"]]) <- c("STUDYID", "USUBJID")
  attr(result_to_compare[["ADSL"]], "dataname") <- "ADSL"

  attr(result_to_compare, "code") <- code_empty

  expect_identical(result, result_to_compare)

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL),
                       cdisc_dataset("ADTTE", ADTTE),
                       cdisc_dataset("ADRS", ADRS))

  result_to_compare <- list(ADSL = cadsl, ADTTE = cadtte, ADRS = cadrs)
  class(result_to_compare) <- "cdisc_data"
  keys(result_to_compare[["ADSL"]]) <- c("STUDYID", "USUBJID")
  keys(result_to_compare[["ADTTE"]]) <- c("STUDYID", "USUBJID", "PARAMCD", "ARMCD")
  keys(result_to_compare[["ADRS"]]) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
  attr(result_to_compare[["ADSL"]], "dataname") <- "ADSL"
  attr(result_to_compare[["ADTTE"]], "dataname") <- "ADTTE"
  attr(result_to_compare[["ADRS"]], "dataname") <- "ADRS"
  attr(result_to_compare, "code") <- code_empty

  expect_identical(result, result_to_compare)
})

test_that("Empty code", {
  keys(ADSL) <- c("STUDYID", "USUBJID")

  # missing code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), check = FALSE)
  expect_identical(attr(result, "code"), code_empty)

  # NULL code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE)
  expect_identical(attr(result, "code"), code_empty)

  # empty code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE)
  expect_identical(attr(result, "code"), code_empty)
})


test_that("Arguments created by code", {
  keys(ADSL) <- c("STUDYID", "USUBJID")
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL),
                       code = "ADSL <- cadsl; keys(ADSL) <- 'STUDYID'",
                       check = FALSE)
  expect_silent(result)

  result_to_compare <- list(ADSL = cadsl)
  class(result_to_compare) <- "cdisc_data"
  keys(result_to_compare[["ADSL"]]) <- c("STUDYID", "USUBJID")
  attr(result_to_compare[["ADSL"]], "dataname") <- "ADSL"
  attr(result_to_compare, "code") <- "ADSL <- cadsl; keys(ADSL) <- 'STUDYID'"

  expect_identical(result, result_to_compare)
})

test_that("Error - objects differs", {
  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL), code = "ADSL <- 2", check = TRUE),
    "Cannot reproduce object"
  )

  keys(ADSL) <- c("STUDYID", "USUBJID")
  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL),
               code = "ADSL <- cadsl; keys(ADSL) <- c('USUBJID', 'STUDYID', 'AGE')",
               check = TRUE),
    "Cannot reproduce object"
  )
})

test_that("Error - ADSL is missing", {
  expect_error(cdisc_data(ARG1 = 1, code = "", check = FALSE), "ADSL and code arguments are missing")
  expect_error(cdisc_data(code = "x <- 2", check = FALSE), "ADSL is missing and cannot be generated by code")
})
