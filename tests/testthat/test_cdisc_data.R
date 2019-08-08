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

  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           code = "ADSL <- cadsl",
                           check = TRUE))
  expect_silent(cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    dataset("ARG1", ARG1),
    dataset("ARG2", ARG2),
    code = "ADSL <- ARG1 <- ARG2 <- cadsl;",
    check = TRUE
  ))
})

test_that("Basic example - with vector code and check", {
  expect_true(is.data.frame(ADSL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))

  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           code = c("ADSL <- cadsl"),
                           check = TRUE))
  expect_silent(cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    dataset("ARG1", ARG1),
    dataset("ARG2", ARG2),
    code = c("ADSL <- ARG1 <- ARG2 <- cadsl"),
    check = TRUE
  ))
})

test_that("Basic example - with line break code and check", {
  expect_true(is.data.frame(ADSL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))

  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           code = "ADSL <- cadsl",
                           check = TRUE))
  expect_silent(cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    dataset("ARG1", ARG1),
    dataset("ARG2", ARG2),
    code = "ADSL <- ARG1 <- ARG2 <- cadsl",
    check = TRUE
  ))
})

test_that("Naming list elements", {

  expect_identical(names(cdisc_data(cdisc_dataset("ADSL", ADSL))), "ADSL")
  expect_identical(names(cdisc_data(cdisc_dataset("ADSL", ADSL),
                                    cdisc_dataset("ADTTE", ADTTE),
                                    cdisc_dataset("ADRS", ADRS))),
                   c("ADSL", "ADTTE", "ADRS"))
})

test_that("List values", {

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL))

  result_to_compare <- list(cdisc_dataset("ADSL", ADSL))
  class(result_to_compare) <- "cdisc_data"
  result_to_compare <- setNames(result_to_compare, c("ADSL"))
  attr(result_to_compare, "code") <- code_empty

  expect_identical(result, result_to_compare)

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL),
                       cdisc_dataset("ADTTE", ADTTE),
                       cdisc_dataset("ADRS", ADRS))

  result_to_compare <- list(cdisc_dataset("ADSL", ADSL),
                            cdisc_dataset("ADTTE", ADTTE),
                            cdisc_dataset("ADRS", ADRS))
  class(result_to_compare) <- "cdisc_data"
  result_to_compare <- setNames(result_to_compare, c("ADSL", "ADTTE", "ADRS"))
  attr(result_to_compare, "code") <- code_empty

  expect_identical(result, result_to_compare)
})

test_that("Empty code", {

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
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL),
                       code = "ADSL <- cadsl",
                       check = FALSE)
  expect_silent(result)

  result_to_compare <- list(cdisc_dataset("ADSL", ADSL))
  class(result_to_compare) <- "cdisc_data"
  result_to_compare <- setNames(result_to_compare, c("ADSL"))
  attr(result_to_compare, "code") <- "ADSL <- cadsl"

  expect_identical(result, result_to_compare)
})

test_that("Error - objects differs", {
  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL), code = "ADSL <- 2", check = TRUE),
    "Cannot reproduce object"
  )

  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL),
               code = "ADSL <- radsl(N=300);",
               check = TRUE),
    "Cannot reproduce object"
  )
})

test_that("Error - ADSL is missing", {
  expect_error(cdisc_data(ARG1 = 1, code = "", check = FALSE),
               "Argument in not of class dataset, please use dataset function!")
  expect_error(cdisc_data(code = "x <- 2", check = FALSE), "ADSL argument is missing.")
})
