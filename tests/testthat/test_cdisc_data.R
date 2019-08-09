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
                           code = "ADSL <- cadsl\n",
                           check = TRUE))
  expect_silent(cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    dataset("ARG1", ARG1),
    dataset("ARG2", ARG2),
    code = "ADSL <- cadsl\n ARG1 <- cadsl\n ARG2 <- cadsl",
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

  result_to_compare <- list(structure(list(
                                      dataname = "ADSL",
                                      data = ADSL,
                                      keys = list(
                                        primary = c("STUDYID", "USUBJID"),
                                        foreign = NULL,
                                        parent = NULL
                                      ),
                                      labels = list(
                                        dataset_label = "Subject Level Analysis Dataset",
                                        column_labels = c(STUDYID = "Study Identifier",
                                                          USUBJID = "Unique Subject Identifier",
                                                          SUBJID = "Identifier for the Study",
                                                          SITEID = "Study Site Identifier",
                                                          AGE = "Age",
                                                          SEX = "Sex",
                                                          RACE = "Race",
                                                          COUNTRY = "Country",
                                                          ARM = "Description of Planned Arm",
                                                          ARMCD = "Planned Arm Code",
                                                          ACTARM = "Description of Actual Arm",
                                                          ACTARMCD = "Actual Arm Code",
                                                          STRATA1 = "Stratification Factor 1",
                                                          STRATA2 = "Stratification Factor 2",
                                                          BMRKR1 = "Continous Level Biomarker 1",
                                                          BMRKR2 = "Categorical Level Biomarker 2",
                                                          ITTFL = "Intent-To-Treat Population Flag",
                                                          BEP01FL = "Biomarker Evaluable Population Flag"
                                        )
                                      )
                                    ),
                                    class = c("cdisc_dataset", "dataset")))
  class(result_to_compare) <- "cdisc_data"
  result_to_compare <- setNames(result_to_compare, c("ADSL"))
  attr(result_to_compare, "code") <- code_empty

  expect_identical(result, result_to_compare)

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL),
                       cdisc_dataset("ADTTE", ADTTE, labels = list(dataset_label = NULL,
                                                                   column_labels = NULL)))

  result_to_compare <- list(structure(list(
    dataname = "ADSL",
    data = ADSL,
    keys = list(
      primary = c("STUDYID", "USUBJID"),
      foreign = NULL,
      parent = NULL
    ),
    labels = list(
      dataset_label = "Subject Level Analysis Dataset",
      column_labels = c(STUDYID = "Study Identifier",
                        USUBJID = "Unique Subject Identifier",
                        SUBJID = "Identifier for the Study",
                        SITEID = "Study Site Identifier",
                        AGE = "Age",
                        SEX = "Sex",
                        RACE = "Race",
                        COUNTRY = "Country",
                        ARM = "Description of Planned Arm",
                        ARMCD = "Planned Arm Code",
                        ACTARM = "Description of Actual Arm",
                        ACTARMCD = "Actual Arm Code",
                        STRATA1 = "Stratification Factor 1",
                        STRATA2 = "Stratification Factor 2",
                        BMRKR1 = "Continous Level Biomarker 1",
                        BMRKR2 = "Categorical Level Biomarker 2",
                        ITTFL = "Intent-To-Treat Population Flag",
                        BEP01FL = "Biomarker Evaluable Population Flag"
      )
    )
  ),
  class = c("cdisc_dataset", "dataset")),
  structure(list(
    dataname = "ADTTE",
    data = ADTTE,
    keys = list(
      primary = c("STUDYID", "USUBJID", "PARAMCD"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    labels = list(
      dataset_label = NULL,
      column_labels = NULL
      )
    ),
  class = c("cdisc_dataset", "dataset")))
  class(result_to_compare) <- "cdisc_data"
  result_to_compare <- setNames(result_to_compare, c("ADSL", "ADTTE"))
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
  expect_error(cdisc_data(cdisc_dataset("ADTTE", ADTTE),
                          code = "ADTTE <- cadtte",
                          check = FALSE), "ADSL argument is missing.")
})

test_that("Error - dataset is not of class cdisc_dataset", {
  expect_error(cdisc_data(ARG1 = 1, code = "", check = FALSE),
               "Argument in not of class dataset, please use cdisc_dataset function!")
})

test_that("Error - foreign keys are not existing in parent's dataset", {
  expect_error(cdisc_data(cdisc_dataset("ADSL", ADSL),
                          cdisc_dataset("ADTTE", ADTTE,
                                        keys = list(primary = NULL,
                                                    foreign = c("CNSR"),
                                                    parent = "ADSL"))
                          ), "Specified foreign keys are not exisiting in parent dataset.")
})

test_that("Error - primary keys are not unique for the dataset", {
  expect_error(cdisc_data(cdisc_dataset("ADSL", ADSL, keys = list(primary = c("SEX"),
                                                                  foreign = NULL,
                                                                  parent = NULL))
                          ), "ADSL: Keys don't uniquely distinguish the rows,  i.e. some rows share the same keys")
})

test_that("Error - parent is defined without foreign key", {
  expect_error(cdisc_data(cdisc_dataset("ADTTE", ADTTE, keys = list(primary = NULL,
                                                                  foreign = NULL,
                                                                  parent = "ADSL"))
  ), "ADTTE: Please specify both foreign keys and a parent!")
})
