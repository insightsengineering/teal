context("cdisc_data")

filename <- system.file("preprocessing_empty_string.txt", package = "teal")
code_empty <- readChar(filename, file.info(filename)$size) # code for file
filename_check <- system.file("check_false_string.txt", package = "teal")
code_check <- readChar(filename_check, file.info(filename_check)$size)

library(random.cdisc.data)
ADSL <- ARG1 <- ARG2 <- cadsl # nolint
ADTTE <- cadtte # nolint
ADRS <- cadrs # nolint

test_that("Basic example cdisc dataset", {
  expect_identical(ADSL, cdisc_dataset("ADSL", ADSL)$data)
  expect_identical("ADSL", cdisc_dataset("ADSL", ADSL)$dataname)
  expect_true(any(class(cdisc_dataset("ADSL", ADSL)) == c("RelationalDataset")))
})

test_that("Basic example - without code and check", {
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE))
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
                           dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")), code = "", check = FALSE))
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
    dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
    dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
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
    dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
    dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
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
    dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
    dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
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

  adsl_yaml <- yaml::yaml.load_file(system.file("metadata/ADSL.yml", package = "random.cdisc.data"))
  adtte_yaml <- yaml::yaml.load_file(system.file("metadata/ADTTE.yml", package = "random.cdisc.data"))

  result_to_compare <- structure(
    list(ADSL = RelationalDataset$new(
    name = "ADSL",
    x = ADSL,
    keys = keys(
      primary = c("STUDYID", "USUBJID"),
      foreign = NULL,
      parent = NULL
    ),
    label = adsl_yaml$domain$label
  )),
  code = paste0(code_empty, "\n\n", code_check, "\n"),
  class = "cdisc_data")

  expect_equal(result, result_to_compare)

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL),
                       cdisc_dataset("ADTTE", ADTTE))

  result_to_compare <- list(
    ADSL = RelationalDataset$new(
    name = "ADSL",
    x = ADSL,
    keys = keys(
      primary = c("STUDYID", "USUBJID"),
      foreign = NULL,
      parent = NULL
    ),
    label = adsl_yaml$domain$label
  ),
  ADTTE = RelationalDataset$new(
    name = "ADTTE",
    x = ADTTE,
    keys = keys(
      primary = c("STUDYID", "USUBJID", "PARAMCD"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    label = adtte_yaml$domain$label
  ))

  class(result_to_compare) <- "cdisc_data"
  attr(result_to_compare, "code") <- paste0(code_empty, "\n\n", code_check, "\n")

  expect_equal(result, result_to_compare)
})

test_that("Keys in cached datasets", {
  expect_true(all(get_cdisc_keys("ADSL")$primary %in% names(random.cdisc.data::cadsl)))

  expect_true(all(get_cdisc_keys("ADAE")$primary %in% names(random.cdisc.data::cadae)))
  expect_true(all(get_cdisc_keys("ADAE")$foreign %in% names(random.cdisc.data::cadae)))

  expect_true(all(get_cdisc_keys("ADTTE")$primary %in% names(random.cdisc.data::cadtte)))
  expect_true(all(get_cdisc_keys("ADTTE")$foreign %in% names(random.cdisc.data::cadtte)))

  expect_true(all(get_cdisc_keys("ADCM")$primary %in% names(random.cdisc.data::cadcm)))
  expect_true(all(get_cdisc_keys("ADCM")$foreign %in% names(random.cdisc.data::cadcm)))

  expect_true(all(get_cdisc_keys("ADLB")$primary %in% names(random.cdisc.data::cadlb)))
  expect_true(all(get_cdisc_keys("ADLB")$foreign %in% names(random.cdisc.data::cadlb)))

  expect_true(all(get_cdisc_keys("ADRS")$primary %in% names(random.cdisc.data::cadrs)))
  expect_true(all(get_cdisc_keys("ADRS")$foreign %in% names(random.cdisc.data::cadrs)))

  expect_true(all(get_cdisc_keys("ADVS")$primary %in% names(random.cdisc.data::cadvs)))
  expect_true(all(get_cdisc_keys("ADVS")$foreign %in% names(random.cdisc.data::cadvs)))
})

test_that("Empty code", {

  # missing code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), check = FALSE)
  expect_identical(attr(result, "code"), paste0(code_empty, "\n\n", code_check, "\n"))

  # NULL code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE)
  expect_identical(attr(result, "code"), paste0(code_empty, "\n\n", code_check, "\n"))

  # empty code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE)
  expect_identical(attr(result, "code"), paste0(code_empty, "\n\n", code_check, "\n"))
})


test_that("Arguments created by code", {
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL),
                       code = "ADSL <- cadsl",
                       check = FALSE)
  expect_silent(result)

  result_to_compare <- list(cdisc_dataset("ADSL", ADSL))
  class(result_to_compare) <- "cdisc_data"
  result_to_compare <- setNames(result_to_compare, c("ADSL"))
  attr(result_to_compare, "code") <- paste0("ADSL <- cadsl", "\n\n", code_check, "\n")

  expect_equal(result, result_to_compare)
})

test_that("Error - objects differs", {
  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL), code = "ADSL <- 2", check = TRUE),
    "Cannot reproduce object"
  )

  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL), code = "ADSL <- radsl(N=300);", check = TRUE),
    "Cannot reproduce object"
  )
})

test_that("Error - ADSL is missing", {
  expect_error(
    cdisc_data(cdisc_dataset("ADTTE", ADTTE), code = "ADTTE <- cadtte", check = FALSE),
    "ADSL argument is missing."
  )
})

test_that("Error - duplicated names", {
  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      cdisc_dataset("ADSL", ADSL),
      code = "",
      check = FALSE
    ),
    "Found duplicated dataset names"
  )
})

test_that("Error - dataset is not of class cdisc_dataset", {
  expect_error(
    cdisc_data(ARG1 = 1, code = "", check = FALSE),
    "Argument in not of class dataset, please use cdisc_dataset function!"
  )
})

test_that("Empty keys for single and multiple datasets", {
  expect_silent(cdisc_data(dataset("ADSL", ADSL)))

  expect_silent(cdisc_data(dataset("ADSL", ADSL), dataset("ADTTE", ADTTE)))
})

test_that("Error - primary keys are not unique for the dataset", {
  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL,
                             keys = keys(primary = c("SEX"),
                                         foreign = NULL,
                                         parent = NULL))),
    "ADSL: Keys don't uniquely distinguish the rows,  i.e. some rows share the same keys")
})

test_that("Error - parent is defined without foreign key", {
  expect_error(cdisc_data(cdisc_dataset("ADTTE", ADTTE, keys = keys(primary = c("STUDYID", "USUBJID"),
                                                                  foreign = NULL,
                                                                  parent = "ADSL"))
  ), "ADTTE: Please specify both foreign keys and a parent!")

  expect_error(
    cdisc_data(cdisc_dataset("ADTTE", ADTTE,
                             keys = keys(primary = c("STUDYID", "USUBJID"),
                                         foreign = c("STUDYID", "USUBJID"),
                                         parent = NULL)
    )), "ADTTE: Please specify both foreign keys and a parent!")

})

test_that("Warning - Different keys names but same length", {
  ADTTE <- dplyr::rename(ADTTE, ADSL_STUDYID = STUDYID, ADSL_USUBJID = USUBJID) # nolint
  expect_warning(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ADTTE", ADTTE,
              keys = keys(primary = c("ADSL_STUDYID", "ADSL_USUBJID", "PARAMCD"),
                          foreign = c("ADSL_STUDYID", "ADSL_USUBJID"),
                          parent = "ADSL"))
    ),
    "Following foreign keys are not identical to the primary keys"
  )
})

test_that("Error - length of child keys > length of parent keys", {
  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ADTTE", ADTTE,
              keys = keys(primary = c("STUDYID", "USUBJID", "PARAMCD"),
                          foreign = c("STUDYID", "USUBJID", "PARAMCD"),
                          parent = "ADSL"))
    ),
    "Number of foreign keys can't be larger than"
  )

  ADTTE <- dplyr::rename(ADTTE, ADSL_STUDYID = STUDYID, ADSL_USUBJID = USUBJID) # nolint
  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ADTTE", ADTTE,
              keys = keys(primary = c("ADSL_STUDYID", "ADSL_USUBJID", "PARAMCD"),
                          foreign = c("ADSL_STUDYID", "ADSL_USUBJID", "PARAMCD"),
                          parent = "ADSL"))
    ),
    "Number of foreign keys can't be larger than"
  )
})

test_that("Error - items dataset to wide (without parent)", {
  COUNTRIES <- data.frame(COUNTRY = unique(ADSL$COUNTRY), VALUE = rnorm(length(unique(ADSL$COUNTRY)))) # nolint

  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("COUNTRIES", COUNTRIES,
              keys = keys(primary = c("COUNTRY"),
                          foreign = c("COUNTRY"),
                          parent = "ADSL"))
    ),
    "Foreign keys are don't match all parent keys and both have different length"
  )

  expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL,
                    keys = keys(primary = c("STUDYID", "USUBJID", "COUNTRY"), foreign = NULL, parent = NULL)),
      dataset(dataname = "COUNTRIES",
              data = COUNTRIES,
              keys = keys(primary = c("COUNTRY"),
                          foreign = c("COUNTRY"),
                          parent = "ADSL"))
    )
  )

})

test_that("Error - Two root datasets with different keys", {
  ADSL2 <- ADSL %>% dplyr::rename(ADSL_STUDYID = STUDYID) # nolint

  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ADSL2", ADSL2,
              keys = keys(primary = c("ADSL_STUDYID", "USUBJID"),
                          foreign = NULL,
                          parent = NULL)),
      cdisc_dataset("ADTTE", ADTTE)
    ),
    "Root dataset keys doesn't match ADSL primary keys"
  )
})
