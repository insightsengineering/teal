library(scda)
scda_data <- synthetic_cdisc_data("latest")
ADSL <- scda_data$adsl # nolint
ADTTE <- scda_data$adtte # nolint
data <- cdisc_data(
  cdisc_dataset("ADSL", ADSL),
  cdisc_dataset("ADTTE", ADTTE)
)
ds <- teal:::CDISCFilteredData$new()
isolate(filtered_data_set(data, ds))

test_that("Can create variable_choices with datasets with no or missing labels", {
  example_data <- data.frame(USUBJID = 1:2, STUDYID = 1:1)

  # no labels given
  choice_1 <- variable_choices(example_data)
  expect_equal(names(choice_1), c("USUBJID: USUBJID", "STUDYID: STUDYID"))

  # one missing label
  missing_one_label_data <- rtables::var_relabel(example_data, USUBJID = as.character(NA), STUDYID = "Label")
  choice_2 <- variable_choices(missing_one_label_data)
  expect_equal(names(choice_2), c("USUBJID: Label Missing", "STUDYID: Label"))

  # all missing label
  missing_two_label_data <- rtables::var_relabel(example_data, USUBJID = as.character(NA), STUDYID = as.character(NA))
  choice_2 <- variable_choices(missing_two_label_data)
  expect_equal(names(choice_2), c("USUBJID: Label Missing", "STUDYID: Label Missing"))
})

test_that("delayed version of variable_choices", {
  # hard-coded subset
  obj <- variable_choices("ADSL", subset = c("SEX", "ARMCD", "COUNTRY"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = c("SEX", "ARMCD", "COUNTRY"), key = NULL),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = c("SEX", "ARMCD", "COUNTRY"))
  )


  # functional subset
  obj <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = function(data) colnames(data)[1:2], key = NULL),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = colnames(ADSL)[1:2], key = get_cdisc_keys("ADSL"))
  )

  # non-null key value
  obj <- variable_choices("ADSL", key = c("USUBJID", "STUDYID"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = NULL, key = c("USUBJID", "STUDYID")),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, key = c("USUBJID", "STUDYID"))
  )
})
