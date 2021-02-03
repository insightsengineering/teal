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
