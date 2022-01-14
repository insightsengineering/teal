adsl <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adtte <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
data <- cdisc_data(cdisc_dataset("ADSL", adsl), cdisc_dataset("ADTTE", adtte))

ds <- teal:::CDISCFilteredData$new()
isolate(filtered_data_set(data, ds))

vc_hard <- variable_choices("ADSL", subset = c("STUDYID", "USUBJID"))
vc_hard_exp <- structure(
  list(data = "ADSL", subset = c("STUDYID", "USUBJID"), key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_hard_short <- variable_choices("ADSL", subset = "STUDYID")
vc_hard_short_exp <- structure(
  list(data = "ADSL", subset = "STUDYID", key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
vc_fun_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1:2], key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun_short <- variable_choices("ADSL", subset = function(data) colnames(data)[1])
vc_fun_short_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1], key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

testthat::test_that("delayed version of choices_selected", {
  # hard-coded choices and selected
  obj <- choices_selected(vc_hard, selected = vc_hard_short)
  testthat::expect_equal(
    obj,
    structure(
      list(choices = vc_hard_exp, selected = vc_hard_short_exp, keep_order = FALSE, fixed = FALSE),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- choices_selected(
    variable_choices(adsl, subset = c("STUDYID", "USUBJID"), key = get_cdisc_keys("ADSL")),
    selected = variable_choices(adsl, subset = c("STUDYID"), key = get_cdisc_keys("ADSL"))
  )
  testthat::expect_equal(res_obj, exp_obj, check.attributes = TRUE)

  # functional choices and selected
  obj <- choices_selected(vc_fun, selected = vc_fun_short)
  testthat::expect_equal(
    obj,
    structure(
      list(choices = vc_fun_exp, selected = vc_fun_short_exp, keep_order = FALSE, fixed = FALSE),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  testthat::expect_equal(res_obj, exp_obj)
})

testthat::test_that("choices_selected does not add selected to choices when selected is not found in choices", {
  test <- choices_selected(choices = c("a"), selected = "b")
  testthat::expect_equal(test$choices, "a")
})

testthat::test_that("all_choices is the same as selecting all choices", {
  testthat::expect_equal(
    choices_selected(choices = letters, selected = letters),
    choices_selected(choices = letters, selected = all_choices())
  )
})

testthat::test_that("choices_selected throws when selected is delayed and choices are not", {
  delayed_selected <- structure("A", class = "delayed_data")
  testthat::expect_error(
    choices_selected(choices = c("A", "B"), selected = delayed_selected),
    regexp = "If 'selected' is of class 'delayed_data', so must be 'choices'"
  )
})

testthat::test_that("choices_selected throws when no_select_keyword is passed to it as a choice", {
  no_select_keyword <- "-- no selection --"
  testthat::expect_error(
    choices_selected(choices = no_select_keyword),
    regexp = "-- no selection -- is not a valid choice as it is used as a keyword"
  )
})

testthat::test_that("is returns choices_selected if passed a choices selected object", {
  testthat::expect_equal(is(choices_selected(choices = "A")), "choices_selected")
})

testthat::test_that("choices_selected remove duplicates", {
  testthat::expect_identical(choices_selected(choices = c("A", "A")),
                             structure(list(choices = "A", selected = "A", fixed = FALSE), class = "choices_selected"))
  testthat::expect_identical(choices_selected(choices = setNames(c("A", "A"), c("A", "A"))),
                             structure(list(choices = c(A = "A"), selected = c(A = "A"), fixed = FALSE),
                                       class = "choices_selected"))
  testthat::expect_equal(
    choices_selected(
      choices = c("name for A" = "A", "name for A" = "A",
                  "Name for nothing" = "", "name for b" = "B", "name for C" = "C"),
      selected = c("A", "A")),
    structure(list(choices = c(`name for A` = "A", `Name for nothing` = "", `name for b` = "B", `name for C` = "C"),
                   selected = "A", fixed = FALSE),
              class = "choices_selected")
  )

  testthat::expect_equal(
    choices_selected(
      structure(c(`STUDYID: Study Identifier` = "STUDYID", `STUDYID: Study Identifier` = "STUDYID"),
                raw_labels = c(STUDYID = "Study Identifier", STUDYID = "Study Identifier"),
                combined_labels = c("STUDYID: Study Identifier", "STUDYID: Study Identifier"),
                class = c("choices_labeled", "character"))
    ),
    structure(list(choices = structure(c(`STUDYID: Study Identifier` = "STUDYID"),
                                       raw_labels = c(STUDYID = "Study Identifier"),
                                       combined_labels = "STUDYID: Study Identifier",
                                       class = c("choices_labeled", "character")),
                   selected = c(`STUDYID: Study Identifier` = "STUDYID"),
                   fixed = FALSE), class = "choices_selected")
  )
})
