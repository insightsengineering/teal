library(scda)

test_that("Proper argument types", {
  choices <- c("c1", "c2", "c3")
  selected <- c("c1", "c2")
  expect_silent(select_spec(choices = choices, selected = selected))

  expect_error(select_spec(choices = list(list(choices)), selected = selected))
  expect_error(select_spec(choices = choices, selected = list(list(selected))))
  expect_error(select_spec(choices = choices, selected = selected, multiple = 1), "Assertion on 'multiple'")
  expect_error(select_spec(choices = choices, selected = selected, multiple = c(TRUE, TRUE)), "Assertion on 'multiple'")
  expect_error(select_spec(choices = choices, selected = selected, fixed = 1), "Assertion on 'fixed'")
  expect_error(select_spec(choices = choices, selected = selected, label = factor("Hello")), "Assertion on 'label'")
})

test_that("Single choice", {
  expect_silent(
    c1 <- select_spec(
      choices = c("AVAL", "BMRKR1", "AGE"),
      selected = c("AVAL"),
      fixed = FALSE,
      label = "Column"
    )
  )
  expect_silent(
    c2 <- select_spec(
      choices = c("AVAL", "BMRKR1", "AGE"),
      fixed = FALSE,
      label = "Column"
    )
  )

  expect_identical(c1, c2)
  expect_identical(class(c1), "select_spec")
  expect_identical(c1$choices, setNames(c("AVAL", "BMRKR1", "AGE"), c("AVAL", "BMRKR1", "AGE")))
  expect_identical(c2$selected, setNames("AVAL", "AVAL"))
  expect_false(c1$multiple)
  expect_false(c2$multiple)
  expect_false(c1$fixed)
  expect_false(c2$fixed)

  # minimal example
  expect_silent(c3 <- select_spec(choices = c("AVAL", "BMRKR1", "AGE")))
  expect_identical(class(c3), "select_spec")
  expect_identical(c3$choices, setNames(c("AVAL", "BMRKR1", "AGE"), c("AVAL", "BMRKR1", "AGE")))
  expect_identical(c3$selected, setNames("AVAL", "AVAL"))
  expect_false(c3$multiple)
  expect_false(c3$fixed)
  expect_identical(c3$label, "Select")
})

test_that("Multiple choices", {
  choices <- c("c1", "c2", "c3")
  selected <- c("c1", "c2")
  expect_error(select_spec(choices = choices, selected = selected, multiple = FALSE), "multiple \\|\\| length")

  expect_silent(c1 <- select_spec(choices = choices, selected = selected, multiple = TRUE))
  expect_silent(c2 <- select_spec(choices = choices, selected = selected))
  expect_identical(c1, c2)

  expect_identical(names(c1), c("choices", "selected", "multiple", "fixed", "always_selected", "ordered", "label"))
  expect_identical(c1$choices, setNames(choices, choices))
  expect_identical(c1$selected, setNames(selected, selected))

  expect_true(c1$multiple)
  expect_false(c1$fixed)
  expect_null(c1$always_selected)
  expect_false(c1$ordered)
  expect_identical(c1$label, "Select")
})

test_that("resolve_delayed select_spec works", {
  set.seed(1)
  ADSL <- data.frame( # nolint
    USUBJID = letters[1:10],
    BMRKR1 = rnorm(10),
    BMRKR2 = sample(c("L", "M", "H"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  attr(ADSL, "keys") <- get_cdisc_keys("ADSL") # nolint

  expected_spec <- select_spec(
    choices = variable_choices(ADSL, c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  delayed_spec <- select_spec(
    choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  expect_equal(class(delayed_spec), c("delayed_select_spec", "delayed_data", "select_spec"))

  expect_equal(names(expected_spec), names(delayed_spec))

  ds <- teal:::CDISCFilteredData$new()
  isolate(ds$set_dataset(dataset("ADSL", ADSL)))
  expect_identical(expected_spec, isolate(resolve_delayed(delayed_spec, ds)))
})

scda_data <- synthetic_cdisc_data("latest")
adsl <- scda_data$adsl # nolint
adtte <- scda_data$adtte # nolint
data <- cdisc_data(
  cdisc_dataset("ADSL", adsl),
  cdisc_dataset("ADTTE", adtte)
)

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

testthat::test_that("delayed version of select_spec", {
  # hard-coded choices & selected
  obj <- select_spec(vc_hard, selected = vc_hard_short, multiple = FALSE)
  testthat::expect_equal(
    obj,
    structure(
      list(
        choices = vc_hard_exp,
        selected = vc_hard_short_exp,
        multiple = FALSE,
        fixed = FALSE,
        always_selected = NULL,
        ordered = FALSE,
        label = "Select"
      ),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- select_spec(
    variable_choices(adsl, subset = c("STUDYID", "USUBJID"), key = get_cdisc_keys("ADSL")),
    selected = variable_choices(adsl, "STUDYID", key = get_cdisc_keys("ADSL"))
  )
  testthat::expect_equal(res_obj, exp_obj)

  # functional choices & selected
  obj <- select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE)
  testthat::expect_equal(
    obj,
    structure(
      list(
        choices = vc_fun_exp,
        selected = vc_fun_short,
        multiple = FALSE,
        fixed = FALSE,
        always_selected = NULL,
        ordered = FALSE,
        label = "Select"
      ),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  testthat::expect_equal(res_obj, exp_obj)
})

testthat::test_that("all_choices passed to selected is the same as passing all choices", {
  testthat::expect_equal(
    select_spec(choices = letters, selected = letters),
    select_spec(choices = letters, selected = all_choices())
  )
})

testthat::test_that("multiple is set to TRUE if all_choices is passed to selected", {
  testthat::expect_true(select_spec(choices = variable_choices("test"), selected = all_choices())$multiple)
  testthat::expect_true(select_spec(choices = variable_choices(iris), selected = all_choices())$multiple)
})

testthat::test_that("default values", {
  test <- select_spec("a")
  testthat::expect_identical(test$selected, c(a = "a"))
  testthat::expect_false(test$multiple)
  testthat::expect_false(test$fixed)
  testthat::expect_null(test$always_selected)
  testthat::expect_false(test$ordered)
  testthat::expect_identical(test$label, "Select")
})