context("filter_spec")

choices <- c("val1", "val2", "val3")
choices_d <- c("val1", "val1", "val2", "val3")
choices_f <- as.factor(choices)
choices_l <- as.list(choices)

test_that("Proper argument types", {
  expect_error(filter_spec(vars = list("var"), choices = choices), "is_character_vector")
  expect_error(filter_spec(vars = "var", choices = choices_l), "is_character_vector")
  expect_error(filter_spec(vars = "var", choices = choices, selected = list("val2")), "is_character_vector")
  expect_error(filter_spec(vars = 1, choices = choices, selected = choices[1]), "is.character")
  expect_error(filter_spec(vars = "var", choices = 1:3, selected = 1), "is.character")
  expect_error(filter_spec(vars = factor("var"), choices = choices, selected = choices[1]), "is.character")
  expect_error(filter_spec(vars = "var", choices = choices_f, selected = choices_f[1]), "is.character")
  expect_error(filter_spec(vars = "var", choices = choices, multiple = 1), "is.logical")
  expect_error(filter_spec(vars = "var", choices = choices, label = factor("test")), "is.character")

  expect_error(filter_spec(vars = "var", choices = choices_d), "duplicated")
  expect_error(filter_spec(vars = "var", choices = choices, selected = c("val1", "val1")), "duplicated")

  expect_error(filter_spec(vars = "var", choices = choices, label = c("test", "test2")), "is_character_single")
  expect_error(filter_spec(vars = "var", choices = choices, sep = c("-", ",")), "is_character_single")
})

test_that("Single choice", {
  expect_silent(f1 <- filter_spec(vars = "var1",
                                  choices = choices,
                                  selected = choices[1],
                                  multiple = FALSE,
                                  label = "test"))
  expect_identical(names(f1), c("vars", "choices", "selected", "multiple", "label", "sep"))
  expect_identical(f1$choices, as.list(setNames(choices, choices)))
  expect_identical(f1$selected, as.list(setNames(choices[1], choices[1])))

  expect_false(f1$multiple)
  expect_identical(f1$label, "test")
})

test_that("Multiple choices", {
  expect_error(filter_spec(vars = "var1", choices = choices, selected = choices[1:2], multiple = FALSE), "multiple")
  expect_silent(f1 <- filter_spec(vars = "var1", choices = choices, selected = choices[1:2], multiple = TRUE))
  expect_silent(f2 <- filter_spec(vars = "var1", choices = choices, selected = choices[1:2]))
  expect_identical(f1, f2)

  expect_true(f1$multiple)
  expect_identical(f1$choices, list(val1 = "val1", val2 = "val2", val3 = "val3"))
  expect_identical(f1$selected, list(val1 = "val1", val2 = "val2"))
})

test_that("Multiple vars", {
  expect_error(filter_spec(vars = c("var1", "var2"), choices = c("val1.1-val2.1", "val1.1-val2.2"), sep = ":"),
               "length\\(vars\\)")

  expect_error(filter_spec(vars = c("var1", "var2"), choices = c("val1.1-val2.1", "val1.1-val2.2")),
               "length\\(vars\\)")

  expect_error(filter_spec(vars = "var1", choices = c("val-1", "val2", "val3", "val4"), sep = "-"),
               "length\\(vars\\)")

  expect_silent(f1 <- filter_spec(vars = c("var1", "var2"),
                                  choices = c("val1.1 - val2.1", "val1.1 - val2.2"),
                                  sep = " - "))

  expect_silent(f2 <- filter_spec(vars = c("var1", "var2"),
                                  choices = c("val1.1 - val2.1", "val1.1 - val2.2")))

  expect_silent(f3 <- filter_spec(vars = c("var1", "var2"),
                                  choices = c(`val1.1 - val2.1` = "val1.1 - val2.1",
                                              `val1.1 - val2.2` = "val1.1 - val2.2")))

  expect_silent(f5 <- filter_spec(vars = c("var1", "var2"),
                                  choices = c(`combo1` = "val1.1 - val2.1",
                                              `combo2` = "val1.1 - val2.2")))



  expect_identical(f1, f2)
  expect_identical(f1, f3)
  expect_true(all(names(f1$choices) != names(f5$choices)))

  expect_identical(f1$vars, c("var1", "var2"))
  expect_identical(names(f1$choices), c("val1.1 - val2.1", "val1.1 - val2.2"))
  expect_identical(names(f5$choices), c("combo1", "combo2"))
  expect_identical(f1$selected, list(`val1.1 - val2.1` = c("val1.1", "val2.1")))

  # Multiple vars and multiple = TRUE
  choices <- c("val1.1 - val2.1", "val1.1 - val2.2", "val1.1 - val2.3")

  expect_silent(
    f1m <- filter_spec(vars = c("var1", "var2"),
                      choices = choices,
                      selected = choices[1:2],
                      multiple = TRUE,
                      sep = " - ")
  )

  expect_silent(
    f2m <- filter_spec(vars = c("var1", "var2"),
                       choices = choices,
                       selected = choices[1:2],
                       sep = " - ")
  )

  expect_identical(f1m, f2m)

  # correct object structure
  expect_identical(names(f1m), c("vars", "choices", "selected", "multiple", "label", "sep"))
  expect_identical(f1m$choices, list(`val1.1 - val2.1` = c("val1.1", "val2.1"),
                                     `val1.1 - val2.2` = c("val1.1", "val2.2"),
                                     `val1.1 - val2.3` = c("val1.1", "val2.3")))
  expect_identical(f1m$selected, list(`val1.1 - val2.1` = c("val1.1", "val2.1"),
                                      `val1.1 - val2.2` = c("val1.1", "val2.2")))

  expect_true(f1m$multiple)
  expect_identical(f1m$label, "Filter")
})

test_that("delayed filter_spec works", {
  set.seed(1)
  ADSL <- data.frame(USUBJID = letters[1:10],  # nolint
                     SEX = sample(c("F", "M", "U"), 10, replace = T),
                     stringsAsFactors = F)


  normal <- filter_spec(
    vars = variable_choices(ADSL, "SEX"),
    sep = "-",
    choices = value_choices(ADSL, "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  delayed <- filter_spec(
    vars = variable_choices("ADSL", "SEX"),
    sep = "-",
    choices = value_choices("ADSL", "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  expect_equal(class(delayed), c("delayed_filter_spec", "delayed_data", "filter_spec"))

  expect_equal(names(normal), names(delayed))

  ds <- teal:::FilteredData$new()
  ds$set_data("ADSL", ADSL)
  expect_identical(normal, resolve_delayed(delayed, ds))
})
