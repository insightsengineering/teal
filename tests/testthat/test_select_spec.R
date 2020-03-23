context("select_spec")

test_that("Proper argument types", {
  choices <- c("c1", "c2", "c3")
  selected <- c("c1", "c2")
  expect_silent(select_spec(choices = choices, selected = selected))

  expect_error(select_spec(choices = list(list(choices)), selected = selected))
  expect_error(select_spec(choices = choices, selected = list(list(selected))))
  expect_error(select_spec(choices = choices, selected = selected, multiple = 1), "is_logical_single")
  expect_error(select_spec(choices = choices, selected = selected, multiple = c(TRUE, TRUE)), "is_logical_single")
  expect_error(select_spec(choices = choices, selected = selected, fixed = 1), "is_logical_single")
  expect_error(select_spec(choices = choices, selected = selected, label = factor("Hello")), "is_character_single")
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
  expect_identical(c3$label, "Column(s)")
})

test_that("Multiple choices", {
  choices <- c("c1", "c2", "c3")
  selected <- c("c1", "c2")
  expect_error(select_spec(choices = choices, selected = selected, multiple = FALSE), "multiple \\|\\| length")

  expect_silent(c1 <- select_spec(choices = choices, selected = selected, multiple = TRUE))
  expect_silent(c2 <- select_spec(choices = choices, selected = selected))
  expect_identical(c1, c2)

  expect_identical(names(c1), c("choices", "selected", "always_selected", "multiple", "fixed", "label"))
  expect_identical(c1$choices, setNames(choices, choices))
  expect_identical(c1$selected, setNames(selected, selected))

  expect_true(c1$multiple)
  expect_false(c1$fixed)
  expect_identical(c1$label, "Column(s)")
})

test_that("resolve_delayed select_spec works", {
  set.seed(1)
  ADSL <- data.frame(USUBJID = letters[1:10], # nolint
                     BMRKR1 = rnorm(10),
                     BMRKR2 = sample(c("L", "M", "H"), 10, replace = T),
                     stringsAsFactors = F)


  normal <- select_spec(
    choices = variable_choices(ADSL, c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  delayed <- select_spec(
    choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  expect_equal(class(delayed), c("delayed_select_spec", "delayed_data", "select_spec"))

  expect_equal(names(normal), names(delayed))

  ds <- teal:::FilteredData$new()
  ds$set_data("ADSL", ADSL)
  expect_identical(normal, resolve_delayed(delayed, ds))
})
