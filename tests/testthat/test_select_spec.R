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

  # select among choices
  choices <- list(c("a", "b"), c("c", "d"))
  expect_error(select_spec(choices = choices, selected = c("a", "b")), "selected %is_in%")
  expect_error(select_spec(choices = choices, selected = list(c("a", "c"))), "selected %is_in%")
  expect_silent(select_spec(choices = choices, selected = list(c("a", "b"))))
  expect_silent(select_spec(choices = c("a", "b", "c"), selected = c("a", "b")))
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
  expect_identical(c1$choices, as.list(setNames(c("AVAL", "BMRKR1", "AGE"), c("AVAL", "BMRKR1", "AGE"))))
  expect_identical(c2$selected, as.list(setNames("AVAL", "AVAL")))
  expect_false(c1$multiple)
  expect_false(c2$multiple)
  expect_false(c1$fixed)
  expect_false(c2$fixed)

  # minimal example
  expect_silent(c3 <- select_spec(choices = c("AVAL", "BMRKR1", "AGE")))
  expect_identical(class(c3), "select_spec")
  expect_identical(c3$choices, as.list(setNames(c("AVAL", "BMRKR1", "AGE"), c("AVAL", "BMRKR1", "AGE"))))
  expect_identical(c3$selected, as.list(setNames("AVAL", "AVAL")))
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

  expect_identical(names(c1), c("choices", "selected", "multiple", "fixed", "label", "sep"))
  expect_identical(c1$choices, as.list(setNames(choices, choices)))
  expect_identical(c1$selected, as.list(setNames(selected, selected)))

  expect_true(c1$multiple)
  expect_false(c1$fixed)
  expect_identical(c1$label, "Column(s)")

  # choices nested in list
  choices <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  selected <- list(c("a", "b"))

  expect_silent(c3 <- select_spec(choices = choices, selected = selected))

  expect_identical(c3$choices, setNames(choices, vapply(choices, paste, collapse = " - ", character(1))))
  expect_identical(c3$selected, setNames(selected, vapply(selected, paste, collapse = " - ", character(1))))
})
