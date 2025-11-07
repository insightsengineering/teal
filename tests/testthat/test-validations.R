testthat::test_that("validate_one_row_per_id throws error when duplicates exist", {
  data <- data.frame(id = c(1, 2, 1), value = 1:3)
  testthat::expect_error(
    validate_one_row_per_id(data, key = "id"),
    "Found more than one row per id"
  )
})

testthat::test_that("validate_one_row_per_id passes when no duplicates exist", {
  data <- data.frame(id = 1:3, value = 1:3)
  testthat::expect_silent(validate_one_row_per_id(data, key = "id"))
})

testthat::test_that("validate_one_row_per_id uses default key", {
  data <- data.frame(USUBJID = 1:3, STUDYID = rep(1, 3), value = 1:3)
  testthat::expect_silent(validate_one_row_per_id(data))
})

testthat::test_that("validate_in throws error when x is not in choices", {
  testthat::expect_error(
    validate_in("d", c("a", "b", "c"), "Value not in choices"),
    "Value not in choices"
  )
})

testthat::test_that("validate_in passes when x is in choices", {
  testthat::expect_silent(validate_in("a", c("a", "b", "c"), "Value not in choices"))
})

testthat::test_that("validate_in throws error when x is empty", {
  testthat::expect_error(
    validate_in(character(0), c("a", "b", "c"), "Value not in choices"),
    "Value not in choices"
  )
})

testthat::test_that("validate_in throws error when choices is empty", {
  testthat::expect_error(
    validate_in("a", character(0), "Value not in choices"),
    "Value not in choices"
  )
})

testthat::test_that("validate_in works with multiple values", {
  testthat::expect_silent(validate_in(c("a", "b"), c("a", "b", "c"), "Value not in choices"))
  testthat::expect_error(
    validate_in(c("a", "d"), c("a", "b", "c"), "Value not in choices"),
    "Value not in choices"
  )
})

testthat::test_that("validate_has_elements throws error when x is empty", {
  testthat::expect_error(
    validate_has_elements(character(0), "No elements"),
    "No elements"
  )
})

testthat::test_that("validate_has_elements passes when x has elements", {
  testthat::expect_silent(validate_has_elements(c("a", "b"), "No elements"))
})

testthat::test_that("validate_no_intersection throws error when x and y intersect", {
  testthat::expect_error(
    validate_no_intersection(c(1, 2, 3), c(3, 4, 5), "Overlap detected"),
    "Overlap detected"
  )
})

testthat::test_that("validate_no_intersection passes when x and y do not intersect", {
  testthat::expect_silent(validate_no_intersection(c(1, 2), c(3, 4), "Overlap detected"))
})

testthat::test_that("validate_has_variable throws error when variable is missing", {
  data <- data.frame(x = 1:3, y = 1:3)
  testthat::expect_error(
    validate_has_variable(data, "z"),
    "does not have the required variables"
  )
})

testthat::test_that("validate_has_variable passes when variable exists", {
  data <- data.frame(x = 1:3, y = 1:3)
  testthat::expect_silent(validate_has_variable(data, "x"))
})

testthat::test_that("validate_has_variable works with multiple variables", {
  data <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  testthat::expect_silent(validate_has_variable(data, c("x", "y")))
  testthat::expect_error(
    validate_has_variable(data, c("x", "w")),
    "does not have the required variables"
  )
})

testthat::test_that("validate_has_variable uses custom message when provided", {
  data <- data.frame(x = 1:3)
  testthat::expect_error(
    validate_has_variable(data, "z", msg = "Custom error message"),
    "Custom error message"
  )
})

testthat::test_that("validate_has_variable passes when varname is empty", {
  data <- data.frame(x = 1:3)
  testthat::expect_silent(validate_has_variable(data, character(0)))
})

testthat::test_that("validate_n_levels throws error when levels less than min_levels", {
  x <- factor(c("a", "b"))
  testthat::expect_error(
    validate_n_levels(x, min_levels = 3, max_levels = 10, var_name = "x"),
    "needs minimum 3 level"
  )
})

testthat::test_that("validate_n_levels throws error when levels greater than max_levels", {
  x <- factor(letters[1:15])
  testthat::expect_error(
    validate_n_levels(x, min_levels = 1, max_levels = 10, var_name = "x"),
    "needs minimum 1 level\\(s\\) and maximum 10 level\\(s\\)"
  )
})

testthat::test_that("validate_n_levels passes when levels are within range", {
  x <- factor(c("a", "b", "c"))
  testthat::expect_silent(validate_n_levels(x, min_levels = 2, max_levels = 5, var_name = "x"))
})

testthat::test_that("validate_n_levels works with only min_levels", {
  x <- factor(c("a", "b"))
  testthat::expect_silent(validate_n_levels(x, min_levels = 2, max_levels = NULL, var_name = "x"))
  testthat::expect_error(
    validate_n_levels(x, min_levels = 3, max_levels = NULL, var_name = "x"),
    "needs minimum 3 levels"
  )
})

testthat::test_that("validate_n_levels works with only max_levels", {
  x <- factor(letters[1:5])
  testthat::expect_silent(validate_n_levels(x, min_levels = NULL, max_levels = 10, var_name = "x"))
  testthat::expect_error(
    validate_n_levels(x, min_levels = NULL, max_levels = 3, var_name = "x"),
    "needs maximum 3 level"
  )
})

testthat::test_that("validate_n_levels works with non-factor vectors", {
  x <- c("a", "b", "c")
  testthat::expect_silent(validate_n_levels(x, min_levels = 2, max_levels = 5, var_name = "x"))
})

testthat::test_that("validate_n_levels works when both min and max are NULL", {
  x <- factor(c("a", "b"))
  testthat::expect_silent(validate_n_levels(x, var_name = "x"))
})

