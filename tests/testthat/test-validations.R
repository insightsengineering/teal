# Tests for validation functions in validations.R

# validate_one_row_per_id ----

testthat::test_that("validate_one_row_per_id does not error when all rows are unique for given key", {
  data <- data.frame(
    USUBJID = c("A", "B", "C"),
    STUDYID = c("1", "1", "1"),
    value = 1:3
  )
  testthat::expect_silent(validate_one_row_per_id(data, key = c("USUBJID", "STUDYID")))
})

testthat::test_that("validate_one_row_per_id errors when there are duplicate rows for given key", {
  data <- data.frame(
    USUBJID = c("A", "A", "C"),
    STUDYID = c("1", "1", "1"),
    value = 1:3
  )
  testthat::expect_error(
    validate_one_row_per_id(data, key = c("USUBJID", "STUDYID")),
    "Found more than one row per id"
  )
})

testthat::test_that("validate_one_row_per_id works with custom key column", {
  data <- data.frame(
    id = c("A", "B", "C"),
    value = 1:3
  )
  testthat::expect_silent(validate_one_row_per_id(data, key = "id"))
})

# validate_in ----

testthat::test_that("validate_in does not error when all elements are in choices", {
  testthat::expect_silent(validate_in(c("a", "b"), c("a", "b", "c"), "msg"))
  testthat::expect_silent(validate_in("a", c("a", "b", "c"), "msg"))
})

testthat::test_that("validate_in errors when some elements are not in choices", {
  testthat::expect_error(
    validate_in(c("a", "d"), c("a", "b", "c"), "Species does not exist."),
    "Species does not exist."
  )
})

testthat::test_that("validate_in errors when x has length 0", {
  testthat::expect_error(
    validate_in(character(0), c("a", "b", "c"), "Empty selection"),
    "Empty selection"
  )
})

testthat::test_that("validate_in errors when choices has length 0", {
  testthat::expect_error(
    validate_in(c("a"), character(0), "No choices available"),
    "No choices available"
  )
})

# validate_has_elements ----

testthat::test_that("validate_has_elements does not error when vector has elements", {
  testthat::expect_silent(validate_has_elements(1:10, "msg"))
  testthat::expect_silent(validate_has_elements(c("a", "b"), "msg"))
})

testthat::test_that("validate_has_elements errors when vector is empty", {
  testthat::expect_error(
    validate_has_elements(numeric(0), "No subjects in strata"),
    "No subjects in strata"
  )
  testthat::expect_error(
    validate_has_elements(character(0), "Empty vector"),
    "Empty vector"
  )
})

# validate_no_intersection ----

testthat::test_that("validate_no_intersection does not error when vectors don't intersect", {
  testthat::expect_silent(validate_no_intersection(1:5, 6:10, "msg"))
  testthat::expect_silent(validate_no_intersection(c("a", "b"), c("c", "d"), "msg"))
})

testthat::test_that("validate_no_intersection errors when vectors intersect", {
  testthat::expect_error(
    validate_no_intersection(1:5, 3:8, "Vectors overlap"),
    "Vectors overlap"
  )
  testthat::expect_error(
    validate_no_intersection(c("a", "b", "c"), c("c", "d"), "Common elements found"),
    "Common elements found"
  )
})

# validate_has_variable ----

testthat::test_that("validate_has_variable does not error when variable exists in data", {
  data <- data.frame(a = 1:5, b = 6:10)
  testthat::expect_silent(validate_has_variable(data, "a"))
  testthat::expect_silent(validate_has_variable(data, c("a", "b")))
})

testthat::test_that("validate_has_variable errors when variable does not exist in data", {
  data <- data.frame(a = 1:5, b = 6:10)
  testthat::expect_error(
    validate_has_variable(data, "c"),
    "data does not have the required variables: c"
  )
  testthat::expect_error(
    validate_has_variable(data, c("a", "c", "d")),
    "data does not have the required variables: c, d"
  )
})

testthat::test_that("validate_has_variable accepts custom error message", {
  data <- data.frame(a = 1:5, b = 6:10)
  testthat::expect_error(
    validate_has_variable(data, "c", msg = "Custom error message"),
    "Custom error message"
  )
})

testthat::test_that("validate_has_variable does not error when varname is empty", {
  data <- data.frame(a = 1:5, b = 6:10)
  testthat::expect_silent(validate_has_variable(data, character(0)))
})

# validate_n_levels ----

testthat::test_that("validate_n_levels does not error when levels are within range", {
  testthat::expect_silent(validate_n_levels(factor(c("a", "b")), min_levels = 1, max_levels = 5, var_name = "x"))
  testthat::expect_silent(validate_n_levels(c(1, 2, 3), min_levels = 2, max_levels = 4, var_name = "x"))
})

testthat::test_that("validate_n_levels errors when levels are below minimum", {
  testthat::expect_error(
    validate_n_levels(factor(c("a")), min_levels = 2, max_levels = 5, var_name = "x"),
    "x variable needs minimum 2 level\\(s\\) and maximum 5 level\\(s\\)"
  )
})

testthat::test_that("validate_n_levels errors when levels are above maximum", {
  testthat::expect_error(
    validate_n_levels(factor(c("a", "b", "c", "d")), min_levels = 1, max_levels = 3, var_name = "x"),
    "x variable needs minimum 1 level\\(s\\) and maximum 3 level\\(s\\)"
  )
})

testthat::test_that("validate_n_levels works with only min_levels specified", {
  testthat::expect_silent(validate_n_levels(factor(c("a", "b", "c")), min_levels = 2, max_levels = NULL, var_name = "x"))
  testthat::expect_error(
    validate_n_levels(factor(c("a")), min_levels = 2, max_levels = NULL, var_name = "x"),
    "x variable needs minimum 2 levels\\(s\\)"
  )
})

testthat::test_that("validate_n_levels works with only max_levels specified", {
  testthat::expect_silent(validate_n_levels(factor(c("a", "b")), min_levels = NULL, max_levels = 5, var_name = "x"))
  testthat::expect_error(
    validate_n_levels(factor(c("a", "b", "c", "d", "e", "f")), min_levels = NULL, max_levels = 5, var_name = "x"),
    "x variable needs maximum 5 level\\(s\\)"
  )
})

testthat::test_that("validate_n_levels works with non-factor variables", {
  testthat::expect_silent(validate_n_levels(c(1, 2, 3, 3), min_levels = 2, max_levels = 5, var_name = "x"))
  testthat::expect_error(
    validate_n_levels(c(1, 1, 1), min_levels = 2, max_levels = 5, var_name = "x"),
    "x variable needs minimum 2 level\\(s\\) and maximum 5 level\\(s\\)"
  )
})
