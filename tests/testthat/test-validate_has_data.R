data <- data.frame(x = 1:10, y = c(1:9, NA), z = c(Inf, 2:10))

testthat::test_that("validate_has_data throws no error when data has at least as many rows as min_nrow", {
  testthat::expect_silent(validate_has_data(data, 10))
  testthat::expect_silent(validate_has_data(data, 5))
})

testthat::test_that("validate_has_data throws error when min_nrow > #rows of data", {
  testthat::expect_error(validate_has_data(data, 11))
})

testthat::test_that("validate_has_data accepts logical complete argument", {
  testthat::expect_silent(validate_has_data(data[, c("x", "z")], 10, complete = TRUE))
  testthat::expect_silent(validate_has_data(data[, c("x", "z")], 10, complete = FALSE))
})

testthat::test_that("validate_has_data throws error when data has NA and complete is set to TRUE", {
  testthat::expect_error(validate_has_data(data[, c("x", "y")], 10, complete = TRUE))
})

testthat::test_that("validate_has_data accepts logical allow_inf argument", {
  testthat::expect_silent(validate_has_data(data[, c("x", "y")], 10, allow_inf = FALSE))
  testthat::expect_error(validate_has_data(data[, c("x", "y")], 10, complete = TRUE, allow_inf = FALSE))
})

testthat::test_that("validate_has_data accepts throws error when data has Inf values and allow_inf is set to FALSE", {
  testthat::expect_error(validate_has_data(data[, c("x", "z")], 10, allow_inf = FALSE))
})

testthat::test_that("validate_has_data accepts throws error when data has Inf values and allow_inf is set to FALSE", {
  testthat::expect_error(validate_has_data(data[, c("x", "z")], 10, allow_inf = FALSE))
  testthat::expect_error(validate_has_data(data[, c("x", "z")], 10, complete = TRUE, allow_inf = FALSE))
})

testthat::test_that("validate_has_data allow_inf argument ignores non-numeric columns", {
  data <- data.frame(x = 3:5, w = c("A", "B", "C"), z = c(Inf, 4, 5))
  testthat::expect_silent(validate_has_data(data[, c("x", "w")], 3, allow_inf = FALSE))
  testthat::expect_error(validate_has_data(data, 3, allow_inf = FALSE))
})

testthat::test_that("validate_has_data returns message when msg argument is set", {
  testthat::expect_error(
    validate_has_data(data, 11, msg = "Check data."),
    "Minimum number of records not met: >= 11 records required.\nCheck data."
  )
})

testthat::test_that("validate_has_data returns message msg argument is set and complete is set to TRUE", {
  testthat::expect_error(
    validate_has_data(data[, c("x", "y")], 11, complete = TRUE, msg = "Check data."),
    "Number of complete cases is less than: 11\nCheck data."
  )
})

testthat::test_that("validate_has_data returns throws error with non-character msg input", {
  testthat::expect_error(
    validate_has_data(data, 10, msg = 1),
    "Assertion on 'msg' failed: Must be of type 'string' \\(or 'NULL'\\), not 'double'"
  )

  testthat::expect_error(
    validate_has_data(data, 10, msg = TRUE),
    "Assertion on 'msg' failed: Must be of type 'string' \\(or 'NULL'\\), not 'logical'."
  )
})
