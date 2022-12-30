
testthat::test_that("rule function yields closure", {
  rule_function <- function(value) if (!value %in% c("a", "b")) "value must be \"a\" or \"b\""
  go <- TRUE
  testthat::expect_type(crule(rule_function, isTRUE(go)), "closure")
})

testthat::test_that("rule formula yields closure", {
  rule_formula <- ~ if (!. %in% c("a", "b")) "value must be \"a\" or \"b\""
  go <- TRUE
  testthat::expect_type(crule(rule_formula, isTRUE(go)), "closure")
})

testthat::test_that("resulting function is different to input function", {
  rule_function <- function(value) if (!value %in% c("a", "b")) "value must be \"a\" or \"b\""
  go <- TRUE
  result_function <- crule(rule_function, isTRUE(go))
  testthat::expect_failure(testthat::expect_identical(rule_function, result_function))
})

testthat::test_that("resulting function contains input function", {
  rule_function <- function(value) if (!value %in% c("a", "b")) "value must be \"a\" or \"b\""
  go <- TRUE
  result_function <- crule(rule_function, isTRUE(go))
  testthat::expect_true(any(body(rule_function) == as.list(body(result_function))))
})

testthat::test_that("resulting function evaluates value", {
  rule_function <- function(value) if (!value %in% c("a", "b")) "value must be \"a\" or \"b\""
  go <- TRUE
  result_function <- crule(rule_function, isTRUE(go))
  testthat::expect_null(result_function("a"))
  testthat::expect_type(result_function("c"), "character")
})

testthat::test_that("evaluation depends on condition", {
  rule_function <- function(value) if (!value %in% c("a", "b")) "value must be \"a\" or \"b\""
  go <- TRUE
  result_function <- crule(rule_function, isTRUE(go))
  testthat::expect_null(result_function("a"))
  testthat::expect_type(result_function("c"), "character")
  go <- FALSE
  testthat::expect_null(result_function("a"))
  testthat::expect_null(result_function("c"))
})

testthat::test_that("bad arguments raise errors", {
  rule_function <- function(value) if (!value %in% c("a", "b")) "value must be \"a\" or \"b\""
  go <- TRUE
  testthat::expect_error(crule("rule_function", isTRUE(go)))
  testthat::expect_error(crule(rule_function, go))
})
