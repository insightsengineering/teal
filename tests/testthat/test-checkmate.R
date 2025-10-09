# Tests for checkmate.R functions

# check_reactive ----

testthat::test_that("check_reactive returns TRUE for reactive objects", {
  r <- shiny::reactive({ 1 + 1 })
  testthat::expect_true(check_reactive(r))
})

testthat::test_that("check_reactive returns error message for non-reactive objects", {
  testthat::expect_match(
    check_reactive(function() { 1 + 1 }),
    "Must be a reactive.*but has class.*function"
  )
  testthat::expect_match(
    check_reactive(42),
    "Must be a reactive.*but has class.*numeric"
  )
})

testthat::test_that("check_reactive handles NULL when null.ok = TRUE", {
  testthat::expect_true(check_reactive(NULL, null.ok = TRUE))
})

testthat::test_that("check_reactive returns error for NULL when null.ok = FALSE", {
  testthat::expect_match(
    check_reactive(NULL, null.ok = FALSE),
    "Must be a reactive.*but has class.*NULL"
  )
})

# test_reactive ----

testthat::test_that("test_reactive returns TRUE for reactive objects", {
  r <- shiny::reactive({ 1 + 1 })
  testthat::expect_true(test_reactive(r))
})

testthat::test_that("test_reactive returns FALSE for non-reactive objects", {
  testthat::expect_false(test_reactive(function() { 1 + 1 }))
  testthat::expect_false(test_reactive(42))
})

testthat::test_that("test_reactive handles NULL correctly", {
  testthat::expect_false(test_reactive(NULL, null.ok = FALSE))
  testthat::expect_true(test_reactive(NULL, null.ok = TRUE))
})

# assert_reactive ----

testthat::test_that("assert_reactive does not error for reactive objects", {
  r <- shiny::reactive({ 1 + 1 })
  testthat::expect_silent(assert_reactive(r))
})

testthat::test_that("assert_reactive throws error for non-reactive objects", {
  testthat::expect_error(
    assert_reactive(function() { 1 + 1 }),
    "Assertion.*failed.*Must be a reactive"
  )
  testthat::expect_error(
    assert_reactive(42),
    "Assertion.*failed.*Must be a reactive"
  )
})

testthat::test_that("assert_reactive handles NULL correctly", {
  testthat::expect_error(
    assert_reactive(NULL, null.ok = FALSE),
    "Assertion.*failed.*Must be a reactive"
  )
  testthat::expect_silent(assert_reactive(NULL, null.ok = TRUE))
})

# decorate_err_msg ----

testthat::test_that("decorate_err_msg returns x when no error occurs", {
  testthat::expect_equal(decorate_err_msg(1 + 1), 2)
  testthat::expect_equal(decorate_err_msg("hello"), "hello")
})

testthat::test_that("decorate_err_msg decorates error message with pre", {
  testthat::expect_error(
    decorate_err_msg(stop("original error"), pre = "PREFIX:"),
    "PREFIX:.*original error"
  )
})

testthat::test_that("decorate_err_msg decorates error message with post", {
  testthat::expect_error(
    decorate_err_msg(stop("original error"), post = "SUFFIX"),
    "original error.*SUFFIX"
  )
})

testthat::test_that("decorate_err_msg decorates error message with both pre and post", {
  testthat::expect_error(
    decorate_err_msg(stop("original error"), pre = "PREFIX:", post = "SUFFIX"),
    "PREFIX:.*original error.*SUFFIX"
  )
})

testthat::test_that("decorate_err_msg works with empty pre and post", {
  testthat::expect_error(
    decorate_err_msg(stop("original error"), pre = character(0), post = character(0)),
    "original error"
  )
})
