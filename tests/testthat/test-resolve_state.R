testthat::test_that("resolve_state returns an identical list if all fields are named", {
  state <- list(selected = "a", keep_na = FALSE)
  testthat::expect_identical(resolve_state(state), state)
})

testthat::test_that("resolve_state returns a list regardless of names", {
  state <- list(selected = "a", keep_na = FALSE, anyname = NA)
  testthat::expect_identical(resolve_state(state), state)
})

testthat::test_that("resolve_state do not change the class of any named field", {
  state <- list(selected = structure(1, class = "custom"), keep_na = "character", keep_inf = 1.0, extra = NA)
  testthat::expect_identical(resolve_state(state), state)
})

testthat::test_that("resolve_state changes a single, unnamed field into selected", {
  state <- list("a", keep_na = FALSE)
  testthat::expect_identical(resolve_state(state), list(selected = "a", keep_na = FALSE))
})

testthat::test_that("resolve_state throws when passed a list with one unnamed element and a `selected` element", {
  state <- list("a", selected = "test")
  testthat::expect_error(
    resolve_state(state),
    regexp = "Unnamed element of filter state cannot be intepreted as 'selected' because it already exists"
  )
})

testthat::test_that("resolve_state returns list with selected when vectors is provided", {
  state <- "a"
  testthat::expect_identical(resolve_state(state), list(selected = "a"))
})

testthat::test_that("resolve_state returns keep_na = TRUE if any element of the vector has NA", {
  state <- c("a", NA)
  testthat::expect_identical(resolve_state(state), list(selected = "a", keep_na = TRUE))
})

testthat::test_that("resolve_state returns keep_inf = TRUE if any element of the vector has Inf", {
  state <- c(1, Inf)
  testthat::expect_identical(resolve_state(state), list(selected = 1, keep_inf = TRUE))
})

testthat::test_that("resolve_state for default filter results in empty list", {
  testthat::expect_identical(resolve_state(default_filter()), list())
})

testthat::test_that("resolve_state throws when passed a list with more than one unnamed element", {
  state <- list(1, 2)
  testthat::expect_error(resolve_state(state), regexp = "More than one element of filter state is unnamed")
})
