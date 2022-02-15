method <- function(a, b, ..., stop = TRUE, allowed_args = character()) {
  check_ellipsis(..., stop = stop, allowed_args = allowed_args)
}

test_that("check_ellipsis with no unused", {
  expect_silent(method(a = 1, b = 2))
})

test_that("check_ellipsis with extra unamed arguments", {
  expect_error(method(a = 1, b = 2, 5, 6), "2 total unused argument\\(s\\)\\.")
  expect_warning(method(a = 1, b = 2, 5, 6, stop = FALSE), "2 total unused argument\\(s\\)\\.")
})

test_that("check_ellipsis with extra named arguments", {
  expect_error(method(a = 1, b = 2, c = 5, d = 6), "2 total unused argument\\(s\\)\\. 2 with name\\(s\\): c, d\\.")
  expect_warning(
    method(a = 1, b = 2, c = 5, d = 6, stop = FALSE),
    "2 total unused argument\\(s\\)\\. 2 with name\\(s\\): c, d\\."
  )
})

test_that("check_ellipsis with extra named and unamed arguments", {
  expect_error(method(a = 1, b = 2, c = 5, 6), "2 total unused argument\\(s\\). 1 with name\\(s\\): c\\.")
  expect_warning(
    method(a = 1, b = 2, c = 5, 6, stop = FALSE),
    "2 total unused argument\\(s\\). 1 with name\\(s\\): c\\."
  )
})

test_that("check_ellipsis with extra named and unamed arguments in wrong order", {
  expect_error(method(c = 5, 6, a = 1, b = 2), "2 total unused argument\\(s\\)\\. 1 with name\\(s\\): c\\.")
  expect_warning(
    method(a = 1, c = 5, b = 2, 6, stop = FALSE),
    "2 total unused argument\\(s\\)\\. 1 with name\\(s\\): c\\."
  )
})

test_that("check_ellipsis with allowed args", {
  expect_silent(method(a = 1, b = 2, z = 3, allowed_args = c("z")))
  expect_error(
    method(a = 1, b = 2, y = 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): y\\."
  )
  expect_error(
    method(a = 1, b = 2, y = 3, z = 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): y\\."
  )
  expect_error(
    method(a = 1, b = 2, 3, allowed_args = c("z")),
    "1 total unused argument\\(s\\)\\."
  )
  expect_silent(method(a = 1, b = 2, 3, allowed_args = c("")))
  expect_error(
    method(a = 1, b = 2, 3, z = 9, allowed_args = c("")),
    "1 total unused argument\\(s\\)\\. 1 with name\\(s\\): z\\."
  )
  expect_silent(method(a = 1, b = 2, 3, z = 5, allowed_args = c("", "z", "y")))
  expect_silent(method(a = 1, b = 2, 3, z = 5, y = 4, allowed_args = c("", "z", "y")))
  expect_silent(method(a = 1, b = 2, allowed_args = c("", "z", "y")))
})
