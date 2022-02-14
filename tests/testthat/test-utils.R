test_that("check_pkg_quietly works", {
  expect_error(check_pkg_quietly("my_random_package_1234567890", "my message"), regexp = "my message")
})

test_that("get_key_duplicates_util function", {
  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5)),
    stringsAsFactors = TRUE
  )
  keys <- c("a", "b")

  # Input validations
  expect_error(get_key_duplicates_util(df, NULL))
  expect_error(get_key_duplicates_util("test", keys))
  expect_error(get_key_duplicates_util(df, c(1, 2, 3)))
  expect_error(get_key_duplicates_util(df, keys = c("a", "test")))

  expect_silent(get_key_duplicates_util(df, keys))

  expect_true(dplyr::all_equal(
    dplyr::tibble(a = factor("b", levels = c("a", "b", "c")), b = 3, rows = "3,4", n = 2L),
    get_key_duplicates_util(df, keys)
  ))

  # Expect empty tibble if there are no duplicated key values
  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 4, 5), c = c(1, 2, 3, 4, 5)),
    stringsAsFactors = TRUE
  )
  expect_true(dplyr::all_equal(
    dplyr::tibble(a = factor(x = NULL, levels = c("a", "b", "c")), b = double(0), rows = character(0), n = integer(0)),
    get_key_duplicates_util(df, keys)
  ))
})
