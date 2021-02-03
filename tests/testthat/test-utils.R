test_that("is in operator", {
  # if both are vectors then it behaves like normal %in%
  expect_identical(c("a", "b") %is_in% c("a", "b", "c"), c("a", "b") %in% c("a", "b", "c"))
  expect_identical(c("a", "b", "d") %is_in% c("a", "b", "c"), c("a", "b", "d") %in% c("a", "b", "c"))


  # if one is a list it looks for same vector
  expect_true(all(c("a", "b", "c") %is_in% list(c("a", "b", "c"))))
  expect_false(all(c("a", "b") %is_in% list("a", "b", "c")))

  vec1 <- c("a", "b")
  list1 <- list(c("a", "b"), c("b", "d"))
  list2 <- list(c("a"), c("b", "d"))
  list3 <- list(c("a", "b"), c("b", "d"), c("a", "d"))

  expect_true(all(vec1 %is_in% list1))
  expect_false(all(vec1 %is_in% list2))
  expect_true(all(list1 %is_in% list3))

})

test_that("check_pkg_quietly works", {
  expect_error(check_pkg_quietly("my_random_package_1234567890", "my message"), regexp = "my message")
})

test_that("get_key_duplicates_util function", {
  df <- as.data.frame(list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5)))
  keys <- c("a", "b")

  # Input validations
  expect_error(get_key_duplicates_util(df, NULL))
  expect_error(get_key_duplicates_util("test", keys))
  expect_error(get_key_duplicates_util(df, c(1, 2, 3)))
  expect_error(get_key_duplicates_util(df, keys = c("a", "test")))

  expect_silent(get_key_duplicates_util(df, keys))

  expect_true(dplyr::all_equal(
    get_key_duplicates_util(df, keys),
    dplyr::tibble(a = factor("b", levels = c("a", "b", "c")), b = 3, rows = "3,4", n = 2L)
  ))

  # Expect empty tibble if there are no duplicated key values
  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 4, 5), c = c(1, 2, 3, 4, 5))
  )
  expect_true(dplyr::all_equal(
    get_key_duplicates_util(df, keys),
    dplyr::tibble(a = factor(levels = c("a", "b", "c")), b = double(0), rows = character(0), n = integer(0))
  ))
})

test_that("get_key_duplicates", {
  teal_keys <- teal::keys(primary = c("a", "b"), foreign = NULL, parent = NULL)
  char_keys <- c("a", "b")
  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
  )
  attr(df, "primary_key") <- c("a", "b")

  rel_df <- RelationalDataset$new("test", df, teal::keys(primary = "c", foreign = NULL, parent = NULL))

  expect_silent(get_key_duplicates(rel_df)) # uses the keys in get_keys()$primary
  expect_silent(get_key_duplicates(rel_df, teal_keys)) # uses the teal_keys
  expect_silent(get_key_duplicates(rel_df, char_keys)) # uses the provided keys
  expect_silent(get_key_duplicates(df, char_keys)) # uses the provided keys
  expect_silent(get_key_duplicates(df, teal_keys)) # uses the provided keys
  expect_silent(get_key_duplicates(df))  # uses the primary key attribute of df

  # The class of the original object and type of keys does not matter for the result
  expect_true(dplyr::all_equal(
    get_key_duplicates(rel_df, teal_keys),
    get_key_duplicates(df, char_keys)
  ))
})
