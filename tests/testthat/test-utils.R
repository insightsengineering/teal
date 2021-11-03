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

dataset_1 <- Dataset$new("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl_dataset <- CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))
mods <- teal:::get_dummy_modules()

test_that("to_relational_data accepts data.frame as input", {
  head_iris <- head(iris)
  output <- to_relational_data(head_iris)
  testthat::expect_error(output, NA)
  testthat::expect_is(output, "RelationalData")
})

test_that("to_relational_data accepts cdisc data.frame as input", {
  output <- to_relational_data(adsl_df)
  testthat::expect_error(output, NA)
  testthat::expect_is(output, "RelationalData")
})

test_that("to_relational_data accepts Dataset/CDISCDataset as input", {
  adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adsl_dataset <- CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))

  output_dataset <- to_relational_data(dataset_1)
  testthat::expect_error(output_dataset, NA)
  testthat::expect_is(output_dataset, "RelationalData")

  output_cdisc_dataset <- to_relational_data(adsl_dataset)
  testthat::expect_error(output_cdisc_dataset, NA)
  testthat::expect_is(output_cdisc_dataset, "RelationalData")
})

test_that("to_relational_data accepts DatasetConnector as input", {
  dsc1 <- DatasetConnector$new("iris", CallableFunction$new(function() head(iris)))
  output_datasetconnector <- to_relational_data(dsc1)
  testthat::expect_error(output_datasetconnector, NA)
  testthat::expect_is(output_datasetconnector, "RelationalData")
  testthat::expect_identical(output_datasetconnector$get_datanames(), "iris")
})

test_that("to_relational_data accepts an unnamed list of data.frame as input", {
  output_dataset_list <- to_relational_data(list(head(iris)))
  testthat::expect_error(output_dataset_list, NA)
  testthat::expect_is(output_dataset_list, "RelationalData")
  testthat::expect_identical(output_dataset_list$get_datanames(), "head(iris)")
})

test_that("to_relational_data accepts a named list of data.frame as input", {
  output_dataset_list <- to_relational_data(list(AA = head(iris)))
  testthat::expect_error(output_dataset_list, NA)
  testthat::expect_is(output_dataset_list, "RelationalData")
  testthat::expect_identical(output_dataset_list$get_datanames(), "AA")
})

test_that("to_relational_data accepts a mixed named list of data.frame as input", {
  output_dataset_list <- to_relational_data(list(AA = head(iris), head(mtcars)))
  testthat::expect_error(output_dataset_list, NA)
  testthat::expect_is(output_dataset_list, "RelationalData")
  testthat::expect_identical(output_dataset_list$get_datanames(), c("AA", "head(mtcars)"))
})

test_that("to_relational_data accepts a complete named list of data.frame as input", {
  output_dataset_list <- to_relational_data(list(AA = head(iris), BB = head(mtcars)))
  testthat::expect_error(output_dataset_list, NA)
  testthat::expect_is(output_dataset_list, "RelationalData")
  testthat::expect_identical(output_dataset_list$get_datanames(), c("AA", "BB"))
})

test_that("to_relational_data accepts a mixed named list of objects as input", {
  dataset_22 <- Dataset$new("iris22", head(iris))
  dsc1 <- DatasetConnector$new("dsc1", CallableFunction$new(function() head(iris)))

  output_dataset_list <- to_relational_data(list(AA = head(iris), dataset_22))
  testthat::expect_error(output_dataset_list, NA)
  testthat::expect_is(output_dataset_list, "RelationalData")
  testthat::expect_identical(output_dataset_list$get_datanames(), c("AA", "iris22"))

  output_dataset_list2 <- to_relational_data(list(AA = head(iris), dataset, mtcars, dsc1))
  testthat::expect_error(output_dataset_list2, NA)
  testthat::expect_is(output_dataset_list2, "RelationalData")
  testthat::expect_identical(output_dataset_list2$get_datanames(), c("AA", "iris22", "mtcars", "dsc1"))
})

test_that("to_relational_data accepts a function returning a named list as input", {
  fun <- function() {list(AA = ADSL, BB = ADSL)}

  output_dataset_fun <- to_relational_data(fun())
  testthat::expect_error(output_dataset_fun, NA)
  testthat::expect_is(output_dataset_fun, "RelationalData")
  testthat::expect_identical(output_dataset_fun$get_datanames(), c("AA", "BB"))
})

test_that("to_relational_data accepts a function returning a Dataset as input", {
  fun <- function() {cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl")}

  output_dataset_fun <- to_relational_data(fun())
  testthat::expect_error(output_dataset_fun, NA)
  testthat::expect_is(output_dataset_fun, "RelationalData")
  testthat::expect_identical(output_dataset_fun$get_datanames(), "ADSL")
})

test_that("to_relational_data throws error with a function returning data.frame", {
  fun <- function() iris

  testthat::expect_error(
    to_relational_data(fun()),
    "Single data.frame shouldn't be provided as a result of a function call. Please name
         the object first or use a named list.")
})

test_that("to_relational_data throws error with a function returning a non-named list", {
  fun <- function() list(iris, mtcars)

  testthat::expect_error(
    to_relational_data(fun()),
    "Unnamed lists shouldn't be provided as input for data. Please use a named list.")
})

test_that("to_relational_data throws error with a function returning a semi-named list", {
  fun <- function() list(iris = iris, mtcars)

  testthat::expect_error(
    to_relational_data(fun()),
    "Unnamed lists shouldn't be provided as input for data. Please use a named list.")
})

test_that("to_relational_data throws error with a multiple functions returning data.frame", {
  fun_iris <- function() iris
  fun_mtcars <- function() mtcars

  testthat::expect_error(
    to_relational_data(setNames(nm = c("AA"), list(fun_iris(), fun_mtcars()))),
    "Unnamed lists shouldn't be provided as input for data. Please use a named list.")
})
