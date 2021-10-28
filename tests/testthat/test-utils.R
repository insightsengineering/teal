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

test_that("cast_to_list returns a list when data.frame, dataset or datasetConnector are passed", {
  dataset <- Dataset$new("iris", head(iris))
  dsc <- DatasetConnector$new("iris", CallableFunction$new(function() head(iris)))
  adsl_df <- iris

  class_dataset <- cast_to_list(dataset, names(dataset))
  expect_true(is(class_dataset, "list"))

  class_dsc <- cast_to_list(dsc, names(dsc))
  expect_true(is(class_dsc, "list"))

  class_df <- cast_to_list(adsl_df, "adsl_df")
  expect_true(is(class_df, "list"))
})

test_that("cast_to_list returns a list when a list is passed as input", {
  dataset <- list(Dataset$new("iris", head(iris)))
  dsc <- list(DatasetConnector$new("iris", CallableFunction$new(function() head(iris))))

  class_dataset <- cast_to_list(dataset, names(dataset))
  expect_true(is(class_dataset, "list"))

  class_dsc <- cast_to_list(dsc, names(dsc))
  expect_true(is(class_dsc, "list"))
})

test_that("cast_to_list throws error with missing data input", {
  expect_error(cast_to_list())
})

test_that("cast_to_list throws error with missing names_data input for dataframe only", {
  adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  expect_error(cast_to_list(adsl_df), "argument \"names_data\" is missing, with no default")

  dataset <- list(Dataset$new("iris", head(iris)))
  expect_error(cast_to_list(dataset), NA)
})

test_that("list_to_named_list throws error with a function returning a list", {
  fun <- function() list(head(iris))
  data_names_call <- substitute(fun())
  names_data <- deparse(substitute(fun()), width.cutoff = 500L)
  expect_error(cast_to_list(fun(), names_data), NA)
})

test_that("list_to_named_list returns named list when the input is a named list", {
  data_names_call <- substitute(Dataset$new("iris", head(iris)))
  names_data <- deparse(data_names_call, width.cutoff = 500L)
  data_list <- cast_to_list(Dataset$new("iris", head(iris)), names_data)
  names_data_list <- names(data_list)

  named_list <- list_to_named_list(data_list, names_data_list, data_names_call)
  expect_identical(names(named_list), "iris")
  expect_null(names(data_list))
  expect_identical(named_list$iris, data_list[[1]])
})

test_that("list_to_named_list returns named list when the input is not a named list", {
  #dataframe
  adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  data_names_call <- substitute(adsl_df)
  data_list <- cast_to_list(adsl_df, "adsl_df")
  names_data_list <- names(data_list)

  named_list <- list_to_named_list(data_list, names_data_list, data_names_call)
  expect_identical(names(named_list), "adsl_df")
  expect_identical(named_list$adsl_df, data_list[[1]])

  #dataset
  data_names_call <- substitute(Dataset$new("iris", head(iris)))
  names_data <- deparse(data_names_call, width.cutoff = 500L)
  data_list <- cast_to_list(Dataset$new("iris", head(iris)), names_data)
  names_data_list <- names(data_list)

  named_list <- list_to_named_list(data_list, names_data_list, data_names_call)
  expect_identical(names(named_list), "iris")
  expect_null(names(data_list))
  expect_identical(named_list$iris, data_list[[1]])
})

test_that("list_to_named_list throws error with missing input", {
  data_list <- list(1, 2, 3)
  expect_error(list_to_named_list())
  expect_error(list_to_named_list(data_list))
  expect_error(list_to_named_list(data_list, names(data_list)))
})

test_that("list_to_named_list throws error with function returning non-named list", {
  fun <- function() list(head(iris))
  data_names_call <- substitute(fun())
  names_data <- deparse(data_names_call, width.cutoff = 500L)
  data_list <- cast_to_list(fun(), names_data)
  names_data_list <- names(data_list)
  expect_error(
    list_to_named_list(data_list, names_data_list, data_names_call),
    "Unnamed lists shouldn't be provided as a result of a function call. Please use a named list."
  )
})

test_that("list_to_named_list works with a function returning a named list", {
  fun <- function() list(iris = head(iris))
  data_names_call <- substitute(fun())
  names_data <- deparse(data_names_call, width.cutoff = 500L)
  data_list <- cast_to_list(fun(), names_data)
  names_data_list <- names(data_list)
  expect_error(list_to_named_list(data_list, names_data_list, data_names_call), NA)
})

test_that("list_to_named_list throws error with function returning a non-fully named list", {
  fun <- function() list(iris = head(iris), head(mtcars))
  data_names_call <- substitute(fun())
  names_data <- deparse(data_names_call, width.cutoff = 500L)
  data_list <- cast_to_list(fun(), names_data)
  names_data_list <- names(data_list)
  expect_error(list_to_named_list(
    data_list, names_data_list, data_names_call),
    "Unnamed lists shouldn't be provided as a result of a function call. Please use a named list."
  )
})

test_that("named_list_to_dataset returns list of Dataset", {
  named_list <- list(df1 = data.frame(1, 2, 3, 4))
  ouput <- named_list_to_dataset(named_list)
  expect_equal(length(ouput), length(named_list))
  expect_is(ouput[[1]], "Dataset")
})

test_that("named_list_to_dataset returns list of Dataset", {
  named_list <- list(df1 = data.frame(1, 2, 3, 4))
  ouput <- named_list_to_dataset(named_list)
  expect_equal(length(ouput), length(named_list))
  expect_is(ouput, "list")
  expect_is(ouput[[1]], "Dataset")
})

test_that("named_list_to_dataset returns list of CDISCDataset", {
  adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))

  named_list <- list(ADSL = adsl_df)
  ouput <- named_list_to_dataset(named_list)
  expect_equal(length(ouput), length(named_list))
  expect_is(ouput[[1]], "CDISCDataset")
})

test_that("named_list_to_dataset throws error when the input is not a list of data.frame or Dataset", {
  expect_error(named_list_to_dataset(1))
  expect_error(named_list_to_dataset("A"))
  expect_error(named_list_to_dataset(TRUE))
  expect_error(named_list_to_dataset(c(1, 2, 3)))
})
