## Dataset =====
testthat::test_that("Dataset basics", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  testthat::expect_silent({
    test_ds <- Dataset$new(
      dataname = "testds",
      x = x,
      keys = "x"
    )
  })

  testthat::expect_equal(
    get_keys(test_ds),
    "x"
  )

  testthat::expect_silent(set_keys(test_ds, "y"))
  testthat::expect_equal(
    get_keys(test_ds),
    "y"
  )

  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
  )
  # keys checking is not immediate
  ds1 <- dataset(dataname = "df", x = df, keys = character(0))
  testthat::expect_silent(ds1$check_keys())

  ds2 <- dataset(dataname = "df", x = df, keys = character(0)) %>% set_keys(c("c"))
  testthat::expect_silent(ds2$check_keys())

  ds3 <- dataset(dataname = "df", x = df) %>% set_keys("non_existing_col")
  testthat::expect_error(
    ds3$check_keys(),
    "Primary keys specifed for df do not exist in the data."
  )

  ds4 <- dataset(dataname = "df", x = df) %>% set_keys("a")
  testthat::expect_error(
    ds4$check_keys(),
    "Duplicate primary key values found in the dataset 'df'"
  )
})

testthat::test_that("Dataset$recreate", {
  ds <- Dataset$new(
    dataname = "mtcars",
    x = mtcars,
    keys = character(0),
    code = "mtcars",
    label = character(0),
    vars = list())
  ds2 <- ds$recreate()

  testthat::expect_identical(ds, ds2)
})

testthat::test_that("Dataset$get_*_colnames", {
  df <- as.data.frame(
    list(
      num = c(1, 2, 3),
      char = as.character(c("a", "b", "c")),
      fac = factor(x = c("lev1", "lev2", "lev1"), levels = c("lev1", "lev2"))
    ),
    stringsAsFactors = FALSE
  )
  ds <- Dataset$new("ds", x = df)

  testthat::expect_equal(ds$get_numeric_colnames(), c("num"))
  testthat::expect_equal(ds$get_character_colnames(), c("char"))
  testthat::expect_equal(ds$get_factor_colnames(), c("fac"))
})

testthat::test_that("Dataset$get_rownames", {
  df <- as.data.frame(
    list(
      num = c(1, 2, 3),
      char = as.character(c("a", "b", "c")),
      fac = factor(x = c("lev1", "lev2", "lev1"), levels = c("lev1", "lev2"))
    ),
    stringsAsFactors = FALSE
  )
  ds <- Dataset$new("ds", x = df)

  testthat::expect_equal(ds$get_rownames(), c("1", "2", "3"))
})

testthat::test_that("Dataset active bindings", {
  df <- as.data.frame(
    list(
      num = c(1, 2, 3),
      char = as.character(c("a", "b", "c")),
      fac = factor(x = c("lev1", "lev2", "lev1"), levels = c("lev1", "lev2")),
      num2 = c(3, 4, 5)
    ),
    stringsAsFactors = FALSE
  )
  ds <- Dataset$new("ds", x = df)

  testthat::expect_equal(ds$ncol, 4)
  testthat::expect_equal(ds$nrow, 3)
  testthat::expect_equal(ds$dim, c(3, 4))
  testthat::expect_equal(ds$colnames, c("num", "char", "fac", "num2"))
  testthat::expect_equal(ds$rownames, c("1", "2", "3"))
  testthat::expect_equal(
    ds$raw_data,
    as.data.frame(
      list(
        num = c(1, 2, 3),
        char = as.character(c("a", "b", "c")),
        fac = factor(x = c("lev1", "lev2", "lev1"), levels = c("lev1", "lev2")),
        num2 = c(3, 4, 5)
      ),
      stringsAsFactors = FALSE
    )
  )
  testthat::expect_equal(ds$var_names, ds$colnames)
  testthat::expect_true(is.null(ds$row_labels))

  # Depreciation warnings
  labs <- ds$get_column_labels()
  exp <- as.character(rep(NA, 4))
  names(exp) <- c("num", "char", "fac", "num2")
  testthat::expect_equal(labs, exp)
})

testthat::test_that("Dataset supplementary constructors", {
  file_example <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "library(teal)

      # code>
      x <- iris
      x$a1 <- 1
      x$a2 <- 2

      # <code
      dataset(dataname = \"iris_mod\", x = x)"
    ),
    con = file_example
  )
  testthat::expect_silent(x <- dataset_file(file_example))

  # Not a Dataset object causes an error
  file_example2 <- tempfile(fileext = "2.R")
  writeLines(
    text = c(
      "iris"
    ),
    con = file_example2
  )
  testthat::expect_error(
    x <- dataset_file(file_example2),
    regexp = "The object returned from the file is not of Dataset class.",
    fixed = TRUE
  )

  # Deprecation warnings
  testthat::expect_warning(x2 <- named_dataset_file(file_example))
  testthat::expect_warning(x3 <- relational_dataset_file(file_example))
  testthat::expect_equal(x, x2)
  testthat::expect_equal(x, x3)

  # Deprecated constructors
  testthat::expect_error(raw_dataset(iris))
  testthat::expect_warning(ds1 <- named_dataset("ds", iris))
  testthat::expect_warning(ds2 <- relational_dataset("ds", iris))
  testthat::expect_equal(ds1, ds2)
})

testthat::test_that("Dataset$set_vars throws an error if passed the enclosing Dataset object directly", {
  test_ds <- Dataset$new("mtcars", mtcars)
  testthat::expect_error(test_ds$set_vars(vars = list(itself = test_ds)), regexp = "Circular dependencies detected")
})

testthat::test_that("Dataset$set_vars throws an error if passed the enclosing Dataset object indirectly, distance 1", {
  test_ds0 <- Dataset$new("mtcars", mtcars)
  test_ds1 <- Dataset$new("iris", iris)
  test_ds1$set_vars(vars = list(test_ds0 = test_ds0))
  testthat::expect_error(test_ds0$set_vars(vars = list(test_ds1 = test_ds1)), regexp = "Circular dependencies detected")
})

testthat::test_that("Dataset$set_vars throws an error if passed the enclosing Dataset object indirectly, distance 2", {
  test_ds0 <- Dataset$new("mtcars", mtcars)
  test_ds1 <- Dataset$new("iris", iris)
  test_ds2 <- Dataset$new("rock", rock)
  test_ds1$set_vars(vars = list(test_ds0 = test_ds0))
  test_ds2$set_vars(vars = list(test_ds1 = test_ds1))

  testthat::expect_error(test_ds0$set_vars(vars = list(test_ds2 = test_ds2)), regexp = "Circular dependencies detected")
})

testthat::test_that("Dataset$set_vars throws an error if passed the enclosing DatasetConnector", {
  test_ds0 <- Dataset$new("mtcars", mtcars)
  test_ds1 <- Dataset$new("iris", iris)
  test_ds2 <- Dataset$new("rock", rock)
  test_ds1$set_vars(vars = list(test_ds0 = test_ds0))
  test_ds2$set_vars(vars = list(test_ds1 = test_ds1))

  testthat::expect_error(mutate_dataset(
    test_ds0, code = "mtcars$new_var <- rock$perm[1]", vars = list(test_ds2 = test_ds2)
    ),
    regexp = "Circular dependencies detected")

  pull_fun2 <- callable_function(data.frame)
  pull_fun2$set_args(args = list(a = c(1, 2, 3)))
  t_dc <- dataset_connector("test", pull_fun2, vars = list(test_ds0 = test_ds0))
  testthat::expect_error(test_ds0$set_vars(vars = list(t_dc = t_dc)), regexp = "Circular dependencies detected")
  mutate_dataset(t_dc, code = "test$new_var <- iris$Species[1]", vars = list(test_ds1 = test_ds1))
  testthat::expect_error(
    mutate_dataset(test_ds0, code = "mtcars$new_var <- t_dc$a[1]", vars = list(t_dc = t_dc)),
    regexp = "Circular dependencies detected"
  )
})

testthat::test_that("Dataset mutate method with delayed logic", {
  test_ds0 <- Dataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  test_ds1 <- Dataset$new("head_iris", head(iris), code = "head_iris <- head(iris)")
  test_ds2 <- Dataset$new("head_rock", head(rock), code = "head_rock <- head(rock)")

  pull_fun2 <- callable_function(data.frame)
  pull_fun2$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun2, vars = list(test_ds1 = test_ds1))

  testthat::expect_false(test_ds0$is_mutate_delayed())
  testthat::expect_equal(test_ds0$get_code(), "head_mtcars <- head(mtcars)")

  mutate_dataset(test_ds0, code = "head_mtcars$carb <- head_mtcars$carb * 2")
  testthat::expect_equal(get_raw_data(test_ds0)$carb, 2 * head(mtcars)$carb)
  testthat::expect_false(test_ds0$is_mutate_delayed())
  testthat::expect_equal(test_ds0$get_code(), "head_mtcars <- head(mtcars)\nhead_mtcars$carb <- head_mtcars$carb * 2")

  mutate_dataset(test_ds0, code = "head_mtcars$Species <- ds1$Species", vars = list(ds1 = test_ds1))
  testthat::expect_false(test_ds0$is_mutate_delayed())
  testthat::expect_equal(get_raw_data(test_ds0)$Species, get_raw_data(test_ds1)$Species)
  testthat::expect_equal(
    pretty_code_string(test_ds0$get_code()),
    c("head_iris <- head(iris)",
      "ds1 <- head_iris",
      "head_mtcars <- head(mtcars)",
      "head_mtcars$carb <- head_mtcars$carb * 2",
      "head_mtcars$Species <- ds1$Species"
    )
  )

  testthat::expect_message(
    mutate_dataset(test_ds0, code = "head_mtcars$head_letters <- dc$head_letters", vars = list(dc = t_dc)),
    regexp = "Mutation is delayed"
  )
  repeated_call <- function() {
    testthat::expect_equal(
      testthat::expect_message(
        test_ds0$get_code(),
        regexp = "The output includes mutate code that are delayed"
      ),
      paste(test_ds0$get_code_class()$append(test_ds0$get_mutate_code_class())$get_code(deparse = TRUE))
    )
  }
  repeated_call()
  repeated_message <-
    "There are mutate statements that are delayed. Returned data may \\(or may not\\) reflect the mutations."
  testthat::expect_message(
    testthat::expect_null(get_raw_data(test_ds0)$head_mtcars),
    regexp = repeated_message
  )
  testthat::expect_true(test_ds0$is_mutate_delayed())
  repeated_call()

  # continuing to delay
  testthat::expect_message(
    mutate_dataset(test_ds0, code = "head_mtcars$new_var <- 1"),
    regexp = "Mutation is delayed"
  )
  repeated_call()
  testthat::expect_message(
    expect_null(get_raw_data(test_ds0)$new_var),
    regexp = repeated_message
    )
  testthat::expect_true(test_ds0$is_mutate_delayed())

  testthat::expect_message(
    mutate_dataset(test_ds0, code = "head_mtcars$perm <- ds2$perm", vars = list(ds2 = test_ds2)),
    regexp = "Mutation is delayed"
  )
  repeated_call()
  testthat::expect_message(
    expect_null(get_raw_data(test_ds0)$perm),
    regexp = repeated_message
  )
  repeated_call()
  testthat::expect_true(test_ds0$is_mutate_delayed())

  load_dataset(t_dc)
  testthat::expect_message(
    get_raw_data(test_ds0),
    regexp = repeated_message
  )
  testthat::expect_true(test_ds0$is_mutate_delayed())

  testthat::expect_message(
    load_dataset(test_ds0),
    regexp = repeated_message
  )
  testthat::expect_silent(get_raw_data(test_ds0))
  testthat::expect_false(test_ds0$is_mutate_delayed())
  testthat::expect_true(all(c("head_letters", "new_var", "perm") %in% names(get_raw_data(test_ds0))))
  expect_code <- c(
    "head_iris <- head(iris)",
    "ds1 <- head_iris",
    "test_ds1 <- head_iris",
    "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
    "dc <- test_dc",
    "head_rock <- head(rock)",
    "ds2 <- head_rock",
    "head_mtcars <- head(mtcars)",
    "head_mtcars$carb <- head_mtcars$carb * 2",
    "head_mtcars$Species <- ds1$Species",
    "head_mtcars$head_letters <- dc$head_letters",
    "head_mtcars$new_var <- 1",
    "head_mtcars$perm <- ds2$perm"
  )
  testthat::expect_equal(
    pretty_code_string(test_ds0$get_code()),
    expect_code
  )

  mutate_dataset(test_ds0, code = "head_mtcars$new_var2 <- 2")
  testthat::expect_equal(
    pretty_code_string(test_ds0$get_code()),
    c(expect_code, "head_mtcars$new_var2 <- 2")
  )
  testthat::expect_false(test_ds0$is_mutate_delayed())
  testthat::expect_equal(get_raw_data(test_ds0)$new_var2, rep(2, 6))
})

testthat::test_that("Dataset check method", {
  test_ds0 <- Dataset$new("head_mtcars", head(mtcars))
  testthat::expect_error(
    test_ds0$check(),
    regex = "Cannot check preprocessing code of 'head_mtcars' - code is empty."
  )
  test_ds1 <- Dataset$new("head_mtcars", x = head(mtcars), code = "head_mtcars <- head(mtcars)")
  testthat::expect_true(
    test_ds1$check()
  )
  test_ds2 <- Dataset$new("head_mtcars", x = head(mtcars), code = "head_mtcars <- mtcars[1:6, ]")
  testthat::expect_true(
    test_ds2$check()
  )
  mutate_dataset(test_ds0, code = "head_mtcars$one <- 1")
  testthat::expect_true(
    test_ds1$check()
  )
  mutate_dataset(test_ds0, code = "head_mtcars$one <- head_mtcars$one * 2")
  testthat::expect_true(
    test_ds1$check()
  )
  mutate_dataset(test_ds1, code = "head_mtcars$one <- 1")
  testthat::expect_true(
    test_ds1$check()
  )
  mutate_dataset(test_ds1, code = "head_mtcars$one <- head_mtcars$one * 2")
  testthat::expect_true(
    test_ds1$check()
  )
})

testthat::test_that("get_hash returns the hash of the object passed to the constructor", {
  iris_hash <- digest::digest(iris, algo = "md5")
  ds <- Dataset$new("iris", iris)
  testthat::expect_equal(ds$get_hash(), iris_hash)
})
