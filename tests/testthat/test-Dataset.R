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

  testthat::expect_error(
    mutate_dataset(
      test_ds0,
      code = "mtcars$new_var <- rock$perm[1]", vars = list(test_ds2 = test_ds2)
    ),
    regexp = "Circular dependencies detected"
  )

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

  mutate_dataset(test_ds0, code = "head_mtcars$head_letters <- dc$head_letters", vars = list(dc = t_dc))

  testthat::expect_equal(
    pretty_code_string(test_ds0$get_code()),
    c("head_iris <- head(iris)",
      "ds1 <- head_iris",
      "test_ds1 <- head_iris",
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "dc <- test_dc",
      "head_mtcars <- head(mtcars)",
      "head_mtcars$carb <- head_mtcars$carb * 2",
      "head_mtcars$Species <- ds1$Species",
      "head_mtcars$head_letters <- dc$head_letters"
    )
  )


  testthat::expect_null(get_raw_data(test_ds0)$head_mtcars)

  testthat::expect_true(test_ds0$is_mutate_delayed())
  testthat::expect_equal(
    pretty_code_string(test_ds0$get_code()),
    c("head_iris <- head(iris)",
      "ds1 <- head_iris",
      "test_ds1 <- head_iris",
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "dc <- test_dc",
      "head_mtcars <- head(mtcars)",
      "head_mtcars$carb <- head_mtcars$carb * 2",
      "head_mtcars$Species <- ds1$Species",
      "head_mtcars$head_letters <- dc$head_letters"
    )
  )

  # continuing to delay
  mutate_dataset(test_ds0, code = "head_mtcars$new_var <- 1")
  testthat::expect_true(test_ds0$is_mutate_delayed())

  testthat::expect_equal(
    pretty_code_string(test_ds0$get_code()),
    c("head_iris <- head(iris)",
      "ds1 <- head_iris",
      "test_ds1 <- head_iris",
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "dc <- test_dc",
      "head_mtcars <- head(mtcars)",
      "head_mtcars$carb <- head_mtcars$carb * 2",
      "head_mtcars$Species <- ds1$Species",
      "head_mtcars$head_letters <- dc$head_letters",
      "head_mtcars$new_var <- 1"
    )
  )
  expect_null(get_raw_data(test_ds0)$new_var)
  testthat::expect_true(test_ds0$is_mutate_delayed())

  mutate_dataset(test_ds0, code = "head_mtcars$perm <- ds2$perm", vars = list(ds2 = test_ds2))
  testthat::expect_equal(
    pretty_code_string(test_ds0$get_code()),
    c("head_iris <- head(iris)",
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
  )

  expect_null(get_raw_data(test_ds0)$perm)
  testthat::expect_equal(
    pretty_code_string(test_ds0$get_code()),
    c("head_iris <- head(iris)",
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
  )
  testthat::expect_true(test_ds0$is_mutate_delayed())

  load_dataset(t_dc)
  testthat::expect_true(test_ds0$is_mutate_delayed())

  load_dataset(test_ds0)
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

testthat::test_that("get_code_class returns the correct CodeClass object", {
  cc1 <- CodeClass$new(code = "iris <- head(iris)", dataname = "iris")
  cc2 <- CodeClass$new(code = "mtcars <- head(mtcars)", dataname = "mtcars", deps = "iris")
  ds1 <- Dataset$new("iris", head(iris), code = "iris <- head(iris)")
  ds2 <- Dataset$new("mtcars", head(mtcars), code = "mtcars <- head(mtcars)", vars = list(iris = ds1))
  testthat::expect_equal(ds1$get_code_class(), cc1)
  testthat::expect_equal(ds2$get_code_class(), cc1$append(cc2))
})

testthat::test_that("get_code_class returns the correct CodeClass after mutating with another Dataset", {
  ds1 <- Dataset$new("iris", head(iris), code = "iris <- head(iris)")
  ds2 <- Dataset$new("mtcars", head(mtcars), code = "mtcars <- head(mtcars)")
  cc1 <- CodeClass$new(code = "mtcars <- head(mtcars)", dataname = "mtcars")
  cc2 <- CodeClass$new(code = "iris <- head(iris)", dataname = "iris")
  cc3 <- CodeClass$new(code = "iris$test <- 1", dataname = "iris")
  ds1$mutate(cc3, vars = list(mtcars = ds2))
  testthat::expect_equal(ds1$get_code_class(), cc1$append(cc2)$append(cc3))
})

testthat::test_that("Dataset$recreate does not reset the mutation code", {
  cf <- CallableFunction$new(function() head(mtcars))
  dataset_connector1 <- DatasetConnector$new("mtcars", cf)
  dataset1 <- Dataset$new("iris", head(iris))
  dataset1$mutate(code = "test", vars = list(test = dataset_connector1))
  code_before_recreating <- dataset1$get_code()
  dataset1$recreate()
  code_after_recreating <- dataset1$get_code()
  testthat::expect_equal(code_after_recreating, code_before_recreating)
})

testthat::test_that("Dataset$recreate does not reset the variables needed for mutation", {
  cf <- CallableFunction$new(function() head(mtcars))
  dataset_connector1 <- DatasetConnector$new("mtcars", cf)
  dataset1 <- Dataset$new("iris", head(iris))
  dataset1$mutate(code = "test", vars = list(test = dataset_connector1))
  mutate_vars_before_recreation <- dataset1$get_mutate_vars()
  dataset1$recreate()
  testthat::expect_identical(dataset1$get_mutate_vars(), mutate_vars_before_recreation)
})

testthat::test_that("Dataset$is_mutate_delayed returns TRUE if the Dataset's dependency is delayed", {
  cf <- CallableFunction$new(function() head(mtcars))
  dataset_connector1 <- DatasetConnector$new("mtcars", cf)
  dataset1 <- Dataset$new("iris", head(iris))
  dataset1$mutate(code = "", vars = list(test = dataset_connector1))
  testthat::expect_true(dataset1$is_mutate_delayed())
})

testthat::test_that("Dataset$is_mutate_delayed stays FALSE if the Dataset's
  dependency turns from not delayed to delayed", {
  cf <- CallableFunction$new(function() head(mtcars))
  dataset_connector1 <- DatasetConnector$new("mtcars", cf)
  dataset1 <- Dataset$new("iris", head(iris))
  dataset_dependency <- Dataset$new("plantgrowth", head(PlantGrowth))

  dataset1$mutate(code = "", vars = list(test = dataset_dependency))
  testthat::expect_false(dataset1$is_mutate_delayed())

  dataset_dependency$mutate(code = "", vars = list(test = dataset_connector1))
  testthat::expect_true(dataset_dependency$is_mutate_delayed())
  testthat::expect_false(dataset1$is_mutate_delayed())
})

testthat::test_that("Dataset$get_join_keys returns an empty JoinKeys object", {
  dataset1 <- Dataset$new("iris", head(iris))
  testthat::expect_true(is(dataset1$get_join_keys(), "JoinKeys"))
  testthat::expect_equal(length(dataset1$get_join_keys()$get()), 0)
})

testthat::test_that("Dataset$set_join_keys works independently", {
  dataset1 <- Dataset$new("iris", head(iris))
  testthat::expect_silent(
    dataset1$set_join_keys(join_key("iris", "other_dataset", c("Species" = "some_col")))
  )
  testthat::expect_error(
    dataset1$set_join_keys(join_key("iris", "other_dataset", c("Sepal.Length" = "some_col2")))
  )
  testthat::expect_true(is(dataset1$get_join_keys(), "JoinKeys"))
  testthat::expect_equal(length(dataset1$get_join_keys()$get()), 2)
})

testthat::test_that("Dataset$mutate_join_keys works independently", {
  dataset1 <- Dataset$new("iris", head(iris))
  testthat::expect_silent(
    dataset1$mutate_join_keys("other_dataset", c("Sepal.Length" = "some_col2"))
  )
  testthat::expect_true(is(dataset1$get_join_keys(), "JoinKeys"))
  testthat::expect_equal(length(dataset1$get_join_keys()$get()), 2)

  dataset2 <- Dataset$new("iris", head(iris))
  testthat::expect_silent(
    dataset2$mutate_join_keys("other_dataset", c("Sepal.Length"))
  )
  testthat::expect_true(is(dataset2$get_join_keys(), "JoinKeys"))
  testthat::expect_equal(length(dataset2$get_join_keys()$get()), 2)
})

testthat::test_that("Dataset$set_join_keys works with Dataset$mutate_join_keys", {
  dataset1 <- Dataset$new("iris", head(iris))
  testthat::expect_silent(
    dataset1$set_join_keys(join_key("iris", "other_dataset", c("Species" = "some_col")))
  )
  testthat::expect_identical(
    dataset1$get_join_keys()$get()$iris$other_dataset, c("Species" = "some_col")
  )
  testthat::expect_silent(
    dataset1$mutate_join_keys("other_dataset", c("Sepal.Length" = "some_col2"))
  )
  dataset1$mutate(code = "iris$unique_id <- 1:2")
  testthat::expect_silent(
    dataset1$mutate_join_keys("iris", "unique_id")
  )
  testthat::expect_silent(
    join_keys_list <- dataset1$get_join_keys()$get()
  )
  testthat::expect_identical(
    dataset1$get_join_keys()$get()$iris$other_dataset, c("Sepal.Length" = "some_col2")
  )
  testthat::expect_identical(
    dataset1$get_join_keys()$get()$iris$iris, c("unique_id" = "unique_id")
  )
  testthat::expect_identical(
    dataset1$get_join_keys()$get()$other_dataset$iris, c("some_col2" = "Sepal.Length")
  )
})

testthat::test_that("Dupliated mutation code is shown via get_code()", {
  dataset <- Dataset$new("iris", head(iris))
  dataset$mutate("7")
  dataset$mutate("7")
  testthat::expect_equal(dataset$get_code(), paste("7", "7", sep = "\n"))
})

test_that("mutate_dataset", {
  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)

  expect_silent({
    test_ds <- dataset(
      dataname = "x",
      x = x,
      code = "data.frame(x = c(1, 2), y = c('a', 'b'), stringsAsFactors = FALSE)"
    )
  })

  expect_error({
    mutate_dataset(x = test_ds)
  }, "is_character_single(code) || is_character_single(script) is not TRUE")

  expect_error({
    mutate_dataset(x = test_ds, code = TRUE)
  }, "character")

  expect_error({
    mutate_dataset(x = test_ds, code = "y <- test")
  }, "Evaluation of the code failed")

  # error because the code, "y <- test", was added even though it yielded an error.
  expect_error({
    test_ds_mut <- test_ds %>% mutate_dataset("x$z <- c('one', 'two')")
  })

  expect_equal(
    test_ds$get_raw_data(),
    data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)
  )

  expect_equal(
    get_raw_data(test_ds),
    data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)
  )

  expect_error({
    test_ds %>% mutate_dataset("x <- 3")
  }, "object 'test' not found")

  expect_error({
    test_ds %>% mutate_dataset(c("x <- 3", "som"))
  }, "is_character_vector")

  expect_silent({
    test_ds <- dataset(
      dataname = "x",
      x = x,
      keys = "x"
    )
  })
  expect_error({
    test_ds_mut <- test_ds %>% mutate_dataset("testds$z <- c('one', 'two')")
  })

  expect_silent({
    test_ds <- dataset(
      dataname = "testds",
      x = x,
      code = "testds <- whatever",
      keys = "x"
    )
  })

  expect_silent({
    test_ds_mut <- mutate_dataset(test_ds, code = "testds$z <- c('one', 'two')")
  })

  expect_equal(
    test_ds_mut$get_raw_data(),
    data.frame(x = c(1, 2), y = c("a", "b"),
               z = c("one", "two"),
               stringsAsFactors = FALSE)
  )

  expect_silent({
    test_ds_mut <- test_ds %>% mutate_dataset(read_script("mutate_code/testds.R"))
  })

  expect_equal(
    test_ds_mut$get_raw_data(),
    data.frame(x = c(1, 2), y = c("a", "b"),
               z = c(1, 1),
               stringsAsFactors = FALSE)
  )

  expect_equal(
    test_ds_mut$get_code(),
    "testds <- whatever\ntestds$z <- c(\"one\", \"two\")\nmut_fun <- function(x) {\n    x$z <- 1\n    return(x)\n}\ntestds <- mut_fun(testds)" # nolint
  )

  expect_true(is(test_ds_mut, "Dataset"))

  expect_silent({
    test_ds_mut <- test_ds %>% mutate_dataset(read_script("mutate_code/testds.R"))
  })

  expect_equal(
    test_ds_mut$get_raw_data(),
    data.frame(x = c(1, 2), y = c("a", "b"),
               z = c(1, 1),
               stringsAsFactors = FALSE)
  )

  expect_equal(
    test_ds_mut$get_code(),
    "testds <- whatever\ntestds$z <- c(\"one\", \"two\")\nmut_fun <- function(x) {\n    x$z <- 1\n    return(x)\n}\ntestds <- mut_fun(testds)\nmut_fun <- function(x) {\n    x$z <- 1\n    return(x)\n}\ntestds <- mut_fun(testds)" # nolint
  )

  expect_true(is(test_ds_mut, "Dataset"))

  expect_error({
    test_ds_mut <- test_ds %>% mutate_dataset(code = "rm('testds')")
  }, "Code from testds need to return a data.frame")
})

test_that("mutate_dataset with vars argument", {
  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)
  var1 <- "3"
  var2 <- "4"
  test_ds <- dataset(
    dataname = "x",
    x = x,
    code = "data.frame(x = c(1, 2), y = c('a', 'b'), stringsAsFactors = TRUE)"
  )
  expect_silent(
    mutate_dataset(x = test_ds, code = "x$z <- var", vars = list(var = var1))
  )
  expect_silent(
    mutate_dataset(x = test_ds, code = "x$z <- var2", vars = list(var2 = paste(var1, var2)))
  )
  expect_error(
    mutate_dataset(x = test_ds, code = "x$z <- var", vars = list(var = var2))
  )
  expect_silent(
    mutate_dataset(x = test_ds, code = "x$zz <- var", vars = list(var = var1))
  )

  pull_fun2 <- callable_function(data.frame)
  pull_fun2$set_args(args = list(a = c(1, 2, 3)))
  expect_silent({
    t <- dataset_connector("test", pull_fun2)
  })
  expect_silent(load_dataset(t))
  expect_silent(
    mutate_dataset(x = t, code = "test$z <- var", vars = list(var = var1))
  )
  expect_silent(
    mutate_dataset(x = t, code = "test$z <- var2", vars = list(var2 = paste(var1, var2)))
  )
  expect_error(
    mutate_dataset(x = t, code = "test$z <- var", vars = list(var = var2))
  )
  expect_silent(
    mutate_dataset(x = t, code = "test$zz <- var", vars = list(var = var1))
  )
})

testthat::test_that("get_hash returns the correct hash after mutating the Dataset object", {
  mutated_iris <- iris
  mutated_iris$test <- 1
  mutated_iris_hash <- digest::digest(mutated_iris, algo = "md5")
  ds <- Dataset$new("iris", iris) %>% mutate_dataset("iris$test <- 1")
  testthat::expect_equal(ds$get_hash(), mutated_iris_hash)
})

testthat::test_that("dataset$merge_join_keys does not throw on basic input", {
  dataset1 <- Dataset$new("iris", head(iris))
  dataset1$set_join_keys(join_key("iris", "other_dataset", c("Species" = "some_col")))

  dataset2 <- Dataset$new("iris", head(iris))
  dataset2$set_join_keys(join_key("iris", "other_dataset_2", c("Sepal.Length" = "some_col2")))

  before_merge <- dataset1$get_join_keys()$get()
  after_merge <- dataset1$merge_join_keys(dataset2$get_join_keys())$get_join_keys()$get()


  testthat::expect_true(all(names(before_merge) %in% names(after_merge)))
  testthat::expect_true(length(before_merge) < length(after_merge))
  testthat::expect_equal(names(after_merge), c("iris", "other_dataset", "other_dataset_2"))
})

testthat::test_that("dataset$print warns of superfluous arguments", {
  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)
  test_ds <- dataset(
    dataname = "x",
    x = x,
    code = "data.frame(x = c(1, 2), y = c('a', 'b'), stringsAsFactors = FALSE)"
  )
  testthat::expect_warning(
    capture.output(print(test_ds, "un used argument"))
  )
})

testthat::test_that("dataset$print prints out all rows when less than 6", {
  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)
  test_ds <- dataset(
    dataname = "x",
    x = x,
    code = "data.frame(x = c(1, 2), y = c('a', 'b'), stringsAsFactors = FALSE)"
  )

  testthat::expect_equal(
    capture.output(print(test_ds)),
    c(
      "A Dataset object containing the following data.frame (2 rows and 2 columns):",
      "  x y",
      "1 1 a",
      "2 2 b"
    )
  )
})

testthat::test_that("dataset$print prints out both head and tail when more than 6 rows", {
  x <- head(iris, 7)
  test_ds <- dataset(
    dataname = "x",
    x = x,
    code = "data.frame(x = c(1, 2), y = c('a', 'b'), stringsAsFactors = FALSE)"
  )

  testthat::expect_equal(
    capture.output(print(test_ds)),
    c(
      "A Dataset object containing the following data.frame (7 rows and 5 columns):",
      "  Sepal.Length Sepal.Width Petal.Length Petal.Width Species",
      "1          5.1         3.5          1.4         0.2  setosa",
      "2          4.9         3.0          1.4         0.2  setosa",
      "3          4.7         3.2          1.3         0.2  setosa",
      "4          4.6         3.1          1.5         0.2  setosa",
      "5          5.0         3.6          1.4         0.2  setosa",
      "6          5.4         3.9          1.7         0.4  setosa",
      "",
      "...",
      "  Sepal.Length Sepal.Width Petal.Length Petal.Width Species",
      "2          4.9         3.0          1.4         0.2  setosa",
      "3          4.7         3.2          1.3         0.2  setosa",
      "4          4.6         3.1          1.5         0.2  setosa",
      "5          5.0         3.6          1.4         0.2  setosa",
      "6          5.4         3.9          1.7         0.4  setosa",
      "7          4.6         3.4          1.4         0.3  setosa"
    )
  )
})
