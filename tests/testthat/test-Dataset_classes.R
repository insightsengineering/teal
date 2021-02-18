## Dataset =====
test_that("Dataset basics", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_silent({
    test_ds <- Dataset$new(
      dataname = "testds",
      x = x,
      keys = "x"
    )
  })

  expect_equal(
    get_keys(test_ds),
    "x"
  )

  expect_silent(set_keys(test_ds, "y"))
  expect_equal(
    get_keys(test_ds),
    "y"
  )

  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
  )
  expect_error(
    suppressWarnings(Dataset$new(dataname = "test", x = df, keys = c("a", "b"))),
    regexp = "The provided primary key does not distinguish unique rows"
  )
})

test_that("Dataset$recreate", {
  ds <- Dataset$new(
    dataname = "mtcars",
    x = mtcars,
    keys = character(0),
    code = "mtcars",
    label = character(0),
    vars = list())
  ds2 <- ds$recreate()

  expect_identical(ds, ds2)
})

test_that("Dataset$get_*_colnames", {
  df <- as.data.frame(
    list(
      num = c(1, 2, 3),
      char = as.character(c("a", "b", "c")),
      fac = factor(x = c("lev1", "lev2", "lev1"), levels = c("lev1", "lev2"))
    ),
    stringsAsFactors = FALSE
  )
  ds <- Dataset$new("ds", x = df)

  expect_equal(ds$get_numeric_colnames(), c("num"))
  expect_equal(ds$get_character_colnames(), c("char"))
  expect_equal(ds$get_factor_colnames(), c("fac"))
})

test_that("Dataset$get_rownames", {
  df <- as.data.frame(
    list(
      num = c(1, 2, 3),
      char = as.character(c("a", "b", "c")),
      fac = factor(x = c("lev1", "lev2", "lev1"), levels = c("lev1", "lev2"))
    ),
    stringsAsFactors = FALSE
  )
  ds <- Dataset$new("ds", x = df)

  expect_equal(ds$get_rownames(), c("1", "2", "3"))
})

test_that("Dataset active bindings", {
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

  expect_equal(ds$ncol, 4)
  expect_equal(ds$nrow, 3)
  expect_equal(ds$dim, c(3, 4))
  expect_equal(ds$colnames, c("num", "char", "fac", "num2"))
  expect_equal(ds$rownames, c("1", "2", "3"))
  expect_equal(
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
  expect_equal(ds$var_names, ds$colnames)
  expect_true(is.null(ds$row_labels))

  # Depreciation warnings
  expect_warning(labs <- ds$column_labels)
  exp <- as.character(rep(NA, 4))
  names(exp) <- c("num", "char", "fac", "num2")
  expect_equal(labs, exp)
})

test_that("Dataset supplementary constructors", {
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
  expect_silent(x <- dataset_file(file_example))

  # Not a Dataset object causes an error
  file_example2 <- tempfile(fileext = "2.R")
  writeLines(
    text = c(
      "iris"
    ),
    con = file_example2
  )
  expect_error(
    x <- dataset_file(file_example2),
    regexp = "The object returned from the file is not of Dataset class.",
    fixed = TRUE
  )

  # Deprecation warnings
  expect_warning(x2 <- named_dataset_file(file_example))
  expect_warning(x3 <- relational_dataset_file(file_example))
  expect_equal(x, x2)
  expect_equal(x, x3)

  # Deprecated constructors
  expect_error(raw_dataset(iris))
  expect_warning(ds1 <- named_dataset("ds", iris))
  expect_warning(ds2 <- relational_dataset("ds", iris))
  expect_equal(ds1, ds2)
})

## CDISCDataset ====
test_that("CDISCDataset basics", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_error(
    CDISCDataset$new(dataname = "abc", x = x)
  )

  expect_silent({
    test_ds <- CDISCDataset$new(
      dataname = "testds",
      x = x,
      keys = "x",
      parent = "testds2"
    )
  })

  expect_equal(
    test_ds$get_parent(),
    "testds2"
  )

  expect_silent(test_ds$set_parent("testds3"))
  expect_equal(
    test_ds$get_parent(),
    "testds3"
  )
})
