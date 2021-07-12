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
