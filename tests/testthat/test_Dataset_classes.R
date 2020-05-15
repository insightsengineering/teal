## RawDataset ====
context("RawDataset")

test_that("RawDataset basics", {

  x <- data.frame(x = c(1, 1), y = c("a", "a"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_error(
    RawDataset$new()
  )

  expect_silent({
    test_ds <- RawDataset$new(x)
  })

  labels <- expect_warning(test_ds$column_labels)

  expect_equal(
    labels,
    setNames(c("X", "Y"), c("x", "y"))
  )

  expect_equal(
    test_ds$get_column_labels(),
    setNames(c("X", "Y"), c("x", "y"))
  )

  expect_equal(
    test_ds$get_character_colnames(),
    character(0)
  )

  expect_equal(
    test_ds$get_factor_colnames(),
    "y"
  )

  expect_equal(
    test_ds$get_numeric_colnames(),
    "x"
  )

  expect_equal(
    test_ds$ncol,
    2
  )

  expect_equal(
    test_ds$nrow,
    2
  )

  expect_equal(
    test_ds$data,
    test_ds$raw_data
  )

  expect_equal(
    test_ds$data,
    x
  )

  expect_equal(
    test_ds$dim,
    c(2, 2)
  )

  x <- data.frame(x = c(1, 1), y = c("a", "a"), stringsAsFactors = FALSE)
  rtables::var_labels(x) <- c("X", "Y")
  expect_silent({
    test_ds <- RawDataset$new(x)
  })

  expect_equal(
    RawDataset$new(x),
    raw_dataset(x)
  )

  expect_equal(
    test_ds$get_character_colnames(),
    "y"
  )

  expect_equal(
    test_ds$get_factor_colnames(),
    character(0)
  )

  expect_equal(
    test_ds$get_numeric_colnames(),
    "x"
  )

  expect_equal(
    test_ds$get_colnames(),
    test_ds$colnames
  )

  expect_equal(
    test_ds$get_rownames(),
    test_ds$rownames
  )

  expect_equal(
    test_ds$get_colnames(),
    test_ds$var_names
  )

  expect_equal(
    get_raw_data(test_ds),
    x
  )


})

## NamedDataset ====
context("NamedDataset")

test_that("NamedDataset basics", {

  x <- data.frame(x = c(1, 1), y = c("a", "a"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_error(
    NamedDataset$new()
  )

  expect_error(
    NamedDataset$new(x = x)
  )

  expect_silent(
    NamedDataset$new(x = x, dataname = "abc")
  )

  expect_silent(
    NamedDataset$new(x = x, dataname = "abc", label = NULL)
  )
  expect_equal(
    NamedDataset$new(x = x, dataname = "abc", label = NULL),
    named_dataset(x = x, dataname = "abc", label = NULL)
  )

  expect_error(
    NamedDataset$new(x = x, code = "abc")
  )

  expect_silent({
    test_ds <- NamedDataset$new(x, dataname = "testds",
                                code = "test_ds <- data.frame(x = c(1, 1), y = c('a', 'a'), stringsAsFactors = TRUE)",
                                label = "Testing Dataset"
                                )
  })

  expect_equal(
    test_ds$ncol,
    2
  )

  expect_equal(
    "Testing Dataset",
    test_ds$dataset_label
  )

  expect_equal(
    "Testing Dataset",
    test_ds$get_dataset_label()
  )

  expect_equal(
    test_ds$get_code(),
    "test_ds <- data.frame(x = c(1, 1), y = c('a', 'a'), stringsAsFactors = TRUE)"
  )

  expect_equal(
    test_ds$dataname,
    "testds"
  )
})

## RelationalDataset ====
context("RelationalDataset")

test_that("RelationalDataset basics", {

  x <- data.frame(x = c(1, 1), y = c("a", "a"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_error(
    RelationalDataset$new(x = x, dataname = "abc")
  )

  expect_silent({
    test_ds <- RelationalDataset$new(x, dataname = "testds",
                                keys = keys(primary = "x", foreign = NULL, parent = NULL)
    )
  })

  expect_equal(
    test_ds$keys,
    keys(primary = "x", foreign = NULL, parent = NULL)
  )

  expect_silent(test_ds$set_keys(keys(primary = "y", foreign = NULL, parent = NULL)))
  expect_equal(
    test_ds$keys,
    keys(primary = "y", foreign = NULL, parent = NULL)
  )
})

## as_relational ====
context("as_relational")

test_that("as_relational function", {
  x <- data.frame(x = c(1, 1), y = c("a", "a"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_silent({
    test_ds <- RawDataset$new(x)
  })

  expect_error({
    as_relational(list(data = "a"))
    },
    "RawDataset"
  )

  expect_equal(
    RelationalDataset$new(
      x = x,
      dataname = "abc",
      keys = keys(primary = "x", foreign = NULL, parent = NULL),
      code = "xx",
      label = character(0)
    ),
    as_relational(
      dataset = test_ds,
      dataname = "abc",
      keys = keys(primary = "x", foreign = NULL, parent = NULL),
      code = "xx",
      label = character(0)
    )
  )
})
