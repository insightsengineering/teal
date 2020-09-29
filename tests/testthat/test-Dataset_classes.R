## RawDataset ====
context("RawDataset")

test_that("RawDataset basics", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
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

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)
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

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_error(
    NamedDataset$new()
  )

  expect_error(
    NamedDataset$new(x = x)
  )

  expect_silent(
    NamedDataset$new(dataname = "abc", x = x)
  )

  expect_silent(
    NamedDataset$new(dataname = "abc", x = x, label = NULL)
  )
  expect_equal(
    NamedDataset$new(dataname = "abc", x = x, label = NULL),
    named_dataset(dataname = "abc", x = x, label = NULL)
  )

  expect_error(
    NamedDataset$new(x = x, code = "abc")
  )

  expect_silent({
    test_ds <- NamedDataset$new(
      dataname = "testds",
      x = x,
      code = "test_ds <- data.frame(x = c(1, 2), y = c('a', 'b'), stringsAsFactors = TRUE)",
      label = "Testing Dataset"
    )
  })

  expect_equal(
    test_ds$ncol,
    2
  )

  expect_equal(
    "Testing Dataset",
    test_ds$get_dataset_label()
  )

  expect_equal(
    "Testing Dataset",
    test_ds$get_dataset_label()
  )

  expect_equal(
    test_ds$get_code(deparse = TRUE),
    "test_ds <- data.frame(x = c(1, 2), y = c(\"a\", \"b\"), stringsAsFactors = TRUE)"
  )
  expect_equal(
    get_code(test_ds, deparse = TRUE),
    "test_ds <- data.frame(x = c(1, 2), y = c(\"a\", \"b\"), stringsAsFactors = TRUE)"
  )

  expect_equal(
    test_ds$get_code(deparse = FALSE),
    as.list(
      as.call(
        parse(
          text = "test_ds <- data.frame(x = c(1, 2), y = c(\"a\", \"b\"), stringsAsFactors = TRUE)"
        )
      )
    )
  )
  expect_equal(
    test_ds$get_code(deparse = FALSE),
    get_code(test_ds, deparse = FALSE)
  )

  expect_true(
    is.list(test_ds$get_code(deparse = FALSE))
  )

  expect_true(
    all(vapply(test_ds$get_code(deparse = FALSE), is.call, logical(1)))
  )

  expect_equal(
    test_ds$get_dataname(),
    "testds"
  )
})

## RelationalDataset ====
context("RelationalDataset")

test_that("RelationalDataset basics", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_error(
    RelationalDataset$new(dataname = "abc", x = x)
  )

  expect_silent({
    test_ds <- RelationalDataset$new(
      dataname = "testds",
      x = x,
      keys = keys(primary = "x", foreign = NULL, parent = NULL)
    )
  })

  expect_equal(
    test_ds$get_keys(),
    keys(primary = "x", foreign = NULL, parent = NULL)
  )

  expect_silent(test_ds$set_keys(keys(primary = "y", foreign = NULL, parent = NULL)))
  expect_equal(
    test_ds$get_keys(),
    keys(primary = "y", foreign = NULL, parent = NULL)
  )
})

## as_relational ====
test_that("as_relational function", {
  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  expect_silent({
    test_ds <- RawDataset$new(x)
  })

  expect_error({
    as_relational(list(data = "a"))
    },
    "no applicable method for"
  )

  expect_equal(
    RelationalDataset$new(
      dataname = "abc",
      x = x,
      keys = keys(primary = "x", foreign = NULL, parent = NULL),
      label = character(0)
    ),
    as_relational(
      x = test_ds,
      dataname = "abc",
      keys = keys(primary = "x", foreign = NULL, parent = NULL),
      label = character(0)
    )
  )
})
