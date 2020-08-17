## mutate_dataset ====
context("mutate_dataset")

test_that("mutate_dataset", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)

  expect_silent({
    test_ds <- RawDataset$new(x)
  })

  expect_error({
    mutate_dataset(x = test_ds, code = "a")
  }, "no applicable method for")

  expect_silent({
    test_ds <- NamedDataset$new(
      x,
      dataname = "x",
      code = "data.frame(x = c(1, 2), y = c('a', 'b'), stringsAsFactors = TRUE)"
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

  expect_silent({
    test_ds_mut <- test_ds %>% mutate_dataset("x$z <- c('one', 'two')")
  })

  expect_equal(
    test_ds_mut$get_raw_data(),
    data.frame(x = c(1, 2), y = c("a", "b"),
               z = c("one", "two"),
               stringsAsFactors = FALSE)
  )

  expect_equal(
    get_raw_data(test_ds_mut),
    data.frame(x = c(1, 2), y = c("a", "b"),
               z = c("one", "two"),
               stringsAsFactors = FALSE)
  )

  expect_error({
    test_ds %>% mutate_dataset("x <- 3")
  }, "data.frame")

  expect_error({
    test_ds %>% mutate_dataset(c("x <- 3", "som"))
  }, "is_character_single")

  expect_silent({
    test_ds <- RelationalDataset$new(
      x,
      dataname = "x",
      keys = keys(primary = "x", foreign = NULL, parent = NULL)
    )
  })
  expect_error({
    test_ds_mut <- test_ds %>% mutate_dataset("testds$z <- c('one', 'two')")
  })

  expect_silent({
    test_ds <- RelationalDataset$new(
      x,
      dataname = "testds",
      code = "testds <- whatever",
      keys = keys(primary = "x", foreign = NULL, parent = NULL)
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

  expect_true(is(test_ds_mut, "RelationalDataset"))

  expect_silent({
    test_ds_mut <- test_ds %>% mutate_dataset(script = "mutate_code/testds.R")
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

  expect_true(is(test_ds_mut, "RelationalDataset"))

  expect_error({
    test_ds_mut <- test_ds %>% mutate_dataset(code = "rm('testds')")
  }, "Code from testds need to return a data.frame")
})
