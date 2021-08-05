## mutate_dataset ====

test_that("mutate_dataset", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)

  expect_silent({
    test_ds <- dataset(
      dataname = "x",
      x = x,
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

  test_script <- system.file("tests", "testthat", "mutate_code", "testds.R", package = "teal")

  expect_silent({
    test_ds_mut <- test_ds %>% mutate_dataset(read_script(test_script))
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
    test_ds_mut <- test_ds %>% mutate_dataset(script = test_script)
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
