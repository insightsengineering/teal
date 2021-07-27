test_that("check_metadata does not produce error if join_keys are consistent for given datasets", {

  df_1 <- data.frame(x = 1:10, y = 1:10)
  df_2 <- data.frame(u = 1:10, v = 1:10)

  constructor_wrapper <- function(join_keys) {
    data <- teal_data(
      dataset("df_1", df_1),
      dataset("df_2", df_2),
      join_keys = join_keys
    )
  }

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys()
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "u")))
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "u", "y" = "v")))
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_2", "df_2", c("u" = "u")))
    )$check_metadata()
  )

})

test_that("check_metadata fails if inconsistent join_keys for given datasets", {

  df_1 <- data.frame(x = 1:10, y = 1:10)
  df_2 <- data.frame(u = 1:10, v = 1:10)

  constructor_wrapper <- function(join_keys) {
    data <- teal_data(
      dataset("df_1", df_1),
      dataset("df_2", df_2),
      join_keys = join_keys
    )
  }

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "w")))
    )$check_metadata()
  )

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "y", "v" = "v")))
    )$check_metadata()
  )

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "x")))
    )$check_metadata()
  )

})

test_that("deep clone", {
  ## DatasetConnector
  expect_silent({
    x_c <- CallableFunction$new(data.frame)
    x_c$set_args(list(c1 = seq_len(10)))
    x <- dataset_connector("x", x_c)
  })

  expect_silent({
    x_copy <- x$clone(deep = TRUE)
  })

  expect_silent({
    load_dataset(x)
  })

  expect_true(is_pulled(x))
  expect_false(is_pulled(x_copy))

  # check one of the private fields of R6 class
  expect_false(rlang::is_reference(x$get_pull_callable(), x_copy$get_pull_callable()))

  ## RelationalData
  expect_silent({
    x1 <- dataset("x1", data.frame(col1 = seq_len(10)))
    x2 <- dataset("x2", data.frame(col2 = seq_len(10)))
    x <- RelationalData$new(x1, x2)
  })

  expect_silent({
    x_copy <- x$clone(deep = TRUE)
  })

  # check one of the private fields of list of R6 class object
  expect_false(rlang::is_reference(x$get_items()[[1]], x_copy$get_items()[[1]]))
  # check one of the private fields of R6 class
  expect_false(rlang::is_reference(x$get_join_keys(), x_copy$get_join_keys()))

  ## RelationalDataConnector
  expect_silent({
    x1_c <- CallableFunction$new(data.frame)
    x1_c$set_args(list(n = seq_len(10)))
    x1 <- dataset_connector("x1", x1_c)

    x2_c <- CallableFunction$new(data.frame)
    x2_c$set_args(list(n = seq_len(20)))
    x2 <- dataset_connector("x2", x2_c)

    x <- teal:::RelationalData$new(x1, x2)
  })

  expect_silent({
    x_copy <- x$clone(deep = TRUE)
  })

  expect_silent({
    lapply(x$get_items(), load_dataset)
  })

  expect_true(is_pulled(x))
  expect_false(is_pulled(x_copy))

  # check one of the private fields of list of R6 class object
  expect_false(rlang::is_reference(x$get_items()[[1]], x_copy$get_items()[[1]]))
  # check one of the private fields of R6 class
  expect_false(rlang::is_reference(x$get_join_keys(), x_copy$get_join_keys()))
})

testthat::test_that("The hashes of Datasets objects are correct after mutating the RelationalData object", {
  mutated_iris <- iris
  mutated_iris$test <- 1
  mutated_iris_hash <- digest::digest(mutated_iris, algo = "md5")
  rd <- teal_data(dataset("iris", iris))
  mutate_data(rd, code = "iris$test <- 1")
  rd$execute_mutate()
  testthat::expect_equal(rd$get_dataset("iris")$get_hash(), mutated_iris_hash)
})
