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

  expect_true(is(test_ds_mut, "Dataset"))

  expect_error({
    test_ds_mut <- test_ds %>% mutate_dataset(code = "rm('testds')")
  }, "Code from testds need to return a data.frame")
})


test_that("Keys handling while mutating dataset", {
  df <- data.frame(x = 1:10, y = 2:11, z = 1)

  ## Dataset
  # simple example - no mutation
  expect_equal(
    dataset("x", df, keys = c("x", "y")) %>%
      get_keys(),
    c("x", "y")
  )

  # mutation on non-key var
  expect_equal(
    dataset("x", df, keys = c("x", "y")) %>%
      mutate_dataset("x$z <- 2") %>%
      get_keys(),
    c("x", "y")
  )
  # mutation on non-key var, change key
  expect_equal(
    dataset("x", df, keys = c("x", "y")) %>%
      mutate_dataset("x$z <- 2", keys = "x") %>%
      get_keys(),
    "x"
  )

  # error when keys arg is not specified when mutate key var
  expect_error(
    dataset("x", df, keys = c("x", "y")) %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)"),
    "Primary keys specifed for x do not exist in the data.",
    fixed = TRUE
  )
  expect_error(
    dataset("x", df, keys = c("x", "y")) %>%
      mutate_dataset("x$x <- NULL"),
    "Primary keys specifed for x do not exist in the data.",
    fixed = TRUE
  )

  # mutation on key var
  expect_equal(
    dataset("x", df, keys = c("x", "y")) %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)", keys = c("x2", "y2")) %>%
      get_keys(),
    c("x2", "y2")
  )
  expect_equal(
    dataset("x", df, keys = c("x", "y")) %>%
      mutate_dataset("x$x <- NULL", keys = "y") %>%
      get_keys(),
    "y"
  )

  ## CDISCDataset (it has overloaded mutate method)
  library(random.cdisc.data)
  # simple example - no mutation
  expect_equal(
    cdisc_dataset("ADSL", radsl(cached = TRUE)) %>%
      get_keys(),
    get_cdisc_keys("ADSL")
  )

  # mutation on non-key var
  expect_equal(
    cdisc_dataset("ADSL", radsl(cached = TRUE)) %>%
      mutate_dataset("ADSL$x <- 1") %>%
      get_keys(),
    get_cdisc_keys("ADSL")
  )
  # mutation on non-key var, change key
  expect_equal(
    cdisc_dataset("ADSL", radsl(cached = TRUE)) %>%
      mutate_dataset("ADSL$x <- seq_len(nrow(ADSL))", keys = c(get_cdisc_keys("ADSL"), "x")) %>%
      get_keys(),
    c(get_cdisc_keys("ADSL"), "x")
  )

  # error when keys arg is not specified when mutate key var
  expect_error(
    cdisc_dataset("ADSL", radsl(cached = TRUE)) %>%
      mutate_dataset("ADSL <- dplyr::rename(ADSL, STUDYID2 = STUDYID, USUBJID2 = USUBJID)"),
    "Primary keys specifed for ADSL do not exist in the data.",
    fixed = TRUE
  )

  # mutation on key var
  expect_equal(
    cdisc_dataset("ADSL", radsl(cached = TRUE)) %>%
      mutate_dataset(
        "ADSL <- dplyr::rename(ADSL, STUDYID2 = STUDYID, USUBJID2 = USUBJID)",
        keys = c("STUDYID2", "USUBJID2")
      ) %>%
      get_keys(),
    c("STUDYID2", "USUBJID2")
  )


  ## DatasetConnector
  cl <- callable_function(function() data.frame(x = 1:10, y = 2:11, z = 1))

  # simple example - no mutation
  # connector keys
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      get_keys(),
    c("x", "y")
  )
  # pulled doataset connector keys
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      load_dataset() %>%
      get_keys(),
    c("x", "y")
  )
  # pulled doataset keys
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      load_dataset() %>%
      get_dataset() %>%
      get_keys(),
    c("x", "y")
  )

  # mutation on non-key var
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x$z <- 2") %>%
      get_keys(),
    c("x", "y")
  )
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x$z <- 2") %>%
      load_dataset() %>%
      get_keys(),
    c("x", "y")
  )
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x$z <- 2") %>%
      load_dataset() %>%
      get_dataset() %>%
      get_keys(),
    c("x", "y")
  )

  # mutation on non-key var, change key
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x$z <- 2", keys = "x") %>%
      get_keys(),
    "x"
  )
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x$z <- 2", keys = "x") %>%
      load_dataset() %>%
      get_keys(),
    "x"
  )
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x$z <- 2", keys = "x") %>%
      load_dataset() %>%
      get_dataset() %>%
      get_keys(),
    "x"
  )

  # mutation on key var
  # no error before pull
  expect_silent(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)")
  )
  # error after pull
  expect_error(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)") %>%
      load_dataset(),
    "Primary keys specifed for x do not exist in the data.",
    fixed = TRUE
  )

  # mutation on key var
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)", keys = c("x2", "y2")) %>%
      get_keys(),
    c("x2", "y2")
  )
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)", keys = c("x2", "y2")) %>%
      load_dataset() %>%
      get_keys(),
    c("x2", "y2")
  )
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)", keys = c("x2", "y2")) %>%
      load_dataset() %>%
      get_dataset() %>%
      get_keys(),
    c("x2", "y2")
  )

  # mutation on key var after load
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      load_dataset() %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)", keys = c("x2", "y2")) %>%
      get_keys(),
    c("x2", "y2")
  )
  expect_equal(
    dataset_connector("x", cl, keys = c("x", "y")) %>%
      load_dataset() %>%
      mutate_dataset("x <- dplyr::rename(x, x2 = x, y2 = y)", keys = c("x2", "y2")) %>%
      get_dataset() %>%
      get_keys(),
    c("x2", "y2")
  )
})
