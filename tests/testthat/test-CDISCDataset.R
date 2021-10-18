## CDISCDataset ====
testthat::test_that("CDISCDataset basics", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  testthat::expect_error(
    CDISCDataset$new(dataname = "abc", x = x)
  )

  testthat::expect_silent({
    test_ds <- CDISCDataset$new(
      dataname = "testds",
      x = x,
      keys = "x",
      parent = "testds2"
    )
  })

  testthat::expect_equal(
    test_ds$get_parent(),
    "testds2"
  )

  testthat::expect_silent(test_ds$set_parent("testds3"))
  testthat::expect_equal(
    test_ds$get_parent(),
    "testds3"
  )
})

testthat::test_that("data returns the data passed in the constructor", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"), object = list(1:3, letters[1:3]))))
  adsl <- CDISCDataset$new("ADSL", adsl_raw, parent = character(0), keys = get_cdisc_keys("ADSL"))

  testthat::expect_identical(adsl_raw, adsl$data)
})

testthat::test_that("get_dataname returns the dataname passed to the constructor", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"), object = list(1:3, letters[1:3]))))
  adsl <- CDISCDataset$new("ADSL", adsl_raw, parent = character(0), keys = get_cdisc_keys("ADSL"))

  testthat::expect_identical("ADSL", adsl$get_dataname())
})

testthat::test_that("Case 1: CDISCDataset$get_code() does not return duplicated code when
  CDISCDataset$mutate method is called", {
  iris_dataset <- CDISCDataset$new("iris", head(iris), code = "head(iris)", parent = character(0), keys = c("test"))
  mtcars_dataset <- CDISCDatasetConnector$new(
    "mtcars",
    callable_function(function() head(mtcars)),
    parent = character(0),
    keys = c("test")
  )
  mtcars_dataset$pull()
  mtcars_dataset$mutate("'mutating connector'")

  iris_dataset$mutate("'mutating dataset'", vars = list(mtcars_dataset = mtcars_dataset))
  testthat::expect_equal(
    iris_dataset$get_code(),
    paste(
      "mtcars <- (function() head(mtcars))()",
      "\"mutating connector\"",
      "mtcars_dataset <- mtcars",
      "head(iris)",
      "\"mutating dataset\"",
      sep = "\n"
    )
  )
})

testthat::test_that("Case 2: CDISCDataset$get_code() does not return duplicated code when
  CDISCDataset$mutate method is called", {
    adsl_d <- cdisc_dataset("ADSL", head(iris))
    adsl_d %>% mutate_dataset("ADSL$a <- x", vars = list(x = 1)) %>% mutate_dataset("ADSL$b <- y", vars = list(y = 2))

    adae_d <- code_cdisc_dataset_connector("ADAE", "head(mtcars)")
    adae_d %>% mutate_dataset("ADAE$a <- x", vars = list(x = 1))
    adae_d %>% mutate_dataset("ADAE$a <- ADAE$a*2")
    adae_d %>% load_dataset() %>% mutate_dataset("ADAE$a <- ADAE$a*2")

    adsl_d %>% mutate_dataset("ADSL$c <- z", vars = list(z = 3))
    adsl_d %>% mutate_dataset("ADSL$d <- ADAE$a[[1]]", vars = list(ADAE = adae_d))
    testthat::expect_equal(
      adsl_d %>% get_code() %>% pretty_code_string(),
      c("x <- 1",
        "y <- 2",
        "z <- 3",
        "ADAE <- head(mtcars)",
        "ADAE$a <- x",
        "ADAE$a <- ADAE$a * 2",
        "ADAE$a <- ADAE$a * 2",
        "ADSL$a <- x",
        "ADSL$b <- y",
        "ADSL$c <- z",
        "ADSL$d <- ADAE$a[[1]]"
      )
    )
  })
