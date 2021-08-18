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

testthat::test_that("CDISCDataset$get_code() does not return duplicated code when
  CDISCDataset$mutate method is called", {
  adsl_d <- cdisc_dataset("ADSL", head(iris))
  adsl_d %>% mutate_dataset("ADSL$a <- x", vars = list(x = 1)) %>% mutate_dataset("ADSL$b <- y", vars = list(y = 2))

  adae_d <- rcd_cdisc_dataset_connector("ADAE", radae, cached = TRUE)
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
      "ADAE <- radae(cached = TRUE)",
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
