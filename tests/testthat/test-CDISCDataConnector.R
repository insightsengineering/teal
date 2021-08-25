library(random.cdisc.data)

# Single rcd_data connector ----
testthat::test_that("One cached and one dependent connector wrapped in a single rcd data connector", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, N = 1)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  data <- rcd_data(adsl, adae)

  items <- data$get_items()
  testthat::expect_true(inherits(data, "RelationalDataConnector"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))

  testthat::expect_equal(items$ADSL$get_code(), "ADSL <- radsl(N = 1)")
  testthat::expect_equal(items$ADAE$get_code(), "ADSL <- radsl(N = 1)\nADAE <- radae(ADSL = ADSL)")
  # pull args supplied this way does not persist to the reproducible code
  data$pull(args = list(seed = 2, na_percentage = .10))

  testthat::expect_equal(
    get_code(data, "ADSL"), "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)"
  )
  testthat::expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADAE <- radae(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(data), "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADAE <- radae(ADSL = ADSL)"
  )
})
