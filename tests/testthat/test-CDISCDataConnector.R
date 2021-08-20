library(random.cdisc.data)

# Single rcd_data connector ----
test_that("One cached and one dependent connector wrapped in a single rcd data connector", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, N = 1)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  data <- rcd_data(adsl, adae)

  items <- data$get_items()
  expect_true(inherits(data, "RelationalDataConnector"))
  expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))

  expect_equal(items$ADSL()$get_code(), "radsl(N = 1)")
  expect_equal(items$ADAE()$get_code(), "radae(ADSL = ADSL)")
  # pull args supplied this way does not persist to the reproducible code
  data$pull(args = list(seed = 2, na_percentage = .10))

  expect_equal(
    get_code(data, "ADSL"), "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)"
  )
  expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADAE <- radae(ADSL = ADSL)"
  )
  expect_equal(
    get_code(data), "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADAE <- radae(ADSL = ADSL)"
  )
})
