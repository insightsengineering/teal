test_that("Test TealDataset is_pulled", {
  rel_data <- dataset(
    dataname = "XY",
    x = head(iris),
    keys = character(0)
  )
  expect_true(is_pulled(rel_data))
})

test_that("Test TealDatasetConnector is_pulled", {
  iris <- TealDatasetConnector$new(dataname = "iris", CallableFunction$new(function() head(iris)))
  testthat::expect_false(is_pulled(iris))

  load_dataset(iris)
  testthat::expect_true(is_pulled(iris))
})

test_that("Test TealDataConnector is_pulled", {
  adsl_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
    }
  )
  adrs_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADRS"))))
    }
  )
  adsl <- cdisc_dataset_connector(dataname = "ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
  adrs <- cdisc_dataset_connector(dataname = "ADRS", adrs_cf, keys = get_cdisc_keys("ADRS"))

  rdc <- cdisc_data(adsl, adrs)

  expect_false(is_pulled(rdc))
})
