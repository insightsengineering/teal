library(scda)
adsl_cf <- callable_function(function() synthetic_cdisc_data("rcd_2021_05_05")$adsl)
adrs_cf <- callable_function(function() synthetic_cdisc_data("rcd_2021_05_05")$adrs)

test_that("Test RelationalDataset is_pulled", {

  rel_data <- dataset(
    dataname = "XY",
    x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
    keys = "y",
    code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
                             stringsAsFactors = FALSE)"
  )
  expect_true(is_pulled(rel_data))
})

test_that("Test RelationalDatasetConnector is_pulled", {

  adsl <- cdisc_dataset_connector(dataname = "ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
  expect_false(is_pulled(adsl))

  load_dataset(adsl)
  expect_true(is_pulled(adsl))
})

test_that("Test RelationalDataConnector is_pulled", {

  adsl <- cdisc_dataset_connector(dataname = "ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
  adrs <- cdisc_dataset_connector(dataname = "ADRS", adrs_cf, keys = get_cdisc_keys("ADRS"))

  rdc <- cdisc_data(adsl, adrs)

  expect_false(is_pulled(rdc))
})
