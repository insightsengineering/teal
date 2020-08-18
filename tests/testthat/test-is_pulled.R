context("is_pulled")

library(random.cdisc.data)

test_that("Test RawDatasetConnector is_pulled", {

  dc <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl)
  expect_false(is_pulled(dc))

  load_dataset(dc)
  expect_true(is_pulled(dc))
})

test_that("Test RelationalDataset is_pulled", {

  rel_data <- relational_dataset(
    dataname = "XY",
    x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
    keys = keys(primary = "y", foreign = NULL, parent = NULL),
    code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'),
                             stringsAsFactors = FALSE)"
  )
  expect_true(is_pulled(rel_data))
})

test_that("Test RelationalDatasetConnector is_pulled", {


  adsl <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
  expect_false(is_pulled(adsl))

  load_dataset(adsl)
  expect_true(is_pulled(adsl))
})

test_that("Test RelationalDataConnector is_pulled", {

  adsl <- rcd_cdisc_dataset_connector(dataname = "ADSL", fun = radsl, cached = TRUE)
  adrs <- rcd_cdisc_dataset_connector(dataname = "ADRS", fun = radrs, ADSL = adsl)

  rdc <- rcd_data(adsl, adrs)

  expect_false(is_pulled(rdc))
})
