testthat::test_that("teal_data returns CDISCTealData object rather than TealData
  object when arguments contain any type of a CDISCTealData object", {
  dummy_adsl <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adsl <- cdisc_dataset("ADSL", dummy_adsl)
  dummy_adtte <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adtte <- cdisc_dataset("ADTTE", dummy_adtte)
  ds2 <- dataset("ds", iris)

  dummy_mae <- head(iris)
  class(dummy_mae) <- "MultiAssayExperiment"

  mae <- MAETealDataset$new("MAE", dummy_mae)

  mixed_data <- teal_data(mae, adsl, adtte, ds2)
  testthat::expect_equal(class(mixed_data), c("CDISCTealData", "TealData", "TealDataAbstract", "R6"))

  mae_only <- teal_data(mae)
  testthat::expect_equal(class(mae_only), c("TealData", "TealDataAbstract", "R6"))

  dataset_only <- teal_data(ds2)
  testthat::expect_equal(class(dataset_only), c("TealData", "TealDataAbstract", "R6"))

  mae_and_dataset <- teal_data(mae, ds2)
  testthat::expect_equal(class(mae_and_dataset), c("TealData", "TealDataAbstract", "R6"))

  cdisc_only <- teal_data(adsl, adtte)
  testthat::expect_equal(class(cdisc_only), c("CDISCTealData", "TealData", "TealDataAbstract", "R6"))

  testthat::expect_error(
    teal_data()
  )
})
