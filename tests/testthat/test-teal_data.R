library(random.cdisc.data)
library(hermes)
testthat::test_that("teal_data returns CDISCData object rather than RelationalData object", {
  adsl <- cdisc_dataset("ADSL", radsl(cached = TRUE, na_percentage = 0.2))
  adtte <- cdisc_dataset("ADTTE", radtte(cached = TRUE))
  ds2 <- dataset("ds", iris)

  MAE <- multi_assay_experiment
  mae <- dataset("MAE", MAE)



  mixed_data <- teal_data(mae, adsl, adtte, ds2)
  testthat::expect_equal(class(mixed_data), c("CDISCData", "RelationalData", "DataAbstract", "R6"))

  mae_only <- teal_data(mae)
  testthat::expect_equal(class(mae_only), c("RelationalData", "DataAbstract", "R6"))

  dataset_only <- teal_data(ds2)
  testthat::expect_equal(class(dataset_only), c("RelationalData", "DataAbstract", "R6"))

  mae_and_dataset <- teal_data(mae, ds2)
  testthat::expect_equal(class(mae_and_dataset), c("RelationalData", "DataAbstract", "R6"))

  cdisc_only <- teal_data(adsl, adtte)
  testthat::expect_equal(class(cdisc_only), c("CDISCData", "RelationalData", "DataAbstract", "R6"))

  testthat::expect_error(
    teal_data()
  )
})
