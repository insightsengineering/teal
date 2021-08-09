library(scda)

testthat::test_that("as_cdisc does not modify a cdisc-flavoured object", {
  ds <- cdisc_dataset("ADSL", synthetic_cdisc_data("rcd_2021_05_05")$adsl)
  testthat::expect_identical(as_cdisc(ds), ds)
  conn <- cdisc_dataset_connector(
    "ADSL",
    callable_code("ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"),
    keys = get_cdisc_keys("ADSL")
  )
  testthat::expect_identical(as_cdisc(conn), conn)
})

testthat::test_that("as_cdisc passes keys from dataset to cdisc dataset", {
  ds <- dataset("ADSL", synthetic_cdisc_data("rcd_2021_05_05")$adsl, keys = c("STUDYID", "USUBJID", "SUBJID"))
  cdisc_ds <- as_cdisc(ds)
  testthat::expect_equal(get_keys(cdisc_ds), get_keys(ds))
})

testthat::test_that("as_cdisc assigns cdisc keys if dataset has no keys and name matches cdisc", {
  ds <- dataset("ADSL", synthetic_cdisc_data("rcd_2021_05_05")$adsl)
  testthat::expect_length(get_keys(ds), 0)
  testthat::expect_equal(get_keys(as_cdisc(ds)), get_cdisc_keys("ADSL"))
})

testthat::test_that("as_cdisc does not assign cdisc keys if dataset has no keys, but name does not match cdisc", {
  ds <- dataset("test", iris)
  testthat::expect_length(get_keys(ds), 0)
  testthat::expect_equal(get_keys(as_cdisc(ds)), get_keys(ds))
})
