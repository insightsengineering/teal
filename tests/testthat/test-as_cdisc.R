testthat::test_that("as_cdisc does not modify a cdisc-flavoured object", {
  ds <- cdisc_dataset("ADSL", as.data.frame(setNames(nm = get_cdisc_keys("ADSL"))))
  testthat::expect_identical(as_cdisc(ds), ds)
  conn <- cdisc_dataset_connector(
    "ADSL",
    callable_code("as.data.frame(setNames(nm = get_cdisc_keys(\"ADSL\")))"),
    keys = get_cdisc_keys("ADSL")
  )
  testthat::expect_identical(as_cdisc(conn), conn)
})

testthat::test_that("as_cdisc passes keys from dataset to cdisc dataset", {
  ds <- dataset(
    "ADSL",
    as.data.frame(setNames(nm = c(get_cdisc_keys("ADSL"), "SUBJID"))),
    keys = c("STUDYID", "USUBJID", "SUBJID")
  )
  cdisc_ds <- as_cdisc(ds)
  testthat::expect_equal(get_keys(cdisc_ds), get_keys(ds))
})

testthat::test_that("as_cdisc passes metadata from dataset to cdisc dataset", {
  ds <- dataset(
    "ADSL",
    as.data.frame(setNames(nm = c(get_cdisc_keys("ADSL"), "SUBJID"))),
    keys = c("STUDYID", "USUBJID", "SUBJID"),
    metadata = list(A = TRUE, B = "x")
  )
  cdisc_ds <- as_cdisc(ds)
  testthat::expect_equal(cdisc_ds$get_metadata(), ds$get_metadata())
})

testthat::test_that("as_cdisc assigns cdisc keys if dataset has no keys and name matches cdisc", {
  ds <- dataset("ADSL", as.data.frame(setNames(nm = get_cdisc_keys("ADSL"))))
  testthat::expect_length(get_keys(ds), 0)
  testthat::expect_equal(get_keys(as_cdisc(ds)), get_cdisc_keys("ADSL"))
})

testthat::test_that("as_cdisc does not assign cdisc keys if dataset has no keys, but name does not match cdisc", {
  ds <- dataset("test", as.data.frame(setNames(nm = get_cdisc_keys("ADSL"))))
  testthat::expect_length(get_keys(ds), 0)
  testthat::expect_equal(get_keys(as_cdisc(ds)), get_keys(ds))
})
