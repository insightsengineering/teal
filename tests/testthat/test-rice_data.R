# Single rice_data connector ----
testthat::test_that("Single rice_data connector with two rice dataset connectors", {
  if (!"rice" %in% installed.packages()) {
    testthat::skip("rice package not available")
  }
  adsl <- rice_cdisc_dataset_connector(dataname = "ADSL", "/path/to/ADSL")
  adlb <- rice_cdisc_dataset_connector("ADLB", "/path/to/ADLB")
  adsl_adlb <- rice_data(adsl, adlb)

  items <- adsl_adlb$get_items()
  testthat::expect_true(inherits(adsl_adlb, "RelationalDataConnector"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))

  testthat::expect_equal(
    items$ADSL$get_pull_callable()$get_call(),
    "rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE)"
  )
  testthat::expect_equal(
    items$ADLB$get_pull_callable()$get_call(),
    "rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE)"
  )

  testthat::expect_equal(
    adsl_adlb$get_code("ADSL"),
    "rice::rice_session_open(password = askpass::askpass())\nADSL <- rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE)\nrice::rice_session_close(message = FALSE)" # nolint
  )
  testthat::expect_equal(
    adsl_adlb$get_code("ADLB"),
    "rice::rice_session_open(password = askpass::askpass())\nADLB <- rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE)\nrice::rice_session_close(message = FALSE)" # nolint
  )
  testthat::expect_equal(
    adsl_adlb$get_code(),
    "rice::rice_session_open(password = askpass::askpass())\nADSL <- rice::rice_read(node = \"/path/to/ADSL\", prolong = TRUE)\nADLB <- rice::rice_read(node = \"/path/to/ADLB\", prolong = TRUE)\nrice::rice_session_close(message = FALSE)" # nolint
  )
})
