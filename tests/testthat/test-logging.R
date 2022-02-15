testthat::test_that(".log is soft-deprecated", {
  lifecycle::expect_deprecated(utils::capture.output(.log("something")))
})
