testthat::test_that("TealDataAbstract cannot be instantiated", {
  testthat::expect_error(TealDataAbstract$new(), "Pure virtual method")
})
