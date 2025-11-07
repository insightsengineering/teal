testthat::test_that("new_tdata throws deprecation error", {
  testthat::expect_error(
    new_tdata(),
    "tdata has been removed"
  )
})

testthat::test_that("tdata2env throws deprecation error", {
  testthat::expect_error(
    tdata2env(),
    "tdata has been removed"
  )
})

testthat::test_that("get_code_tdata throws deprecation error", {
  testthat::expect_error(
    get_code_tdata(),
    "tdata has been removed"
  )
})

testthat::test_that("get_metadata throws deprecation error", {
  testthat::expect_error(
    get_metadata(),
    "tdata has been removed"
  )
})

testthat::test_that("as_tdata throws deprecation error", {
  testthat::expect_error(
    as_tdata(),
    "tdata has been removed"
  )
})

