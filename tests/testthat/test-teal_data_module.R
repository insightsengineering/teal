testthat::test_that("teal_data_module returns teal_data_module", {
  testthat::expect_s3_class(
    teal_data_module(ui = function(id) div(), server = function(id) NULL),
    "teal_data_module"
  )
})

testthat::test_that("teal_data_module throws when ui has other formals than id only", {
  testthat::expect_error(
    teal_data_module(ui = function(id, x) div(), server = function(id) NULL),
    "Must have exactly 1 formal arguments"
  )
})

testthat::test_that("teal_data_module throws when server has other formals than id only", {
  testthat::expect_error(
    teal_data_module(ui = function(id) div(), server = function(id, x) NULL),
    "Must have exactly 1 formal arguments"
  )
})
