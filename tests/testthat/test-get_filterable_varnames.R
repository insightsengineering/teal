testthat::test_that("get_filterable_varnames returns character(0) for an empty matrix", {
  testthat::expect_equal(get_filterable_varnames(matrix()), character(0))
})

testthat::test_that("get_filterable_varnames returns character(0) for an empty data.frame", {
  testthat::expect_equal(get_filterable_varnames(data.frame()), character(0))
})

testthat::test_that("get_filterable_varnames returns character(0) for a single column numeric matrix", {
  testthat::expect_equal(get_filterable_varnames(matrix(c(1, 2, 3))), character(0))
})

testthat::test_that("get_filterable_varnames returns character(0) for a single column of NA values", {
  testthat::expect_equal(get_filterable_varnames(matrix(c(NA, NA))), character(0))
})

testthat::test_that("get_filterable_varnames returns column names for a non-empty data.frame", {
  testthat::expect_equal(get_filterable_varnames(as.data.frame(list(a = 1, b = 2))), c("a", "b"))
})
