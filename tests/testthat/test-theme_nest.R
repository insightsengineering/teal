testthat::test_that("theme_nest returns `ggplot2::theme_bw`", {
  testthat::expect_identical(theme_nest(), ggplot2::theme_bw())
})
