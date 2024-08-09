testthat::test_that("report_previewer_module has specific classes", {
  testthat::expect_s3_class(
    reporter_previewer_module(),
    c("teal_module_previewer", "teal_module")
  )
})

testthat::test_that("report_previewer_module throws error if label is not string", {
  testthat::expect_error(
    reporter_previewer_module(label = 5), "Assertion on 'label' failed: Must be of type 'string'"
  )
  testthat::expect_error(
    reporter_previewer_module(label = c("A", "B")), "Assertion on 'label' failed: Must have length 1."
  )
})

testthat::test_that("report_previewer_module throws no error and stores label if label is string", {
  testthat::expect_no_error(r_p_m <- reporter_previewer_module(label = "My label"))
  testthat::expect_equal(r_p_m$label, "My label")
})

testthat::test_that("report_previewer_module default label is Report previewer ", {
  r_p_m <- reporter_previewer_module()
  testthat::expect_equal(r_p_m$label, "Report previewer")
})

testthat::test_that(
  "report_previewer_module does not accept server_args out of formals(teal.reporter::reporter_previewer_srv) ",
  {
    error_pattern <- ".*Assertion on \\'all\\(names\\(server_args"
    testthat::expect_error(
      reporter_previewer_module(server_args = list(x = "A")),
      error_pattern
    )
    testthat::expect_error(
      reporter_previewer_module(server_args = list(reporter = "A", global_knitr = 5, d = 1)),
      error_pattern
    )
  }
)
