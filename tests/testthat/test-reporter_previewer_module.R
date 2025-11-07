testthat::test_that("reporter_previewer_module returns teal_module with previewer class", {
  module <- reporter_previewer_module(label = "test")
  testthat::expect_s3_class(module, "teal_module")
  testthat::expect_true("teal_module_previewer" %in% class(module))
  testthat::expect_equal(module$label, "test")
})

testthat::test_that("reporter_previewer_module throws error when label is not a string", {
  testthat::expect_error(
    reporter_previewer_module(label = 123),
    "Assertion on 'label' failed"
  )
})

testthat::test_that("reporter_previewer_module throws error when server_args is not a named list", {
  testthat::expect_error(
    reporter_previewer_module(label = "test", server_args = list(1, 2)),
    "Assertion on 'server_args' failed"
  )
})

testthat::test_that("reporter_previewer_module throws error when server_args has invalid names", {
  testthat::expect_error(
    reporter_previewer_module(label = "test", server_args = list(invalid_arg = 1)),
    "Assertion on 'all\\(names\\(server_args\\) %in% names\\(formals\\(teal\\.reporter::reporter_previewer_srv\\)\\)\\)' failed"
  )
})

testthat::test_that("reporter_previewer_module accepts valid server_args", {
  testthat::expect_no_error(
    reporter_previewer_module(
      label = "test",
      server_args = list()
    )
  )
})

