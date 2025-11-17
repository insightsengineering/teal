testthat::describe("reporter_previewer_module", {
  testthat::it("returns teal_module with previewer class", {
    module <- reporter_previewer_module(label = "test")
    testthat::expect_s3_class(module, "teal_module")
    testthat::expect_true("teal_module_previewer" %in% class(module))
    testthat::expect_equal(module$label, "test")
    testthat::expect_equal(module$path, "test")
    testthat::expect_true(isTRUE(attr(module, "teal_bookmarkable")))
  })

  testthat::it("uses default label", {
    module <- reporter_previewer_module()
    testthat::expect_equal(module$label, "Report Previewer")
    testthat::expect_equal(module$path, "Report Previewer")
  })

  testthat::it("throws error when label is not a string", {
    testthat::expect_error(
      reporter_previewer_module(label = 123),
      "Assertion on 'label' failed"
    )
  })

  testthat::it("throws error when server_args is not a named list", {
    testthat::expect_error(
      reporter_previewer_module(label = "test", server_args = list(1, 2)),
      "Assertion on 'server_args' failed"
    )
  })

  testthat::it("throws error when server_args has invalid names", {
    testthat::expect_error(
      reporter_previewer_module(label = "test", server_args = list(invalid_arg = 1)),
      paste0(
        "Assertion on 'all\\(names\\(server_args\\) %in% names\\(formals\\",
        "(teal\\.reporter::reporter_previewer_srv\\)\\)\\)' failed"
      )
    )
  })

  testthat::it("accepts valid server_args", {
    testthat::expect_no_error(
      reporter_previewer_module(
        label = "test",
        server_args = list()
      )
    )
  })

  testthat::it("creates module with server and ui functions", {
    module <- reporter_previewer_module(label = "test")
    testthat::expect_true(is.function(module$server))
    testthat::expect_true(is.function(module$ui))
  })

  testthat::it("stores server_args", {
    server_args <- list(previewer_buttons = c("preview", "download"))
    module <- reporter_previewer_module(label = "test", server_args = server_args)
    testthat::expect_equal(module$server_args, server_args)
  })

  testthat::it("processes multiple server_args", {
    server_args <- list(
      previewer_buttons = c("preview", "download"),
      global_knitr = list(echo = FALSE),
      rmd_yaml_args = list(title = "Test Report"),
      rmd_output = "html_document"
    )

    previewer_module <- reporter_previewer_module(
      label = "test",
      server_args = server_args
    )

    testthat::expect_equal(previewer_module$server_args, server_args)
  })
})
