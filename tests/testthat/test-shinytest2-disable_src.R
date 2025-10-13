testthat::describe("srv_teal teal_modules disable_src", {
  testthat::skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
    testthat::it("Button is disable on a module", {
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "m1") |> disable_src()
    )


    expect_true(endsWith(app$get_attr(
      selector = "#teal-teal_modules-nav-m1-source_code_wrapper-source_code-button",
      attribute = "class"
    ), "shiny-bound-input disabled"))

    app$stop()
  })

  testthat::it("Disables button on nested modules", {
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = modules(
        example_module(label = "m1"),
        example_module(label = "m2")
      ) |> disable_src()
    )

    expect_true(endsWith(app$get_attr(
      selector = "#teal-teal_modules-nav-m1-source_code_wrapper-source_code-button",
      attribute = "class"
    ), "shiny-bound-input disabled"))

    app$stop()
  })
})
