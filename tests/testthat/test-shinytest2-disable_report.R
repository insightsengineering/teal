testthat::describe("srv_teal teal_modules disable_report", {
  testthat::it("Button is active on a module", {
    testthat::skip_if_not_installed("shinytest2")
    skip_if_too_deep(5)
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "m1")
    )


    expect_true(endsWith(app$get_attr(
      selector = "#teal-teal_modules-nav-m1-add_reporter_wrapper-reporter_add-add_report_card_button",
      attribute = "class"
    ), "shiny-bound-input"))

    app$stop()
  })
  testthat::it("Button is disable on a module", {
    testthat::skip_if_not_installed("shinytest2")
    skip_if_too_deep(5)
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "m1") |> disable_report()
    )


    expect_true(endsWith(app$get_attr(
      selector = "#teal-teal_modules-nav-m1-add_reporter_wrapper-reporter_add-add_report_card_button",
      attribute = "class"
    ), "shiny-bound-input disabled"))

    app$stop()
  })

  testthat::it("Button is active on nested module", {
    testthat::skip_if_not_installed("shinytest2")
    skip_if_too_deep(5)
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = modules(
        example_module(label = "m1"),
        example_module(label = "m2")
      )
    )

    expect_true(endsWith(app$get_attr(
      selector = "#teal-teal_modules-nav-m1-add_reporter_wrapper-reporter_add-add_report_card_button",
      attribute = "class"
    ), "shiny-bound-input"))

    app$stop()
  })

  testthat::it("Disables button on nested modules", {
    testthat::skip_if_not_installed("shinytest2")
    skip_if_too_deep(5)
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = modules(
        example_module(label = "m1"),
        example_module(label = "m2")
      ) |> disable_report()
    )

    expect_true(endsWith(app$get_attr(
      selector = "#teal-teal_modules-nav-m1-add_reporter_wrapper-reporter_add-add_report_card_button",
      attribute = "class"
    ), "shiny-bound-input disabled"))

    app$stop()
  })
})
