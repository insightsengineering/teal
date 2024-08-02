testthat::test_that("e2e: reporter tab is created when a module has reporter", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )

  teal_tabs <- rvest::html_elements(app$get_html_rvest(selector = "#teal-teal_modules-active_tab"), "a")
  tab_names <- setNames(
    rvest::html_attr(teal_tabs, "data-value"),
    rvest::html_text(teal_tabs)
  )
  testthat::expect_identical(
    tab_names,
    c("Module with Reporter" = "module_with_reporter", "Report previewer" = "report_previewer")
  )

  app$stop()
})

testthat::test_that("e2e: reporter tab is not created when a module has no reporter", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  teal_tabs <- rvest::html_elements(
    app$get_html_rvest(selector = "#teal-teal_modules-active_tab"),
    "a"
  )
  tab_names <- setNames(
    rvest::html_attr(teal_tabs, "data-value"),
    rvest::html_text(teal_tabs)
  )

  testthat::expect_identical(
    tab_names,
    c("Example Module" = "example_module")
  )

  app$stop()
})

testthat::test_that("e2e: adding a report card in a module adds it in the report previewer tab", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )

  app$click(NS(app$active_module_ns(), "reporter-add_report_card_simple-add_report_card_button"))

  app$set_input(
    NS(app$active_module_ns(), "reporter-add_report_card_simple-label"),
    "Card name"
  )
  app$set_input(
    NS(app$active_module_ns(), "reporter-add_report_card_simple-label"),
    "Card name"
  )
  app$set_input(
    NS(app$active_module_ns(), "reporter-add_report_card_simple-comment"),
    "Card comment"
  )

  app$click(NS(app$active_module_ns(), "reporter-add_report_card_simple-add_card_ok"))

  app$navigate_teal_tab("Report previewer")

  accordian_selector <- sprintf("#%s-pcards .accordion-toggle", app$active_module_ns())
  app$click(selector = accordian_selector)


  testthat::expect_match(
    app$get_text(selector = accordian_selector),
    "Card 1: Card name"
  )

  testthat::expect_match(
    app$get_text(selector = "#card1 pre"),
    "Card comment"
  )

  app$stop()
})
