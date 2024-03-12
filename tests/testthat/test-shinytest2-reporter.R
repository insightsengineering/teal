testthat::test_that("e2e: reporter tab is only created when a module has reporter", {
  app_without_reporter <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  app_with_reporter <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )

  teal_tabs <- app_with_reporter$get_html(selector = "#teal-main_ui-root-active_tab") %>%
    rvest::read_html() %>%
    rvest::html_elements("a")
  reporter_tabs <- setNames(
    rvest::html_attr(teal_tabs, "data-value"),
    rvest::html_text(teal_tabs)
  )
  teal_tabs <- app_without_reporter$get_html(selector = "#teal-main_ui-root-active_tab") %>%
    rvest::read_html() %>%
    rvest::html_elements("a")
  non_reporter_tabs <- setNames(
    rvest::html_attr(teal_tabs, "data-value"),
    rvest::html_text(teal_tabs)
  )

  testthat::expect_identical(
    non_reporter_tabs,
    c("Example Module" = "example_module")
  )
  testthat::expect_identical(
    reporter_tabs,
    c("Module with Reporter" = "module_with_reporter", "Report previewer" = "report_previewer")
  )

  app_without_reporter$stop()
  app_with_reporter$stop()
})

testthat::test_that("e2e: adding a report card in a module adds it in the report previewer tab", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  app$click(NS(app$active_module_ns(), "reporter-add_report_card_simple-add_report_card_button"))
  app$wait_for_idle(timeout = default_idle_timeout)

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
