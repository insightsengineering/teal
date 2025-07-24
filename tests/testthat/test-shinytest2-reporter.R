testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: reporter tab is created when a module has reporter", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )

  teal_tabs <- rvest::html_elements(
    app$get_html_rvest(selector = "#teal-teal_modules-active_module_id .dropdown-menu"),
    "a"
  )
  testthat::expect_identical(
    rvest::html_text(teal_tabs),
    c("Module with Reporter", "Report previewer")
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
    app$get_html_rvest(selector = "#teal-teal_modules-active_module_id .dropdown-menu"),
    "a"
  )
  testthat::expect_identical(rvest::html_text(teal_tabs), "Example Module")

  app$stop()
})

testthat::test_that("e2e: adding a report card in a module adds it in the report previewer tab", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )

  # Add new card with label and comment
  app$click(NS(app$active_module_ns(), "reporter-add_report_card_simple-add_report_card_button"))
  app$set_input(
    NS(app$active_module_ns(), "reporter-add_report_card_simple-label"),
    "Card name"
  )
  app$set_input(
    NS(app$active_module_ns(), "reporter-add_report_card_simple-comment"),
    "Card comment"
  )
  app$click(NS(app$active_module_ns(), "reporter-add_report_card_simple-add_card_ok"))

  # Check whether card was added
  app$navigate_teal_tab("Report previewer")
  accordion_selector <- sprintf("#%s-pcards .accordion-toggle", app$active_module_ns())
  app$click(selector = accordion_selector)

  testthat::expect_match(app$get_text(selector = accordion_selector), "Card 1: Card name")
  testthat::expect_match(app$get_text(selector = "#card1 pre"), "Card comment")

  app$stop()
})

testthat::test_that("e2e: reporter_previewer_module do not show data_summary nor filter_panel", {
  skip_if_too_deep(5)
  app <- teal:::TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )

  app$navigate_teal_tab("Report previewer")

  testthat::expect_null(app$is_visible(app$active_data_summary_element("table")))
  testthat::expect_null(app$get_active_filter_vars())

  app$stop()
})
