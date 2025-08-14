testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: reporter tab is created when a module has reporter", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = report_module(label = "Module with Reporter")
  )
  testthat::expect_true(length(app$get_html("#teal-reporter_menu_container")) > 0)
  app$stop()
})

testthat::test_that("e2e: reporter tab is not created when a module has no reporter", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  testthat::expect_null(app$get_html("#teal-reporter_menu_container"))
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
  app$run_js("document.querySelector('#teal-preview_report-preview_button').click();") # skipping menu hovering
  app$wait_for_idle()
  accordion_selector <- "#teal-preview_report-preview_content-reporter_cards"
  testthat::expect_identical(app$get_text(selector = paste(accordion_selector, ".accordion-title")), "Card name")
  testthat::expect_match(app$get_text(selector = paste(accordion_selector, ".accordion-body")), "Card comment")

  app$stop()
})
