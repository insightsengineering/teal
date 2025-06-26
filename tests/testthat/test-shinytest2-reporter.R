testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: reporter tab is created when a module has reporter + report_fun", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Module with Reporter")
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

testthat::test_that("e2e: reporter card can be customized", {
  skip("TODO")
})

testthat::test_that("e2e: reporter tab is not created when a module has no reporter", {
  testthat::skip("chromium")
  skip("TODO - change to reporter button is not visible if module has no report_card")
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

testthat::test_that("e2e: adding a report card with global button adds it in the report previewer tab", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Module with Reporter")
  )

  app$click(NS(app$active_module_ns(), "reporter_add-add_report_card_button"))

  app$set_input(
    NS(app$active_module_ns(), "reporter_add-label"),
    "Card name"
  )

  app$click(NS(app$active_module_ns(), "reporter_add-add_card_ok"))

  app$navigate_teal_tab("Report previewer")

  accordian_selector <- sprintf("#%s-pcards .accordion-toggle", app$active_module_ns())
  app$click(selector = accordian_selector)


  testthat::expect_match(
    app$get_text(selector = accordian_selector),
    "Card name"
  )

  app$stop()
})

testthat::test_that("e2e: reporter_previewer_module has download, load and reset buttons", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app <- teal:::TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module with Reporter")
  )

  app$navigate_teal_tab("Report previewer")

  testthat::expect_true(
    app$is_visible(app$active_module_element("download_data_prev"))
  )
  testthat::expect_true(
    app$is_visible(app$active_module_element("load_reporter_previewer"))
  )
  testthat::expect_true(
    app$is_visible(app$active_module_element("resetButtonPreviewer-reset_reporter"))
  )
})

testthat::test_that("e2e: reporter_previewer_module do not show data_summary nor filter_panel", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app <- teal:::TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Module with Reporter")
  )

  app$navigate_teal_tab("Report previewer")

  testthat::expect_null(app$is_visible(app$active_data_summary_element("table")))

  testthat::expect_null(app$get_active_filter_vars())

  app$stop()
})
