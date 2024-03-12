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
