testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: wunder_bar_srv clicking filter icon opens filter-manager modal", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  testthat::expect_null(app$get_text(".teal-filter-manager-modal"))
  app$click("teal-filter_manager_panel-show_filter_manager")
  testthat::expect_match(app$get_text(".teal-filter-manager-modal"), "Filter manager")
})
