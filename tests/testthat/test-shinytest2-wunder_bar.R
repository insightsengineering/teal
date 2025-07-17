testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: wunder_bar_srv clicking filter icon opens filter-manager modal", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )

  filter_manager_btn_id <- grep(
    "filter_manager",
    x = app$get_attr(".wunder_bar_button", "id"),
    value = TRUE
  )

  testthat::expect_null(app$get_text(".teal-filter-manager-modal"))
  app$click(filter_manager_btn_id)
  testthat::expect_type(app$get_text(".teal-filter-manager-modal"), "character")
})


testthat::test_that("e2e: wunder_bar_srv clicking snapshot icon opens snapshot-manager modal", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )

  snapshot_manager_btn_id <- grep(
    "snapshot_manager",
    x = app$get_attr(".wunder_bar_button", "id"),
    value = TRUE
  )

  testthat::expect_null(app$get_text(".snapshot_manager_modal"))
  app$click(snapshot_manager_btn_id)
  testthat::expect_type(app$get_text(".snapshot_manager_modal"), "character")
})

testthat::test_that("e2e: collapsing wunderbar hides filter and data summary", {
  skip_if_too_deep(5)

  data <- teal.data::teal_data(mtcars1 = mtcars, mtcars2 = data.frame(am = c(0, 1), test = c("a", "b")))
  app <- TealAppDriver$new(data = data, modules = example_module())
  app$click(selector = ".teal_module.active > .bslib-sidebar-layout > button.collapse-toggle")
  testthat::expect_false(app$is_visible(".teal-filter-panel"))
  testthat::expect_false(app$is_visible(".teal-active-data-summary-panel"))
  app$expect_screenshot(selector = ".bslib-sidebar-layout .main")
  app$stop()
})

testthat::test_that("e2e: wunderbar filter and data summary are hidden on narrow windows", {
  skip_if_too_deep(5)

  data <- teal.data::teal_data(mtcars1 = mtcars, mtcars2 = data.frame(am = c(0, 1), test = c("a", "b")))
  app <- TealAppDriver$new(data = data, modules = example_module(), width = 300, height = 1080)
  testthat::expect_false(app$is_visible(".teal-filter-panel"))
  testthat::expect_false(app$is_visible(".teal-active-data-summary-panel"))

  app$stop()
})
