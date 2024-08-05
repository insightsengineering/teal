testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("wunder_bar_srv clicking filter icon opens filter-manager modal", {
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

  testthat::expect_true(is.null(app$get_text(".filter_manager_modal")))
  app$click(filter_manager_btn_id)
  testthat::expect_true(!is.null(app$get_text(".filter_manager_modal")))
})


testthat::test_that("wunder_bar_srv clicking snapshot icon opens snapshot-manager modal", {
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

  testthat::expect_true(is.null(app$get_text(".snapshot_manager_modal")))
  app$click(snapshot_manager_btn_id)
  testthat::expect_true(!is.null(app$get_text(".snapshot_manager_modal")))
})
