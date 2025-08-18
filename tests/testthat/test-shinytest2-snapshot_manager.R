testthat::test_that("e2e: wunder_bar_srv clicking snapshot icon opens snapshot-manager modal", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )

  testthat::expect_null(app$get_text(".snapshot_manager_modal"))
  app$click("teal-snapshot_manager_panel-show_snapshot_manager")
  testthat::expect_match(app$get_text(".snapshot_manager_modal"), "Snapshot manager")
})
