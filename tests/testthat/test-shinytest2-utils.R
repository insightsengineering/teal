testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: show/hide hamburger works as expected", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module()
  )

  get_class_attributes <- function(app, selector) {
    element <- rvest::html_elements(app$get_html_rvest(selector = selector), selector)
    list(
      class = rvest::html_attr(element, "class"),
      style = rvest::html_attr(element, "style")
    )
  }

  primary_attrs <- get_class_attributes(app, ".teal_primary_col")
  secondary_attrs <- get_class_attributes(app, ".teal_secondary_col")

  testthat::expect_true(grepl("col-sm-9", primary_attrs$class))
  testthat::expect_true(is.na(secondary_attrs$style))

  app$click(selector = ".btn.action-button.filter_hamburger")
  primary_attrs <- get_class_attributes(app, ".teal_primary_col")
  secondary_attrs <- get_class_attributes(app, ".teal_secondary_col")

  testthat::expect_true(grepl("col-sm-12", primary_attrs$class))
  testthat::expect_true(grepl("display: none;", secondary_attrs$style))
  app$stop()
})
