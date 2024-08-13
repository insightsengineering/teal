testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("bookmark_manager_button is not rendered by default", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    options = list()
  )
  on.exit(app$stop())
  testthat::expect_null(
    app$get_html(".bookmark_manager_button")
  )
})


testthat::test_that("bookmark_manager_button is not rendered when enableBookmarking = 'url'", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    options = list(shiny.bookmarkStore = "url")
  )
  on.exit(app$stop())
  testthat::expect_null(
    app$get_html(".bookmark_manager_button")
  )
})


testthat::test_that("bookmark_manager_button is rendered when enableBookmarking = 'server'", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    options = list(shiny.bookmarkStore = "server")
  )
  on.exit(app$stop())
  testthat::expect_true(!is.null(app$get_html(".bookmark_manager_button")))
})

testthat::test_that("bookmark_manager_button shows modal with url containing state_id when clicked", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    options = list(shiny.bookmarkStore = "server")
  )
  bookmark_button_id <- app$get_attr(".bookmark_manager_button", "id")
  app$click(bookmark_button_id)

  testthat::expect_match(
    rvest::html_text(app$get_html_rvest("div[id$=bookmark_modal] pre")),
    "_state_id_"
  )
})
