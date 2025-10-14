testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")
skip_if_too_deep(5)
skip("todo: error")

bookmark_manager_selector <- "button[id$='bookmark_manager-do_bookmark']"
testthat::test_that("bookmark_manager_button is not rendered by default", {
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module"),
      options = list()
    )
  )
  testthat::expect_null(app$get_html(bookmark_manager_selector))
  app$stop()
})

testthat::test_that("bookmark_manager_button is not rendered when enableBookmarking = 'url'", {
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module"),
      options = list(shiny.bookmarkStore = "url")
    )
  )
  testthat::expect_null(app$get_html(bookmark_manager_selector))
  app$stop()
})

testthat::test_that("bookmark_manager_button is rendered when enableBookmarking = 'server'", {
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module"),
      options = list(shiny.bookmarkStore = "server")
    )
  )
  testthat::expect_type(app$get_html(bookmark_manager_selector), "character")
  app$stop()
})

testthat::test_that("bookmark_manager_button shows modal with url containing state_id when clicked", {
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module"),
      options = list(shiny.bookmarkStore = "server")
    )
  )
  bookmark_button_id <- app$get_attr(bookmark_manager_selector, "id")
  app$click(bookmark_button_id)

  testthat::expect_match(
    rvest::html_text(app$get_html_rvest("div[id$=bookmark_modal] pre")),
    "_state_id_=[a-zA-Z0-9]+", # bookmark link has `_state_id_=<hex code from shiny bookmark>`
  )
  app$stop()
})
