testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

bookmark_manager_selector <- "button[id$='bookmark_manager-do_bookmark']"
testthat::describe("bookmark_manager_button is", {
  it("not rendered by default", {
    skip_if_too_deep(5)
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module"),
      options = list()
    )
    testthat::expect_null(app$get_html(bookmark_manager_selector))
    app$stop()
  })

  it("not rendered when enableBookmarking = 'url'", {
    skip_if_too_deep(5)
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module"),
      options = list(shiny.bookmarkStore = "url")
    )
    testthat::expect_null(app$get_html(bookmark_manager_selector))
    app$stop()
  })

  it("rendered when enableBookmarking = 'server'", {
    skip_if_too_deep(5)
    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = example_module(label = "Example Module"),
      options = list(shiny.bookmarkStore = "server")
    )
    testthat::expect_type(app$get_html(bookmark_manager_selector), "character")
    app$stop()
  })
})

testthat::test_that("bookmark_manager_button shows modal with url containing state_id when clicked", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    options = list(shiny.bookmarkStore = "server")
  )
  bookmark_button_id <- app$get_attr(bookmark_manager_selector, "id")
  app$click(bookmark_button_id)

  testthat::expect_match(
    rvest::html_text(app$get_html_rvest("div[id$=bookmark_modal] pre")),
    "_state_id_=[a-zA-Z0-9]+", # bookmark link has `_state_id_=<hex code from shiny bookmark>`
  )
  app$stop()
})
