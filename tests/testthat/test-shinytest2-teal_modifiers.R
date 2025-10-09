testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::describe("e2e: modify_title sets custom title in the page title (`head title`)", {
  testthat::it("displays custom title in the app", {
    skip_if_too_deep(5)
    
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ) |>
      modify_title(title = "Custom Test Title")
    
    app_driver <- TealAppDriver$new(app)
    
    # Check that the title is present in the page
    page_title <- app_driver$get_text("head title")
    testthat::expect_equal(page_title, "Custom Test Title")
    
    app_driver$stop()
  })

  testthat::it("displays custom favicon in the app", {
    skip_if_too_deep(5)
    
    custom_favicon <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png"
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ) |>
      modify_title(title = "Test App", favicon = custom_favicon)
    
    app_driver <- TealAppDriver$new(app)
    
    # Check that favicon link is present
    testthat::expect_true(app_driver$is_visible("link[rel='icon']"))
    
    app_driver$stop()
  })
})

testthat::describe("e2e: modify_header", {
  testthat::it("displays custom header in the app", {
    skip_if_too_deep(5)
    
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ) |>
      modify_header(element = tags$h1("Custom App Header"))
    
    app_driver <- TealAppDriver$new(app)
    
    # Check that the header content is visible
    testthat::expect_true(app_driver$is_visible("#teal-header-content"))
    header_text <- app_driver$get_text("#teal-header-content")
    testthat::expect_equal(header_text, "Custom App Header")
    
    app_driver$stop()
  })
})

testthat::describe("e2e: modify_footer", {
  testthat::it("displays custom footer in the app", {
    skip_if_too_deep(5)
    
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ) |>
      modify_footer(element = tags$p("Custom Footer Text"))
    
    app_driver <- TealAppDriver$new(app)
    
    # Check that the footer content is visible
    testthat::expect_true(app_driver$is_visible("#teal-footer-content"))
    footer_text <- app_driver$get_text("#teal-footer-content")
    testthat::expect_equal(footer_text, "Custom Footer Text")
    
    app_driver$stop()
  })
})

testthat::describe("e2e: add_landing_modal", {
  testthat::it("displays landing modal on app startup", {
    skip_if_too_deep(5)
    
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ) |>
      add_landing_modal(
        title = "Welcome to the App",
        content = "Please read these instructions before proceeding."
      )
    
    app_driver <- TealAppDriver$new(app)
    
    # Check that modal is visible on startup
    testthat::expect_true(app_driver$is_visible(".modal"))
    
    # Check modal title
    modal_title <- app_driver$get_text(".modal-title")
    testthat::expect_equal(modal_title, "Welcome to the App")
    
    # Check modal content is present
    modal_body <- app_driver$get_text(".modal-body")
    testthat::expect_match(modal_body, "Please read these instructions")
    
    app_driver$stop()
  })

  testthat::it("modal can be dismissed", {
    skip_if_too_deep(5)
    
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ) |>
      add_landing_modal(
        title = "Welcome",
        content = "Test content",
        footer = modalButton("Accept")
      )
    
    app_driver <- TealAppDriver$new(app)
    
    # Modal should be visible initially
    testthat::expect_true(app_driver$is_visible(".modal"))
    
    # Click the Accept button
    app_driver$click("button:contains('Accept')")
    
    # Wait a moment for modal to close
    Sys.sleep(0.5)
    
    # Modal should no longer be visible
    testthat::expect_false(app_driver$is_visible(".modal"))
    
    app_driver$stop()
  })
})

testthat::describe("e2e: combined modifiers", {
  testthat::it("displays all customizations when chained together", {
    skip_if_too_deep(5)
    
    app <- init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ) |>
      modify_title(title = "Complete Custom App") |>
      modify_header(element = tags$div("Custom Header")) |>
      modify_footer(element = tags$div("Custom Footer")) |>
      add_landing_modal(title = "Welcome", content = "Welcome message")
    
    app_driver <- TealAppDriver$new(app)
    
    # Check title
    page_title <- app_driver$get_text("head title")
    testthat::expect_equal(page_title, "Complete Custom App")
    
    # Check modal is visible
    testthat::expect_true(app_driver$is_visible(".modal"))
    
    # Dismiss modal
    app_driver$click("button:contains('Accept')")
    Sys.sleep(0.5)
    
    # Check header is visible
    testthat::expect_true(app_driver$is_visible("#teal-header-content"))
    header_text <- app_driver$get_text("#teal-header-content")
    testthat::expect_equal(header_text, "Custom Header")
    
    # Check footer is visible
    testthat::expect_true(app_driver$is_visible("#teal-footer-content"))
    footer_text <- app_driver$get_text("#teal-footer-content")
    testthat::expect_equal(footer_text, "Custom Footer")
    
    app_driver$stop()
  })
})
