testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::describe("e2e: modify_title sets custom title in the page title (`head title`)", {
  testthat::it("displays custom title in the app", {
    skip_if_too_deep(5)
    
    app_driver <- TealAppDriver$new(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      title_args = list(title = "Custom Test Title")
    )
    
    # Check that the title is present in the page
    page_title <- app_driver$get_text("head title")
    testthat::expect_equal(page_title, "Custom Test Title")
    
    app_driver$stop()
  })

  testthat::it("displays custom favicon in the app", {
    skip_if_too_deep(5)
    
    custom_favicon <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png"
    app_driver <- TealAppDriver$new(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      title_args = list(title = "Test App", favicon = custom_favicon)
    )
    
    # Check that favicon link is present
    testthat::expect_true(app_driver$is_visible("link[rel='icon']"))
    
    app_driver$stop()
  })
})

testthat::describe("e2e: modify_header", {
  testthat::it("displays custom header in the app", {
    skip_if_too_deep(5)
    
    app_driver <- TealAppDriver$new(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      header = tags$h1("Custom App Header")
    )
    
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
    
    app_driver <- TealAppDriver$new(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      footer = tags$p("Custom Footer Text")
    )
    
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
    
    app_driver <- TealAppDriver$new(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      landing_popup_args = list(
        title = "Welcome to the App",
        content = "Please read these instructions before proceeding."
      )
    )
    
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
    
    app_driver <- TealAppDriver$new(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      landing_popup_args = list(
        title = "Welcome",
        content = "Test content",
        footer = modalButton("Accept")
      )
    )
    
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
    
    app_driver <- TealAppDriver$new(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      title_args = list(title = "Complete Custom App"),
      header = tags$div("Custom Header"),
      footer = tags$div("Custom Footer"),
      landing_popup_args = list(
        title = "Welcome",
        content = "Welcome message"
      )
    )
    
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
