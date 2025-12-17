testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")
skip_if_too_deep(5)

testthat::test_that(
  "e2e: modify_title sets custom title in the page title (`head title`) displays custom title in the app",
  {
    app_driver <- TealAppDriver$new(
      init(
        data = teal.data::teal_data(iris = iris),
        modules = modules(example_module())
      ) |>
        modify_title(title = "Custom Test Title")
    )

    # Check that the title is present in the page
    page_title <- app_driver$get_text("head title")
    testthat::expect_equal(page_title[1], "Custom Test Title")

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: modify_title sets custom title in the page title (`head title`) displays custom favicon in the app",
  {
    custom_favicon <- "test.png"
    app_driver <- TealAppDriver$new(
      init(
        data = teal.data::teal_data(iris = iris),
        modules = modules(example_module())
      ) |>
        modify_title(title = "Test App", favicon = custom_favicon)
    )

    testthat::expect_identical(app_driver$get_attr("link[rel='icon']", "href"), custom_favicon)

    app_driver$stop()
  }
)

testthat::test_that("e2e: modify_header displays custom header in the app", {
  app_driver <- TealAppDriver$new(init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  ) |> modify_header(element = tags$h1("Custom App Header")))

  header_text <- app_driver$get_text("#teal-header-content")
  testthat::expect_equal(trimws(header_text), "Custom App Header")

  app_driver$stop()
})

testthat::test_that("e2e: modify_footer displays custom footer in the app", {
  app_driver <- TealAppDriver$new(init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  ) |>
    modify_footer(tags$p("Custom Footer Text")))

  footer_text <- app_driver$get_text("#teal-footer-content")
  testthat::expect_equal(trimws(footer_text), "Custom Footer Text")

  app_driver$stop()
})

testthat::test_that("e2e: add_landing_modal displays landing modal on app startup", {
  app_driver <- TealAppDriver$new(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ) |> add_landing_modal(
      title = "Welcome to the App",
      content = "Please read these instructions before proceeding."
    )
  )

  app_driver$expect_visible(".modal")
  modal_title <- app_driver$get_text(".modal-title")
  testthat::expect_equal(modal_title, "Welcome to the App")
  modal_body <- app_driver$get_text(".modal-body")
  testthat::expect_match(modal_body, "Please read these instructions")

  app_driver$stop()
})

testthat::test_that("e2e: add_landing_modal modal can be dismissed", {
  app_driver <- TealAppDriver$new(init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  ) |> add_landing_modal(
    title = "Welcome",
    content = "Test content",
    footer = modalButton("Accept")
  ))

  app_driver$expect_visible(".modal")
  # because $click(button:contains('Accept')) doesn't work
  app_driver$get_js("document.querySelector('#shiny-modal-wrapper button').click()")
  Sys.sleep(0.5) # Wait a moment for modal to close
  app_driver$expect_hidden(".modal")

  app_driver$stop()
})

testthat::test_that("e2e: combined modifiers displays all customizations when chained together", {
  app_driver <- TealAppDriver$new(init(
    data = teal.data::teal_data(iris = iris),
    modules = modules(example_module())
  ) |>
    modify_title(title = "Complete Custom App") |>
    modify_header(tags$div("Custom Header")) |>
    modify_footer(tags$div("Custom Footer")) |>
    add_landing_modal(
      title = "Welcome",
      content = "Welcome message"
    ))

  # Check title
  page_title <- app_driver$get_text("head title")
  testthat::expect_equal(page_title[1], "Complete Custom App")

  # Check modal is visible
  app_driver$expect_visible(".modal")

  # Dismiss modal
  # because $click(button:contains('Accept')) doesn't work
  app_driver$get_js("document.querySelector('#shiny-modal-wrapper button').click()")
  Sys.sleep(0.5)

  # Check header is visible
  app_driver$expect_visible("#teal-header-content")
  header_text <- app_driver$get_text("#teal-header-content")
  testthat::expect_equal(trimws(header_text), "Custom Header")

  # Check footer is visible
  app_driver$expect_visible("#teal-footer-content")
  footer_text <- app_driver$get_text("#teal-footer-content")
  testthat::expect_equal(trimws(footer_text), "Custom Footer")

  app_driver$stop()
})
