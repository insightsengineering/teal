testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: teal app initializes with no errors", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  app$expect_no_shiny_error()
  app$expect_screenshot(selector = "#teal-tabpanel_wrapper")
  app$stop()
})

testthat::test_that("e2e: teal app initializes with sessionInfo modal", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )

  # Check if button exists.
  button_selector <- "#teal-footer-session_info-sessionInfo-button"
  testthat::expect_equal(
    app$get_text(button_selector),
    "Session Info"
  )

  app$click(selector = button_selector)

  # Check header and title content.
  testthat::expect_equal(
    app$get_text("#shiny-modal > div > div > div.modal-header > h4"),
    "SessionInfo"
  )

  # There are two Copy buttons with similar id and the same label.
  testthat::expect_setequal(
    testthat::expect_length(
      app$get_text(
        "#shiny-modal [id^='teal-footer-session_info-sessionInfo-copy_button']"
      ),
      2
    ),
    "Copy to Clipboard"
  )
  # There are two Dismiss buttons with similar id and the same label.
  testthat::expect_setequal(
    testthat::expect_length(
      app$get_text("#shiny-modal button[data-dismiss]"),
      2
    ),
    "Dismiss"
  )

  # Check session info output.
  session_info <- app$get_text("#teal-footer-session_info-sessionInfo-verbatim_content")

  testthat::expect_match(session_info, "R version", fixed = TRUE)
  testthat::expect_match(session_info, "attached base packages:", fixed = TRUE)
  testthat::expect_match(session_info, "loaded via a namespace (and not attached):", fixed = TRUE)

  testthat::expect_match(session_info, "shiny", fixed = TRUE)
  testthat::expect_match(session_info, "teal.slice", fixed = TRUE)
  testthat::expect_match(session_info, "teal.reporter", fixed = TRUE)

  app$stop()
})

testthat::test_that("e2e: init creates UI containing specified title, favicon, header and footer", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    title_args = list(
      title = "Custom Teal App Title",
      something_else = "asdfsdf",
      favicon = "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png"
    ),
    header = "Custom Teal App Header",
    footer = "Custom Teal App Footer"
  )

  testthat::expect_equal(
    app$get_text("head > title")[1],
    "Custom Teal App Title"
  )
  testthat::expect_equal(
    rvest::html_attr(
      rvest::html_elements(app$get_html_rvest("head > link[rel='icon']"), "link"),
      "href"
    ),
    "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png"
  )
  testthat::expect_match(
    app$get_text("header"),
    "Custom Teal App Header"
  )
  testthat::expect_match(
    app$get_text("footer"),
    "Custom Teal App Footer"
  )
  app$stop()
})
