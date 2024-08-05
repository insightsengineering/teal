testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: teal app initializes with no errors", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  app$expect_no_shiny_error()
  app$stop()
})

testthat::test_that("e2e: teal app initializes with sessionInfo modal", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )

  # Check if button exists.
  button_selector <- "#teal-sessionInfo-button"
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
        "#shiny-modal [id^='teal-sessionInfo-copy_button']"
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
  session_info <- app$get_text("#teal-sessionInfo-verbatim_content")

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
  app_title <- "Custom Teal App Title"
  app_favicon <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png"
  app_header <- "Custom Teal App Header"
  app_footer <- "Custom Teal App Footer"
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    title = build_app_title(
      app_title,
      app_favicon
    ),
    header = app_header,
    footer = app_footer
  )

  testthat::expect_equal(
    app$get_text("head > title")[1],
    app_title
  )
  testthat::expect_equal(
    app$get_html_rvest("head > link[rel='icon']") %>%
      rvest::html_elements("link") %>%
      rvest::html_attr("href"),
    app_favicon
  )
  testthat::expect_match(
    app$get_text("header"),
    app_header
  )
  testthat::expect_match(
    app$get_text("footer"),
    app_footer
  )
  app$stop()
})
