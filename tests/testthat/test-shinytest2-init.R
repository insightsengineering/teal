testthat::test_that("e2e: teal app initializes with no errors", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()
  app$stop()
})

testthat::test_that("e2e: teal app initializes with sessionInfo modal", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  # Check if button exists.
  testthat::expect_equal(
    app$get_text("#teal-sessionInfo-button"),
    "Session Info"
  )

  app$click(selector = "#teal-sessionInfo-button")

  # Check header and title content.
  testthat::expect_equal(
    app$get_text("#shiny-modal > div > div > div.modal-header > h4"),
    "SessionInfo"
  )
  testthat::expect_equal(
    app$get_text("#teal-sessionInfo-copy_button1"),
    "Copy to Clipboard"
  )
  testthat::expect_equal(
    app$get_text("#shiny-modal > div > div > div.modal-body > div > button:nth-child(2)"),
    "Dismiss"
  )

  # Check session info output.
  session_info <- app$get_text("#teal-sessionInfo-verbatim_content")

  greplf <- function(text, content = session_info) {
    grepl(text, content, fixed = TRUE)
  }

  testthat::expect_true(greplf("R version"))
  testthat::expect_true(greplf("attached base packages:"))
  testthat::expect_true(greplf("loaded via a namespace (and not attached):"))

  testthat::expect_true(greplf("shiny"))
  testthat::expect_true(greplf("teal.slice"))
  testthat::expect_true(greplf("teal.reporter"))

  # Check footer buttons.
  testthat::expect_equal(
    app$get_text("#teal-sessionInfo-copy_button2"),
    "Copy to Clipboard"
  )
  testthat::expect_equal(
    app$get_text("#shiny-modal > div > div > div.modal-footer > button:nth-child(2)"),
    "Dismiss"
  )

  app$stop()
})





testthat::test_that("e2e: init creates UI containing specified title, favicon, header and footer", {
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
  app$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_equal(
    app$get_text("head > title")[1],
    app_title
  )
  testthat::expect_equal(
    app$get_html("head > link[rel='icon']") %>%
      rvest::read_html() %>%
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
