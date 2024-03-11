testthat::test_that("e2e: teal app initializes with no errors", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module")
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()
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
    app$get_html("head > title")[1] %>%
      rvest::read_html() %>%
      rvest::html_text(),
    app_title
  )
  testthat::expect_equal(
    app$get_html("head > link[rel='icon']") %>%
      rvest::read_html() %>%
      rvest::html_elements("link") %>%
      rvest::html_attr("href"),
    app_favicon
  )
  testthat::expect_true(
    grepl(
      app_header,
      app$get_html("header") %>%
        rvest::read_html() %>%
        rvest::html_text()
    )
  )
  testthat::expect_true(
    grepl(
      app_footer,
      app$get_html("footer") %>%
        rvest::read_html() %>%
        rvest::html_text()
    )
  )
  app$stop()
})
