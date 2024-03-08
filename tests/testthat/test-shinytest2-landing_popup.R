testthat::test_that("e2e: teal app with landing_popup_module initializes with no errors", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(
        title = "Welcome",
        content = tags$b("A welcome message!", style = "color: red;")
      ),
      example_module()
    )
  )
  app$view()

  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()
  app$stop()
})

test_that("e2e: app with landing_popup_module creates modal containing specified title, content and button", {
  modal_title <- "Custom Landing Popup Module Title"
  modal_content_message <- "A welcome message!"
  modal_content <- tags$b(modal_content_message, style = "color: red;")

  modal_buttons_texts <- c("Proceed", "Read more", "Reject")
  modal_buttons <-
    tagList(
      modalButton(modal_buttons_texts[1]),
      actionButton("read", modal_buttons_texts[2], onclick = "window.open('http://google.com', '_blank')"),
      actionButton("close", modal_buttons_texts[3], onclick = "window.close()")
    )

  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(
        title = modal_title,
        content = modal_content,
        buttons = modal_buttons
      ),
      example_module()
    )
  )

  app$wait_for_idle(timeout = default_idle_timeout)

  expect_equal(
    app$get_html(".modal-title") %>%
      rvest::read_html() %>%
      rvest::html_text(),
    modal_title
  )
  expect_equal(
    app$get_html(".modal-body") %>%
      rvest::read_html() %>%
      rvest::html_text() %>%
      trimws(),
    modal_content_message
  )

  expect_equal(
    app$get_html(".btn-default:nth-child(1)") %>%
      rvest::read_html() %>%
      rvest::html_text(),
      modal_buttons_texts[1]
  )

  expect_equal(
    app$get_html("#read") %>%
      rvest::read_html() %>%
      rvest::html_text(),
    modal_buttons_texts[2]
  )

  expect_equal(
    app$get_html("#close") %>%
      rvest::read_html() %>%
      rvest::html_text(),
    modal_buttons_texts[3]
  )

  app$stop()
})
