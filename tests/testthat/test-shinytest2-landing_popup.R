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

test_that("e2e: app with landing_popup_module creates modal containing specified title, content and buttons", {
  modal_title <- "Custom Landing Popup Module Title"
  modal_content_message <- "A welcome message!"
  modal_content <- tags$b(modal_content_message, style = "color: red;")

  modal_btns <- list(
    list(text = "Proceed"),
    list(text = "Read more", onclick = "window.open('http://google.com', '_blank')", id = "read"),
    list(text = "Reject", onclick = "window.close()", id = "close")
  )
  modal_buttons <-
    tagList(
      modalButton(modal_btns[[1]]$text),
      actionButton(
        modal_btns[[2]]$id,
        label = modal_btns[[2]]$text,
        onclick = modal_btns[[2]]$onclick
      ),
      actionButton(
        modal_btns[[3]]$id,
        label = modal_btns[[3]]$text,
        onclick = modal_btns[[3]]$onclick
      )
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

  extract_onclick <- function(id) {
    app$get_html(id) %>%
      rvest::read_html() %>%
      rvest::html_nodes("button") %>%
      rvest::html_attr("onclick")
  }
  extract_text <- function(id) {
    app$get_html(id) %>%
      rvest::read_html() %>%
      rvest::html_text() %>%
      trimws()
  }
  phash <- function(text) paste0("#", text)

  expect_equal(
    extract_text(".modal-title"),
    modal_title
  )

  expect_equal(
    extract_text(".modal-body"),
    modal_content_message
  )

  expect_equal(
    extract_text(".btn-default:nth-child(1)"),
    modal_btns[[1]]$text
  )

  expect_equal(
    extract_text(phash(modal_btns[[2]]$id)),
    modal_btns[[2]]$text
  )

  expect_equal(
    extract_onclick(phash(modal_btns[[2]]$id)),
    modal_btns[[2]]$onclick
  )

  expect_equal(
    extract_text(phash(modal_btns[[3]]$id)),
    modal_btns[[3]]$text
  )

  expect_equal(
    extract_onclick(phash(modal_btns[[3]]$id)),
    modal_btns[[3]]$onclick
  )

  app$stop()
})
