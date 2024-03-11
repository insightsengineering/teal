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

  app$wait_for_idle(timeout = default_idle_timeout)
  testthat::expect_equal(
    app$get_text("#landingpopup b"),
    "A welcome message!"
  )
  app$stop()
})

test_that("e2e: app with default landing_popup_module creates modal containing a button", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(),
      example_module()
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_equal(
    app$get_text("#shiny-modal-wrapper .btn-default"),
    "Accept"
  )

  app$stop()
})

test_that("e2e: when default landing_popup_module is closed, it shows the underlying teal app", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(),
      example_module()
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  app$click(selector = "#shiny-modal-wrapper button[data-dismiss='modal']")

  # QUESTION: Is there a better way of checking the name of current module?
  expect_equal(
    app$get_html("#teal-main_ui-root-active_tab") %>%
      rvest::read_html() %>%
      rvest::html_node("a") %>%
      rvest::html_attr("data-value"),
    "example_teal_module"
  )

  app$stop()
})


# customized landing_popup_module ---------------------------------------------------------------------------------


extract_onclick <- function(id) {
  app$get_html(id) %>%
    rvest::read_html() %>%
    rvest::html_nodes("button") %>%
    rvest::html_attr("onclick")
}
phash <- function(text) paste0("#", text)


test_that("e2e: app with customized landing_popup_module creates modal containing specified title, content and buttons", {
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

  expect_equal(
    app$get_text(".modal-title"),
    modal_title
  )

  expect_equal(
    trimws(app$get_text(".modal-body")),
    modal_content_message
  )

  expect_equal(
    app$get_text(".btn-default:nth-child(1)"),
    modal_btns[[1]]$text
  )

  expect_equal(
    app$get_text(phash(modal_btns[[2]]$id)),
    modal_btns[[2]]$text
  )

  expect_equal(
    extract_onclick(phash(modal_btns[[2]]$id)),
    modal_btns[[2]]$onclick
  )

  expect_equal(
    app$get_text(phash(modal_btns[[3]]$id)),
    modal_btns[[3]]$text
  )

  expect_equal(
    extract_onclick(phash(modal_btns[[3]]$id)),
    modal_btns[[3]]$onclick
  )

  app$stop()
})

test_that("e2e: when customized button in landing_popup_module is clicked, it redirects to a certain page", {
  skip("It actually did not clicked the button.")
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(
        buttons = actionButton("read", "Read more", onclick = "window.open('http://google.com', '_blank')")
      ),
      example_module()
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  app$click(selector = "#read")
  # app$get_screenshot() still shows the app... :/

  expect_equal(
    app$get_url(),
    "https://www.google.com/"
  )

  app$stop()
})
