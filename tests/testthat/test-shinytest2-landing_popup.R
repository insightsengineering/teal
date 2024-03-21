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

testthat::test_that("e2e: app with default landing_popup_module creates modal containing a button", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(),
      example_module()
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_equal(
    app$get_text("#shiny-modal-wrapper button"),
    "Accept"
  )

  app$stop()
})

testthat::test_that("e2e: when default landing_popup_module is closed, it shows the underlying teal app", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(),
      example_module()
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  # Button is clicked.
  app$click(selector = "#shiny-modal-wrapper button[data-dismiss='modal']")
  app$wait_for_idle(timeout = default_idle_timeout)

  # There is no more modal displayed.
  testthat::expect_null(app$get_html("#shiny-modal-wrapper"))

  app$stop()
})


# customized landing_popup_module ---------------------------------------------------------------------------------

testthat::test_that(
  "e2e: app with customized landing_popup_module creates modal containing specified title, content and buttons",
  {
    phash <- function(text) paste0("#", text)

    modal_title <- "Custom Landing Popup Module Title"
    modal_content_message <- "A welcome message!"
    modal_content <- tags$b(modal_content_message, style = "color: red;")

    modal_btns <- list(
      go = list(text = "Proceed"),
      more = list(text = "Read more", onclick = "window.open('http://google.com', '_blank')", id = "read"),
      reject = list(text = "Reject", onclick = "window.close()", id = "close")
    )
    modal_buttons <-
      tagList(
        shiny::modalButton(modal_btns$go$text),
        shiny::actionButton(
          modal_btns$more$id,
          label = modal_btns$more$text,
          onclick = modal_btns$more$onclick
        ),
        shiny::actionButton(
          modal_btns$reject$id,
          label = modal_btns$reject$text,
          onclick = modal_btns$reject$onclick
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

    testthat::expect_equal(
      app$get_text(".modal-title"),
      modal_title
    )

    testthat::expect_equal(
      trimws(app$get_text(".modal-body")),
      modal_content_message
    )

    testthat::expect_equal(
      app$get_text(".btn-default:nth-child(1)"),
      modal_btns$go$text
    )

    testthat::expect_equal(
      app$get_text(phash(modal_btns$more$id)),
      modal_btns$more$text
    )

    testthat::expect_equal(
      app$get_onclick(phash(modal_btns$more$id)),
      modal_btns$more$onclick
    )

    testthat::expect_equal(
      app$get_text(phash(modal_btns$reject$id)),
      modal_btns$reject$text
    )

    testthat::expect_equal(
      app$get_onclick(phash(modal_btns$reject$id)),
      modal_btns$reject$onclick
    )

    app$stop()
  }
)

testthat::test_that("e2e: when customized button in landing_popup_module is clicked, it redirects to a certain page", {
  onclick_text <- "window.open('http://google.com', '_blank')"
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(
        buttons = actionButton("read", "Read more", onclick = onclick_text)
      ),
      example_module()
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_equal(
    app$get_attr("#read", "onclick"),
    onclick_text
  )

  app$stop()
})
