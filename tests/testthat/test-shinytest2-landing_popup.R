testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: teal app with landing_popup_module initializes with no errors", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module()
    ),
    landing_popup_args = list(
      title = "Welcome",
      content = tags$b("A welcome message!", style = "color: red;")
    )
  )

  testthat::expect_equal(
    app$get_text("#landingpopup b"),
    "A welcome message!"
  )
  app$stop()
})

testthat::test_that("e2e: app with default landing_popup_module creates modal containing a button", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module()
    ),
    landing_popup_args = list()
  )

  testthat::expect_equal(
    app$get_text("#shiny-modal-wrapper button"),
    "Accept"
  )

  app$stop()
})

testthat::test_that("e2e: when default landing_popup_module is closed, it shows the underlying teal app", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module()
    ),
    landing_popup_args = list()
  )

  # Button is clicked.
  app$click(selector = "#shiny-modal-wrapper button[data-dismiss='modal']")

  # There is no more modal displayed.
  testthat::expect_null(app$get_html("#shiny-modal-wrapper"))

  app$stop()
})


# customized landing_popup_module ---------------------------------------------------------------------------------

testthat::test_that(
  "e2e: app with customized landing_popup_module creates modal containing specified title, content and buttons",
  {
    skip_if_too_deep(5)

    app <- TealAppDriver$new(
      data = simple_teal_data(),
      modules = modules(
        example_module()
      ),
      landing_popup_args = list(
        title = "Custom Landing Popup Module Title",
        content = tags$b("A welcome message!", style = "color: red;"),
        footer = tagList(
          shiny::modalButton("Proceed"),
          shiny::actionButton(
            "read",
            label = "Read more",
            onclick = "window.open('http://google.com', '_blank')"
          ),
          shiny::actionButton(
            "close",
            label = "Reject",
            onclick = "window.close()"
          )
        )
      )
    )

    testthat::expect_equal(
      app$get_text(".modal-title"),
      "Custom Landing Popup Module Title"
    )

    testthat::expect_equal(
      trimws(app$get_text(".modal-body")),
      "A welcome message!"
    )

    testthat::expect_equal(
      app$get_text(".btn-default:nth-child(1)"),
      "Proceed"
    )

    testthat::expect_equal(
      app$get_text("#read"),
      "Read more"
    )

    testthat::expect_equal(
      app$get_attr("#read", "onclick"),
      "window.open('http://google.com', '_blank')"
    )

    testthat::expect_equal(
      app$get_text("#close"),
      "Reject"
    )

    testthat::expect_equal(
      app$get_attr("#close", "onclick"),
      "window.close()"
    )

    app$stop()
  }
)

testthat::test_that("e2e: when customized button in landing_popup_module is clicked, it redirects to a certain page", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module()
    ),
    landing_popup_args = list(
      footer = actionButton("read", "Read more", onclick = "window.open('http://google.com', '_blank')")
    )
  )

  testthat::expect_equal(
    app$get_attr("#read", "onclick"),
    "window.open('http://google.com', '_blank')"
  )

  app$stop()
})
