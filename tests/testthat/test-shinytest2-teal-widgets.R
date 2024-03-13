testthat::test_that(
  "e2e: teal.widgets::verbatim_popup is initialized with a button that opens a modal with a verbatim text", {

  # App can not start if I put the content in variables.
  # verbatim_popup_ui(id, button_label = ui_popup_button_label)
  # verbatim_popup_srv(id, verbatim_content = verbatim_content_text, title = modal_title, style = TRUE)

  # {shiny}      R  stderr ----------- Warning: Error in <Anonymous>: object 'ui_popup_button_label' not found
  # {shiny}      R  stderr ----------- Warning: Error in <Anonymous>: object 'modal_title' not found
  module_label <- "Module with verbatim popup"
  ui_popup_button_label <- "Open popup"
  modal_title <- "My custom title"
  verbatim_content_text <- "if (TRUE) { print('Popups are the best') }"

  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      module(
        label = module_label,
        ui = function(id) {
          verbatim_popup_ui(id, button_label = "Open popup")
        },
        server = function(id) {
          verbatim_popup_srv(
            id,
            verbatim_content = "if (TRUE) { print('Popups are the best') }",
            title = "My custom title",
            style = FALSE
          )

        }
      )
    )
  )
  sprintf2 <- function(x, y = app$active_module_ns()){
    sprintf("#%s-%s", y, x)
  }

  app$wait_for_idle(timeout = default_idle_timeout)

  # Make sure module with a button was created.
  testthat::expect_equal(
    app$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    module_label
  )
  popup_button_element <- sprintf2("button")
  testthat::expect_equal(
    app$get_text(popup_button_element),
    ui_popup_button_label
  )

  # Click the button.
  app$click(selector = popup_button_element)

  # Verify the content of the popped modal is as expected.
  testthat::expect_equal(
    app$get_text(".modal-title"),
    modal_title
  )
  testthat::expect_equal(
    app$get_text(sprintf2("copy_button1")),
    "Copy to Clipboard"
  )
  testthat::expect_equal(
    app$get_text("#shiny-modal > div > div > div.modal-body > div > button:nth-child(2)"),
    "Dismiss"
  )
  testthat::expect_equal(
    app$get_text(sprintf2("verbatim_content")),
    verbatim_content_text
  )

  # Modal is closed, once the button is clicked.
  app$click(selector = "#shiny-modal-wrapper button[data-dismiss='modal']")
  # So far there are two Dismiss buttons, but will open an issue to fix this.
  # https://github.com/insightsengineering/teal.widgets/issues/233
  app$wait_for_idle(timeout = default_idle_timeout)
  testthat::expect_null(app$get_html("#shiny-modal-wrapper"))

  app$stop()
})
