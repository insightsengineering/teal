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

  app$wait_for_idle(timeout = default_idle_timeout)

  # Make sure module with a button was created.
  testthat::expect_equal(
    app$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    module_label
  )
  popup_button_element <- app$active_module_element("button")
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
    app$active_module_element("copy_button1") %>%
    app$get_text(),
    "Copy to Clipboard"
  )
  testthat::expect_equal(
    app$get_text("#shiny-modal > div > div > div.modal-body > div > button:nth-child(2)"),
    "Dismiss"
  )
  testthat::expect_equal(
    app$active_module_element("verbatim_content") %>%
    app$get_text(),
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

testthat::test_that(
  "e2e: teal.widgets::table_with_settings is initialized with a button that opens a modal with a verbatim text", {

  # This should probably be moved to teal.widgets directly.
  testthat::skip_if_not_installed(c("rtables", "formatters"))

  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      module(
        label = "Module with table_with_settings",
        ui = function(id) {
          teal.widgets::table_with_settings_ui(id = id)
        },
        server <- function(id, input, output, session) {
          table_r <- reactive({
            layout <- rtables::basic_table() %>%
              rtables::split_cols_by("ARM") %>%
              rtables::analyze(c("SEX", "AGE"))

            rtables::build_table(layout, formatters::DM)
          })

          teal.widgets::table_with_settings_srv(id = id, table_r = table_r)
        }
      )
    )
  )
  # Check if there are two buttons above the table.
  table_buttons_selector <- app$active_module_element("table-with-settings > div.table-settings-buttons")
  table_buttons <-
    app$get_html(table_buttons_selector) %>%
    rvest::read_html() %>%
    rvest::html_elements("button")
  testthat::expect_length(table_buttons, 2)
  # Check is the first one is a toggle button.
  testthat::expect_equal(
    table_buttons[[1]] %>%
      html_attr("data-toggle"),
    "dropdown"
  )
  # First button has specific font-awesome icon.
  dwnl_button <- app$active_module_element("downbutton-dwnl")
  testhat::expect_equal(
    app$get_html(dwnl_button) %>%
      rvest::read_html() %>%
      rvest::html_element("i") %>%
      rvest::html_attr("class"),
    "fas fa-download"
  )

  # Click the first button.

  # Review the content of the toggle.

  # Click the second button.

  # Review the modal content.

  # Close modal.

  # Review the table content.

  app$stop()

})

