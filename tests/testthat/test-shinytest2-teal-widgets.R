testthat::test_that(
  "e2e: teal.widgets::verbatim_popup is initialized with a button that opens a modal with a verbatim text",
  {
    skip_if_too_deep(5)
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
            teal.widgets::verbatim_popup_ui(id, button_label = "Open popup")
          },
          server = eval(
            substitute(
              function(id) {
                teal.widgets::verbatim_popup_srv(
                  id,
                  verbatim_content = verbatim_content_text,
                  title = "modal_title",
                  style = FALSE
                )
              },
              env = list(verbatim_content_text = verbatim_content_text)
            )
          )
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
    app$wait_for_idle(timeout = default_idle_timeout)

    # Verify the content of the popped modal is as expected.
    testthat::expect_equal(
      app$get_text(".modal-title"),
      modal_title
    )

    testthat::expect_equal(
      app$active_module_element_text("copy_button1"),
      "Copy to Clipboard"
    )
    testthat::expect_equal(
      app$get_text("#shiny-modal > div > div > div.modal-body > div > button:nth-child(2)"),
      "Dismiss"
    )
    testthat::expect_equal(
      app$active_module_element_text("verbatim_content"),
      verbatim_content_text
    )

    # Modal is closed, once the button is clicked.
    app$click(selector = "#shiny-modal-wrapper button[data-dismiss='modal']")
    # There are two Dismiss buttons.
    app$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_null(app$get_html("#shiny-modal-wrapper"))

    app$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::table_with_settings is initialized with two buttons and a table",
  {
    skip_if_too_deep(5)
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

    app$wait_for_idle(timeout = default_idle_timeout)

    # Check if there are two buttons above the table.
    table_buttons_selector <- app$active_module_element("table-with-settings > div.table-settings-buttons")
    table_buttons <-
      app$get_html_rvest(table_buttons_selector) %>%
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
    testthat::expect_equal(
      app$get_html_rvest(dwnl_button) %>%
        rvest::html_element("i") %>%
        rvest::html_attr("class"),
      "fas fa-download"
    )

    # Click the first TABLE button.
    app$click(selector = dwnl_button)
    app$wait_for_idle(timeout = default_idle_timeout)

    # Review the content of the toggle.
    testthat::expect_equal(
      app$active_module_element_text("downbutton-file_format-label"),
      "File type"
    )

    file_format_text <- app$active_module_element_text("downbutton-file_format > div")
    testthat::expect_match(file_format_text, "formatted txt\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "csv\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "pdf\n", fixed = TRUE)

    testthat::expect_equal(
      app$active_module_element_text("downbutton-file_name-label"),
      "File name (without extension)"
    )

    # QUESTION - NEED HELP
    # How to get
    # value="table_20240321_142134"
    # out of
    # <input id="teal-main_ui-root-module_with_table_with_settings-module-downbutton-file_name" type="text" class="shiny-input-text form-control shinyjs-resettable shiny-bound-input" value="table_20240321_142134" data-shinyjs-resettable-id="teal-main_ui-root-module_with_table_with_settings-module-downbutton-file_name" data-shinyjs-resettable-type="Text" data-shinyjs-resettable-value="table_20240321_142134">
    # for
    # app$get_html_rvest(app$active_module_element("downbutton-file_name"))

    # TODO - NEED HELP
    # do not use div:nth-child(3)
    pagination <- "downbutton-dwnl > li > div > div:nth-child(3) > div.paginate-ui  > div.form-group.shiny-input-container"
    pagination_class <- gsub("#", "#dropdown-menu-", app$active_module_element(pagination))
    pagination_text <- app$get_text(pagination_class)
    testthat::expect_match(pagination_text, "Paginate table:\n", fixed = TRUE)
    testthat::expect_match(pagination_text, "lines / page\n", fixed = TRUE)

    download_button <- app$get_html_rvest(app$active_module_element("downbutton-data_download > i"))
    testthat::expect_equal(
      download_button %>%
        html_node("i") %>%
        html_attr("class"),
      "fas fa-download"
    )
    testthat::expect_equal(
      download_button %>%
        html_node("i") %>%
        html_attr("aria-label"),
      "download icon"
    )

    # TODO - NEED HELP
    # click second radio button
    # app$click(selector = "input[value~=csv]")
    # check that pagination is missing

    # Click the second TABLE button.
    app$click(selector = app$active_module_element("expand"))
    app$wait_for_idle(timeout = default_idle_timeout)
    # Review the table modal content.

    table_content <- app$active_module_element_text("table_out_modal")

    check_table <- function(content) {
      testthat::expect_match(content, "A: Drug X", fixed = TRUE)
      testthat::expect_match(content, "C: Combination", fixed = TRUE)
      testthat::expect_match(content, "UNDIFFERENTIATED", fixed = TRUE)
      testthat::expect_match(content, "SEX", fixed = TRUE)
      testthat::expect_match(content, "34.91", fixed = TRUE)
      testthat::expect_match(content, "33.02", fixed = TRUE)
    }
    check_table(table_content)

    # Close modal.
    app$click(selector = "#shiny-modal-wrapper .modal-footer > button")
    app$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_null(app$get_html(app$active_module_element("table_out_modal")))

    # Review the main table content.
    main_table_content <- app$active_module_element_text("table_out_main")
    check_table(main_table_content)

    app$stop()
  }
)
