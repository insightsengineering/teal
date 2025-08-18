# FilteredData ------

#' Drive a `teal` application
#'
#' Extension of the `shinytest2::AppDriver` class with methods for
#' driving a teal application for performing interactions for `shinytest2` tests.
#'
#' @keywords internal
#'
TealAppDriver <- R6::R6Class( # nolint: object_name.
  "TealAppDriver",
  cloneable = FALSE,
  inherit = {
    lapply(c("testthat", "shinytest2", "rvest"), function(.x, use_testthat) {
      if (!requireNamespace(.x, quietly = TRUE)) {
        if (use_testthat) {
          testthat::skip(sprintf("%s is not installed", .x))
        } else {
          stop("Please install '", .x, "' package to use this class.", call. = FALSE)
        }
      }
    }, use_testthat = requireNamespace("testthat", quietly = TRUE) && testthat::is_testing())
    shinytest2::AppDriver
  },
  # public methods ----
  public = list(
    #' @description
    #' Initialize a `TealAppDriver` object for testing a `teal` application.
    #'
    #' @param data,modules,filter arguments passed to `init`
    #' @param title_args,header,footer,landing_popup_args to pass into the modifier functions.
    #' @param timeout (`numeric`) Default number of milliseconds for any timeout or
    #' timeout_ parameter in the `TealAppDriver` class.
    #' Defaults to 20s.
    #'
    #' See [`shinytest2::AppDriver`] `new` method for more details on how to change it
    #' via options or environment variables.
    #' @param load_timeout (`numeric`) How long to wait for the app to load, in ms.
    #' This includes the time to start R. Defaults to 100s.
    #'
    #' See [`shinytest2::AppDriver`] `new` method for more details on how to change it
    #' via options or environment variables
    #' @param ... Additional arguments to be passed to `shinytest2::AppDriver$new`
    #'
    #'
    #' @return  Object of class `TealAppDriver`
    initialize = function(data,
                          modules,
                          filter = teal_slices(),
                          title_args = list(),
                          header = tags$p(),
                          footer = tags$p(),
                          landing_popup_args = NULL,
                          timeout = rlang::missing_arg(),
                          load_timeout = rlang::missing_arg(),
                          ...) {
      private$data <- data
      private$modules <- modules
      private$filter <- filter

      new_title <- modifyList(
        list(
          title = "Custom Teal App Title",
          favicon = .teal_favicon
        ),
        title_args
      )
      app <- init(
        data = data,
        modules = modules,
        filter = filter
      ) |>
        modify_title(title = new_title$title, favicon = new_title$favicon) |>
        modify_header(header) |>
        modify_footer(footer)

      if (!is.null(landing_popup_args)) {
        default_args <- list(
          title = NULL,
          content = NULL,
          footer = modalButton("Accept")
        )
        landing_popup_args[names(default_args)] <- Map(
          function(x, y) if (is.null(y)) x else y,
          default_args,
          landing_popup_args[names(default_args)]
        )
        app <- add_landing_modal(
          app,
          title = landing_popup_args$title,
          content = landing_popup_args$content,
          footer = landing_popup_args$footer
        )
      }

      # Default timeout is hardcoded to 4s in shinytest2:::resolve_timeout
      # It must be set as parameter to the AppDriver
      suppressWarnings(
        super$initialize(
          app_dir = shinyApp(app$ui, app$server),
          name = "teal",
          variant = shinytest2::platform_variant(),
          timeout = rlang::maybe_missing(timeout, 20 * 1000),
          load_timeout = rlang::maybe_missing(load_timeout, 100 * 1000),
          ...
        )
      )

      # Check for minimum version of Chrome that supports the tests
      #  - Element.checkVisibility was added on 105
      chrome_version <- numeric_version(
        gsub(
          "[[:alnum:]_]+/", # Prefix that ends with forward slash
          "",
          self$get_chromote_session()$Browser$getVersion()$product
        ),
        strict = FALSE
      )

      required_version <- "121"

      testthat::skip_if(
        is.na(chrome_version),
        "Problem getting Chrome version, please contact the developers."
      )
      testthat::skip_if(
        chrome_version < required_version,
        sprintf(
          "Chrome version '%s' is not supported, please upgrade to '%s' or higher",
          chrome_version,
          required_version
        )
      )
      # end od check

      private$set_active_ns()
      self$wait_for_idle()
    },
    #' @description
    #' Append parent [`shinytest2::AppDriver`] `click` method with a call to `waif_for_idle()` method.
    #' @param ... arguments passed to parent [`shinytest2::AppDriver`] `click()` method.
    click = function(...) {
      super$click(...)
      private$wait_for_page_stability()
    },
    #' @description
    #' Check if the app has shiny errors. This checks for global shiny errors.
    #' Note that any shiny errors dependent on shiny server render will only be captured after the teal module tab
    #' is visited because shiny will not trigger server computations when the tab is invisible.
    #' So, navigate to the module tab you want to test before calling this function.
    #' Although, this catches errors hidden in the other module tabs if they are already rendered.
    expect_no_shiny_error = function() {
      testthat::expect_null(
        self$get_html(".shiny-output-error:not(.shiny-output-error-validation)"),
        info = "Shiny error is observed"
      )
    },
    #' @description
    #' Check if the app has no validation errors. This checks for global shiny validation errors.
    expect_no_validation_error = function() {
      testthat::expect_null(
        self$get_html(".shiny-output-error-validation"),
        info = "No validation error is observed"
      )
    },
    #' @description
    #' Check if the app has validation errors. This checks for global shiny validation errors.
    expect_validation_error = function() {
      testthat::expect_false(
        is.null(self$get_html(".shiny-output-error-validation")),
        info = "Validation error is not observed"
      )
    },
    #' @description
    #' Set the input in the `teal` app.
    #'
    #' @param input_id (character) The shiny input id with it's complete name space.
    #' @param value The value to set the input to.
    #' @param ... Additional arguments to be passed to `shinytest2::AppDriver$set_inputs`
    #'
    #' @return The `TealAppDriver` object invisibly.
    set_input = function(input_id, value, ...) {
      do.call(
        self$set_inputs,
        c(setNames(list(value), input_id), list(...))
      )
      invisible(self)
    },
    #' @description
    #' Navigate the teal tabs in the `teal` app.
    #'
    #' @param tab (character) Labels of tabs to navigate to.
    #' Note: Make sure to provide unique labels for the tabs.
    #'
    #' @return The `TealAppDriver` object invisibly.
    navigate_teal_tab = function(tab) {
      checkmate::check_string(tab)
      self$run_js(
        sprintf(
          "$('.dropdown-menu a:contains(\"%s\")').click()",
          tab
        )
      )
      self$wait_for_idle()
      private$set_active_ns()
      invisible(self)
    },
    #' @description
    #' Get the active shiny name space for different components of the teal app.
    #'
    #' @return (`list`) The list of active shiny name space of the teal components.
    active_ns = function() {
      if (identical(private$ns$module, character(0))) {
        private$set_active_ns()
      }
      private$ns
    },
    #' @description
    #' Get the active shiny name space for interacting with the module content.
    #'
    #' @return (`string`) The active shiny name space of the component.
    active_module_ns = function() {
      if (identical(private$ns$module, character(0))) {
        private$set_active_ns()
      }
      private$ns$module
    },
    #' @description
    #' Get the active shiny name space bound with a custom `element` name.
    #'
    #' @param element `character(1)` custom element name.
    #'
    #' @return (`string`) The active shiny name space of the component bound with the input `element`.
    active_module_element = function(element) {
      checkmate::assert_string(element)
      sprintf("#%s-%s", self$active_module_ns(), element)
    },
    #' @description
    #' Get the text of the active shiny name space bound with a custom `element` name.
    #'
    #' @param element `character(1)` the text of the custom element name.
    #'
    #' @return (`string`) The text of the active shiny name space of the component bound with the input `element`.
    active_module_element_text = function(element) {
      checkmate::assert_string(element)
      self$get_text(self$active_module_element(element))
    },
    #' @description
    #' Get the active shiny name space for interacting with the filter panel.
    #'
    #' @return (`string`) The active shiny name space of the component.
    active_filters_ns = function() {
      if (identical(private$ns$filter_panel, character(0))) {
        private$set_active_ns()
      }
      private$ns$filter_panel
    },
    #' @description
    #' Get the active shiny name space for interacting with the data-summary panel.
    #'
    #' @return (`string`) The active shiny name space of the data-summary component.
    active_data_summary_ns = function() {
      if (identical(private$ns$data_summary, character(0))) {
        private$set_active_ns()
      }
      private$ns$data_summary
    },
    #' @description
    #' Get the active shiny name space bound with a custom `element` name.
    #'
    #' @param element `character(1)` custom element name.
    #'
    #' @return (`string`) The active shiny name space of the component bound with the input `element`.
    active_data_summary_element = function(element) {
      checkmate::assert_string(element)
      sprintf("#%s-%s", self$active_data_summary_ns(), element)
    },
    #' @description
    #' Get the input from the module in the `teal` app.
    #' This function will only access inputs from the name space of the current active teal module.
    #'
    #' @param input_id (character) The shiny input id to get the value from.
    #'
    #' @return The value of the shiny input.
    get_active_module_input = function(input_id) {
      checkmate::check_string(input_id)
      self$get_value(input = sprintf("%s-%s", self$active_module_ns(), input_id))
    },
    #' @description
    #' Get the output from the module in the `teal` app.
    #' This function will only access outputs from the name space of the current active teal module.
    #'
    #' @param output_id (character) The shiny output id to get the value from.
    #'
    #' @return The value of the shiny output.
    get_active_module_output = function(output_id) {
      checkmate::check_string(output_id)
      self$get_value(output = sprintf("%s-%s", self$active_module_ns(), output_id))
    },
    #' @description
    #' Get the output from the module's `teal.widgets::table_with_settings` or `DT::DTOutput` in the `teal` app.
    #' This function will only access outputs from the name space of the current active teal module.
    #'
    #' @param table_id (`character(1)`) The id of the table in the active teal module's name space.
    #' @param which (integer) If there is more than one table, which should be extracted.
    #' By default it will look for  a table that is built using `teal.widgets::table_with_settings`.
    #'
    #' @return The data.frame with table contents.
    get_active_module_table_output = function(table_id, which = 1) {
      checkmate::check_number(which, lower = 1)
      checkmate::check_string(table_id)
      table <- rvest::html_table(
        self$get_html_rvest(self$active_module_element(table_id)),
        fill = TRUE
      )
      if (length(table) == 0) {
        data.frame()
      } else {
        table[[which]]
      }
    },
    #' @description
    #' Get the output from the module's `teal.widgets::plot_with_settings` in the `teal` app.
    #' This function will only access plots from the name space of the current active teal module.
    #'
    #' @param plot_id (`character(1)`) The id of the plot in the active teal module's name space.
    #'
    #' @return The `src` attribute as `character(1)` vector.
    get_active_module_plot_output = function(plot_id) {
      checkmate::check_string(plot_id)
      self$get_attr(
        self$active_module_element(sprintf("%s-plot_main > img", plot_id)),
        "src"
      )
    },
    #' @description
    #' Set the input in the module in the `teal` app.
    #' This function will only set inputs in the name space of the current active teal module.
    #'
    #' @param input_id (character) The shiny input id to get the value from.
    #' @param value The value to set the input to.
    #' @param ... Additional arguments to be passed to `shinytest2::AppDriver$set_inputs`
    #'
    #' @return The `TealAppDriver` object invisibly.
    set_active_module_input = function(input_id, value, ...) {
      checkmate::check_string(input_id)
      checkmate::check_string(value)
      self$set_input(
        sprintf("%s-%s", self$active_module_ns(), input_id),
        value,
        ...
      )
      dots <- rlang::list2(...)
      if (!isFALSE(dots[["wait"]])) self$wait_for_idle() # Default behavior is to wait
      invisible(self)
    },
    #' @description
    #' Get the active datasets that can be accessed via the filter panel of the current active teal module.
    get_active_filter_vars = function() {
      displayed_datasets_index <- self$is_visible(
        sprintf("#%s-filters-filter_active_vars_contents > div > span", self$active_filters_ns())
      )

      js_code <- sprintf(
        "
          const accordionTitles = document.querySelectorAll(
            '#%s-filters-filter_active_vars_contents .accordion-title'
          );
          let textContents = [];

          accordionTitles.forEach(accordionTitle => {
              let textNode = accordionTitle.childNodes[0];
              textContents.push(textNode.textContent);
          });
          textContents;
        ",
        self$active_filters_ns()
      )
      available_datasets <- unlist(self$get_js(js_code))

      available_datasets[displayed_datasets_index]
    },
    #' @description
    #' Get the active data summary table
    #' @return `data.frame`
    get_active_data_summary_table = function() {
      summary_table <- rvest::html_table(
        self$get_html_rvest(self$active_data_summary_element("table")),
        fill = TRUE
      )[[1]]

      col_names <- unlist(summary_table[1, ], use.names = FALSE)
      summary_table <- summary_table[-1, ]
      colnames(summary_table) <- col_names
      if (nrow(summary_table) > 0) {
        summary_table
      } else {
        NULL
      }
    },
    #' @description
    #' Test if `DOM` elements are visible on the page with a JavaScript call.
    #' @param selector (`character(1)`) `CSS` selector to check visibility.
    #' A `CSS` id will return only one element if the UI is well formed.
    #' @param content_visibility_auto,opacity_property,visibility_property (`logical(1)`) See more information
    #' on <https://developer.mozilla.org/en-US/docs/Web/API/Element/checkVisibility>.
    #'
    #' @return Logical vector with all occurrences of the selector.
    is_visible = function(selector,
                          content_visibility_auto = FALSE,
                          opacity_property = FALSE,
                          visibility_property = FALSE) {
      checkmate::assert_string(selector)
      checkmate::assert_flag(content_visibility_auto)
      checkmate::assert_flag(opacity_property)
      checkmate::assert_flag(visibility_property)

      private$wait_for_page_stability()

      testthat::skip_if_not(
        self$get_js("typeof Element.prototype.checkVisibility === 'function'"),
        "Element.prototype.checkVisibility is not supported in the current browser."
      )

      unlist(
        self$get_js(
          sprintf(
            "Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility({%s, %s, %s}))",
            selector,
            # Extra parameters
            sprintf("contentVisibilityAuto: %s", tolower(content_visibility_auto)),
            sprintf("opacityProperty: %s", tolower(opacity_property)),
            sprintf("visibilityProperty: %s", tolower(visibility_property))
          )
        )
      )
    },
    #' @description
    #' Get the active filter variables from a dataset in the `teal` app.
    #'
    #' @param dataset_name (character) The name of the dataset to get the filter variables from.
    #' If `NULL`, the filter variables for all the datasets will be returned in a list.
    get_active_data_filters = function(dataset_name = NULL) {
      checkmate::check_string(dataset_name, null.ok = TRUE)
      datasets <- self$get_active_filter_vars()
      checkmate::assert_subset(dataset_name, datasets)
      active_filters <- lapply(
        datasets,
        function(x) {
          var_names <- gsub(
            pattern = "\\s",
            replacement = "",
            self$get_text(
              sprintf(
                "#%s-filters-%s-container .filter-card-varname",
                self$active_filters_ns(),
                x
              )
            )
          )
          structure(
            lapply(var_names, private$get_active_filter_selection, dataset_name = x),
            names = var_names
          )
        }
      )
      names(active_filters) <- datasets
      if (is.null(dataset_name)) {
        return(active_filters)
      }
      active_filters[[dataset_name]]
    },
    #' @description
    #' Add a new variable from the dataset to be filtered.
    #'
    #' @param dataset_name (character) The name of the dataset to add the filter variable to.
    #' @param var_name (character) The name of the variable to add to the filter panel.
    #' @param ... Additional arguments to be passed to `shinytest2::AppDriver$set_inputs`
    #'
    #' @return The `TealAppDriver` object invisibly.
    add_filter_var = function(dataset_name, var_name, ...) {
      checkmate::check_string(dataset_name)
      checkmate::check_string(var_name)
      private$set_active_ns()
      self$click(
        selector = sprintf(
          "#%s-filters-%s-add_filter_icon",
          private$ns$filter_panel,
          dataset_name
        )
      )
      self$set_input(
        sprintf(
          "%s-filters-%s-%s-filter-var_to_add",
          private$ns$filter_panel,
          dataset_name,
          dataset_name
        ),
        var_name,
        ...
      )
      invisible(self)
    },
    #' @description
    #' Remove an active filter variable of a dataset from the active filter variables panel.
    #'
    #' @param dataset_name (character) The name of the dataset to remove the filter variable from.
    #' If `NULL`, all the filter variables will be removed.
    #' @param var_name (character) The name of the variable to remove from the filter panel.
    #' If `NULL`, all the filter variables of the dataset will be removed.
    #'
    #' @return The `TealAppDriver` object invisibly.
    remove_filter_var = function(dataset_name = NULL, var_name = NULL) {
      checkmate::check_string(dataset_name, null.ok = TRUE)
      checkmate::check_string(var_name, null.ok = TRUE)
      if (is.null(dataset_name)) {
        remove_selector <- sprintf(
          "#%s-active-remove_all_filters",
          self$active_filters_ns()
        )
      } else if (is.null(var_name)) {
        remove_selector <- sprintf(
          "#%s-active-%s-remove_filters",
          self$active_filters_ns(),
          dataset_name
        )
      } else {
        remove_selector <- sprintf(
          "#%s-active-%s-filter-%s_%s-remove",
          self$active_filters_ns(),
          dataset_name,
          dataset_name,
          var_name
        )
      }
      self$click(
        selector = remove_selector
      )
      invisible(self)
    },
    #' @description
    #' Set the active filter values for a variable of a dataset in the active filter variable panel.
    #'
    #' @param dataset_name (character) The name of the dataset to set the filter value for.
    #' @param var_name (character) The name of the variable to set the filter value for.
    #' @param input The value to set the filter to.
    #' @param ... Additional arguments to be passed to `shinytest2::AppDriver$set_inputs`
    #'
    #' @return The `TealAppDriver` object invisibly.
    set_active_filter_selection = function(dataset_name,
                                           var_name,
                                           input,
                                           ...) {
      checkmate::check_string(dataset_name)
      checkmate::check_string(var_name)
      checkmate::check_string(input)

      input_id_prefix <- sprintf(
        "%s-filters-%s-filter-%s_%s-inputs",
        self$active_filters_ns(),
        dataset_name,
        dataset_name,
        var_name
      )

      # Find the type of filter (based on filter panel)
      supported_suffix <- c("selection", "selection_manual")
      slices_suffix <- supported_suffix[
        match(
          TRUE,
          vapply(
            supported_suffix,
            function(suffix) {
              !is.null(self$get_html(sprintf("#%s-%s", input_id_prefix, suffix)))
            },
            logical(1)
          )
        )
      ]

      # Generate correct namespace
      slices_input_id <- sprintf(
        "%s-filters-%s-filter-%s_%s-inputs-%s",
        self$active_filters_ns(),
        dataset_name,
        dataset_name,
        var_name,
        slices_suffix
      )

      if (identical(slices_suffix, "selection_manual")) {
        checkmate::assert_numeric(input, len = 2)

        dots <- rlang::list2(...)
        checkmate::assert_choice(dots$priority_, formals(self$set_inputs)[["priority_"]], null.ok = TRUE)
        checkmate::assert_flag(dots$wait_, null.ok = TRUE)

        self$run_js(
          sprintf(
            "Shiny.setInputValue('%s:sw.numericRange', [%f, %f], {priority: '%s'})",
            slices_input_id,
            input[[1]],
            input[[2]],
            priority_ = ifelse(is.null(dots$priority_), "input", dots$priority_)
          )
        )

        if (isTRUE(dots$wait_) || is.null(dots$wait_)) {
          self$wait_for_idle(
            timeout = if (is.null(dots$timeout_)) rlang::missing_arg() else dots$timeout_
          )
        }
      } else if (identical(slices_suffix, "selection")) {
        self$set_input(
          slices_input_id,
          input,
          ...
        )
      } else {
        stop("Filter selection set not supported for this slice.")
      }

      invisible(self)
    },
    #' @description
    #' Extract `html` attribute (found by a `selector`).
    #'
    #' @param selector (`character(1)`) specifying the selector to be used to get the content of a specific node.
    #' @param attribute (`character(1)`) name of an attribute to retrieve from a node specified by `selector`.
    #'
    #' @return The `character` vector.
    get_attr = function(selector, attribute) {
      rvest::html_attr(
        rvest::html_nodes(self$get_html_rvest("html"), selector),
        attribute
      )
    },
    #' @description
    #' Wrapper around `get_html` that passes the output directly to `rvest::read_html`.
    #'
    #' @param selector `(character(1))` passed to `get_html`.
    #'
    #' @return An XML document.
    get_html_rvest = function(selector) {
      rvest::read_html(self$get_html(selector))
    },
    #' Wrapper around `get_url()` method that opens the app in the browser.
    #'
    #' @return Nothing. Opens the underlying teal app in the browser.
    open_url = function() {
      browseURL(self$get_url())
    },
    #' @description
    #' Waits until a specified input, output, or export value.
    #' This function serves as a wrapper around the `wait_for_value` method,
    #' providing a more flexible interface for waiting on different types of values within the active module namespace.
    #' @param input,output,export A name of an input, output, or export value.
    #' Only one of these parameters may be used.
    #' @param ... Must be empty. Allows for parameter expansion.
    #' Parameter with additional value to passed in `wait_for_value`.
    wait_for_active_module_value = function(input = rlang::missing_arg(),
                                            output = rlang::missing_arg(),
                                            export = rlang::missing_arg(),
                                            ...) {
      ns <- shiny::NS(self$active_module_ns())

      if (!rlang::is_missing(input) && checkmate::test_string(input, min.chars = 1)) input <- ns(input)
      if (!rlang::is_missing(output) && checkmate::test_string(output, min.chars = 1)) output <- ns(output)
      if (!rlang::is_missing(export) && checkmate::test_string(export, min.chars = 1)) export <- ns(export)

      self$wait_for_value(
        input = input,
        output = output,
        export = export,
        ...
      )
    }
  ),
  # private members ----
  private = list(
    # private attributes ----
    data = NULL,
    modules = NULL,
    filter = teal_slices(),
    ns = list(
      module = character(0),
      filter_panel = character(0)
    ),
    # private methods ----
    set_active_ns = function() {
      all_inputs <- self$get_values()$input
      active_tab_inputs <- all_inputs[grepl("-active_module_id$", names(all_inputs))]

      active_wrapper_id <- sub(
        "^#",
        "",
        self$get_attr(
          selector = sprintf(".teal-modules-tree li a.module-button[data-value='%s']", active_tab_inputs),
          attribute = "href"
        )
      )
      active_base_id <- sub("-wrapper$", "", active_wrapper_id)

      private$ns$module <- shiny::NS(active_base_id, "module")
      private$ns$filter_panel <- shiny::NS(active_base_id, "filter_panel")
      private$ns$data_summary <- shiny::NS(active_base_id, "data_summary")
    },
    # @description
    # Get the active filter values from the active filter selection of dataset from the filter panel.
    #
    # @param dataset_name (character) The name of the dataset to get the filter values from.
    # @param var_name (character) The name of the variable to get the filter values from.
    #
    # @return The value of the active filter selection.
    get_active_filter_selection = function(dataset_name, var_name) {
      checkmate::check_string(dataset_name)
      checkmate::check_string(var_name)
      input_id_prefix <- sprintf(
        "%s-filters-%s-filter-%s_%s-inputs",
        self$active_filters_ns(),
        dataset_name,
        dataset_name,
        var_name
      )

      # Find the type of filter (categorical or range)
      supported_suffix <- c("selection", "selection_manual")
      for (suffix in supported_suffix) {
        if (!is.null(self$get_html(sprintf("#%s-%s", input_id_prefix, suffix)))) {
          return(self$get_value(input = sprintf("%s-%s", input_id_prefix, suffix)))
        }
      }

      NULL # If there are not any supported filters
    },
    # @description
    # Check if the page is stable without any `DOM` updates in the body of the app.
    # This is achieved by blocing the R process by sleeping until the page is unchanged till the `stability_period`.
    # @param stability_period (`numeric(1)`) The time in milliseconds to wait till the page to be stable.
    # @param check_interval (`numeric(1)`) The time in milliseconds to check for changes in the page.
    # The stability check is reset when a change is detected in the page after sleeping for check_interval.
    wait_for_page_stability = function(stability_period = 2000, check_interval = 200) {
      previous_content <- self$get_html("body")
      end_time <- Sys.time() + (stability_period / 1000)

      repeat {
        Sys.sleep(check_interval / 1000)
        current_content <- self$get_html("body")

        if (!identical(previous_content, current_content)) {
          previous_content <- current_content
          end_time <- Sys.time() + (stability_period / 1000)
        } else if (Sys.time() >= end_time) {
          break
        }
      }
    }
  )
)
