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
  inherit = shinytest2::AppDriver,
  # public methods ----
  public = list(
    #' @description
    #' Initialize a `TealAppDriver` object for testing a `teal` application.
    #'
    #' @param data,modules,filter,title,header,footer arguments passed to `init`
    #' @param ... Additional arguments to be passed to `shinytest2::AppDriver$new`
    #'
    #' @return  Object of class `TealAppDriver`
    initialize = function(data,
                          modules,
                          filter = teal_slices(),
                          title = build_app_title(),
                          header = tags$p(),
                          footer = tags$p(),
                          ...) {
      private$data <- data
      private$modules <- modules
      private$filter <- filter
      app <- init(
        data = data,
        modules = modules,
        filter = filter,
        title = title,
        header = header,
        footer = footer
      )
      suppressWarnings(
        super$initialize(
          shinyApp(app$ui, app$server),
          name = "teal",
          variant = platform_variant(),
          ...
        )
      )

      private$set_active_ns()
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
    #' @param tabs (character) Labels of tabs to navigate to. The order of the tabs is important,
    #' and it should start with the most parent level tab.
    #' Note: In case the teal tab group has duplicate names, the first tab will be selected,
    #' If you wish to select the second tab with the same name, use the suffix "_1".
    #' If you wish to select the third tab with the same name, use the suffix "_2" and so on.
    #'
    #' @return The `TealAppDriver` object invisibly.
    navigate_teal_tab = function(tabs) {
      checkmate::check_character(tabs, min.len = 1)
      for (tab in tabs) {
        root <- "root"
        self$set_input(
          sprintf("teal-main_ui-%s-active_tab", root),
          get_unique_labels(tab),
          wait_ = FALSE
        )
        root <- sprintf("%s-%s", private$modules$label, get_unique_labels(tab))
      }
      self$wait_for_idle(timeout = private$idle_timeout)
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
    #' Set the input in the module in the `teal` app.
    #' This function will only set inputs in the name space of the current active teal module.
    #'
    #' @param input_id (character) The shiny input id to get the value from.
    #' @param value The value to set the input to.
    #' @param ... Additional arguments to be passed to `shinytest2::AppDriver$set_inputs`
    #'
    #' @return The `TealAppDriver` object invisibly.
    set_module_input = function(input_id, value, ...) {
      checkmate::check_string(input_id)
      checkmate::check_string(value)
      self$set_input(
        sprintf("%s-%s", self$active_module_ns(), input_id),
        value,
        ...
      )
      invisible(self)
    },
    #' @description
    #' Get the active datasets that can be accessed via the filter panel of the current active teal module.
    get_active_filter_vars = function() {
      displayed_datasets_index <- unlist(
        self$get_js(
          sprintf(
            "Array.from(
                document.querySelectorAll(\"#%s-active-filter_active_vars_contents > span\")
            ).map((el) => window.getComputedStyle(el).display != \"none\");",
            self$active_filters_ns()
          )
        )
      )

      available_datasets <- self$get_text(
        sprintf(
          "#%s-active-filter_active_vars_contents .filter_panel_dataname",
          self$active_filters_ns()
        )
      )
      available_datasets[displayed_datasets_index]
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
          var_names <- self$get_text(
            sprintf(
              "#%s-active-%s-filters .filter-card-varname",
              self$active_filters_ns(),
              x
            )
          ) %>%
            gsub(pattern = "\\s", replacement = "")
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
      self$set_input(
        sprintf(
          "%s-add-%s-filter-var_to_add",
          self$active_filters_ns(),
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
        "%s-active-%s-filter-%s_%s-inputs",
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
        "%s-active-%s-filter-%s_%s-inputs-%s",
        self$active_filters_ns(),
        dataset_name,
        dataset_name,
        var_name,
        slices_suffix
      )

      if (identical(slices_suffix, "selection_manual")) {
        checkmate::assert_numeric(input, len = 2)
        extra_formals <- formals(app$set_inputs)

        checkmate::assert_choice(dots$priority_, formals(self$set_inputs))

        dots <- rlang::list2(...)
        self$run_js(
          sprintf(
            "Shiny.setInputValue('%s:sw.numericRange', [%f, %f], {priority: '%s'})",
            slices_input_id,
            input[[1]],
            input[[2]],
            priority_ = dplyr::coalesce(dots$priority_, "input", .size = 1)
          )
        )
        self$wait_for_idle(
          wait = dplyr::coalesce(dots$wait_, TRUE, .size = 1),
          timeout = dplyr::coalesce(dots$timeout_, private$timeout, .size = 1)
        )
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
      self$get_html_rvest("html") %>%
        rvest::html_nodes(selector) %>%
        rvest::html_attr(attribute)
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
    idle_timeout = 20000, # 20 seconds
    load_timeout = 100000, # 100 seconds
    # private methods ----
    set_active_ns = function() {
      all_inputs <- self$get_values()$input
      active_tab_inputs <- all_inputs[grepl("-active_tab$", names(all_inputs))]

      tab_ns <- lapply(names(active_tab_inputs), function(name) {
        gsub(
          pattern = "-active_tab$",
          replacement = sprintf("-%s", active_tab_inputs[[name]]),
          name
        )
      }) %>%
        unlist()
      active_ns <- tab_ns[1]
      if (length(tab_ns) > 1) {
        for (i in 2:length(tab_ns)) {
          next_ns <- tab_ns[i]
          if (grepl(pattern = active_ns, next_ns)) {
            active_ns <- next_ns
          }
        }
      }
      private$ns$module <- sprintf("%s-%s", active_ns, "module")

      component <- "filter_panel"
      if (!is.null(self$get_html(sprintf("#teal-main_ui-%s", component)))) {
        private$ns[[component]] <- sprintf("teal-main_ui-%s", component)
      } else {
        private$ns[[component]] <- sprintf("%s-module_%s", active_ns, component)
      }
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
        "%s-active-%s-filter-%s_%s-inputs",
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
    }
  )
)
