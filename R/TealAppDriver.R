# FilteredData ------

#' Drive a `teal` application
#'
#' This class inherits the `shinytest2::AppDriver` class and has additional
#' helper functions to help in driving a `teal` application for performing interactions
#' on a `teal` application for implementing `shinytest2` tests.
#'
#' @keywords internal
#'
TealAppDriver <- R6::R6Class( # nolint
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
      for (tab in tabs) {
        self$set_input(
          sprintf("teal-main_ui-%s-active_tab", private$modules$label),
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
    #' Get the active shiny name space for interacting with the filter panel.
    #'
    #' @return (`string`) The active shiny name space of the component.
    filter_manager_ns = function() {
      if (identical(private$ns$filter_manager, character(0))) {
        private$set_active_ns()
      }
      private$ns$filter_manager
    },
    #' @description
    #' Advance utility to help in creating namespace and CSS selectors for Shiny UI.
    #' It is similar with [shiny::NS()] by returning a function that can be used
    #' to create a namespace for the shiny UI.
    #'
    #' This namespace can be enriched with a prefix and suffix to create a CSS selector.
    #'
    #' @param namespace (`character(1)`) The base id to be used for the namespace.
    #' @param ... (`character`) The additional ids to be appended to `namespace`.
    #'
    #' @return A function similar to [shiny::NS()] that is used to create a `character`
    #' namespace for the shiny UI.
    #'
    helper_NS = function(namespace, ...) { # nolint: object_name.
      dots <- rlang::list2(...)
      checkmate::assert_list(dots, types = "character")
      base_id <- namespace
      if (length(dots) > 0) base_id <- paste(c(namespace, dots), collapse = shiny::ns.sep)

      function(..., .css_prefix = "", .css_suffix = "") {
        dots <- rlang::list2(...)
        checkmate::assert_list(dots, types = "character")
        base_string <- sprintf("%s%s%s", .css_prefix, base_id, .css_suffix)
        if (length(dots) == 0) {
          return(base_string)
        }
        (shiny::NS(base_string))(paste(dots, collapse = shiny::ns.sep))
      }
    },
    #' @description
    #' Get the input from the module in the `teal` app.
    #' This function will only access inputs from the name space of the current active teal module.
    #'
    #' @param input_id (character) The shiny input id to get the value from.
    #'
    #' @return The value of the shiny input.
    get_active_module_input = function(input_id) {
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
      self$get_value(output = sprintf("%s-%s", self$active_module_ns(), output_id))
    },
    #' @description
    #' Set the input in the module in the `teal` app.
    #' This function will only set inputs in the name space of the current active teal module.
    #'
    #' @param input_id (character) The shiny input id to get the value from.
    #' @param value The value to set the input to.
    #'
    #' @return The `TealAppDriver` object invisibly.
    set_module_input = function(input_id, value) {
      self$set_input(
        sprintf("%s-%s", self$active_module_ns(), input_id),
        value
      )
      invisible(self)
    },
    #' @description
    #' Get the active datasets that can be accessed via the filter panel of the current active teal module.
    get_active_filter_vars = function() {
      displayed_datasets_index <- vapply(
        self$get_html(
          sprintf(
            "#%s-active-filter_active_vars_contents > span",
            self$active_filters_ns()
          )
        ),
        function(x) {
          style <- x %>%
            rvest::read_html() %>%
            rvest::html_element("span") %>%
            rvest::html_attr("style")
          style <- ifelse(is.na(style), "", style)
          style != "display: none;"
        },
        logical(1),
        USE.NAMES = FALSE
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
      datasets <- self$get_active_filter_vars()
      checkmate::assert_subset(dataset_name, datasets)
      active_filters <- lapply(
        datasets,
        function(x) {
          self$get_text(
            sprintf(
              "#%s-active-%s-filters .filter-card-varname",
              self$active_filters_ns(),
              x
            )
          ) |>
            gsub(pattern = "\\s", replacement = "")
        }
      )
      names(active_filters) <- datasets
      if (!is.null(dataset_name)) {
        active_filters <- active_filters[[dataset_name]]
      }
      active_filters
    },
    #' @description
    #' Get the active filter values from the active filter selection of dataset from the filter panel.
    #'
    #' @param dataset_name (character) The name of the dataset to get the filter values from.
    #' @param var_name (character) The name of the variable to get the filter values from.
    #' @param is_numeric (logical) If the variable is numeric or not.
    #'
    #' @return The value of the active filter selection.
    get_active_filter_selection = function(dataset_name, var_name, is_numeric = FALSE) {
      selection_suffix <- ifelse(is_numeric, "selection_manual", "selection")
      self$get_value(
        input = sprintf(
          "%s-active-%s-filter-%s_%s-inputs-%s",
          self$active_filters_ns(),
          dataset_name,
          dataset_name,
          var_name,
          selection_suffix
        )
      )
    },
    #' @description
    #' Add a new variable from the dataset to be filtered.
    #'
    #' @param dataset_name (character) The name of the dataset to add the filter variable to.
    #' @param var_name (character) The name of the variable to add to the filter panel.
    #'
    #' @return The `TealAppDriver` object invisibly.
    add_filter_var = function(dataset_name, var_name) {
      self$set_input(
        sprintf(
          "%s-add-%s-filter-var_to_add",
          self$active_filters_ns(),
          dataset_name
        ),
        var_name
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
    #' @param is_numeric (logical) If the variable is numeric or not.
    #'
    #' @return The `TealAppDriver` object invisibly.
    set_active_filter_selection = function(dataset_name, var_name, input, is_numeric = FALSE) {
      selection_suffix <- ifelse(is_numeric, "selection_manual", "selection")
      self$set_input(
        sprintf(
          "%s-active-%s-filter-%s_%s-inputs-%s",
          self$active_filters_ns(),
          dataset_name,
          dataset_name,
          var_name,
          selection_suffix
        ),
        input
      )
      invisible(self)
    },
    #' @description
    #' Click on the filter manager show button.
    #'
    #' @return The `TealAppDriver` object invisibly.
    open_filter_manager = function() {
      active_ns <- self$filter_manager_ns()
      ns <- self$helper_NS(active_ns)

      self$click(ns("show"))
      self$wait_for_idle(500)
      invisible(self)
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
      filter_panel = character(0),
      filter_manager = character(0)
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

      component <- "filter_manager"
      if (!is.null(self$get_html(sprintf("#teal-main_ui-%s-show", component)))) {
        private$ns[[component]] <- sprintf("teal-main_ui-%s", component)
      } else {
        private$ns[[component]] <- sprintf("%s-module_%s", active_ns, component)
      }
    }
  )
)
