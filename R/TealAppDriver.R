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
  public = list(
    #' @description
    #' Initialize a `TealAppDriver` object for testing a `teal` application.
    #'
    #' @param data (`teal_data` or `teal_data_module`)
    #' For constructing the data object, refer to [teal_data()] and [teal_data_module()].
    #' @param modules (`list` or `teal_modules` or `teal_module`)
    #'   nested list of `teal_modules` or `teal_module` objects or a single
    #'   `teal_modules` or `teal_module` object. These are the specific output modules which
    #'   will be displayed in the `teal` application. See [modules()] and [module()] for
    #'   more details.
    #' @param filter (`teal_slices`)
    #'   Specifies the initial filter using [teal_slices()].
    #' @param title (`shiny.tag` or `character(1)`)
    #'   The browser window title. Defaults to a title "teal app" with the icon of NEST.
    #'   Can be created using the `build_app_title()` or
    #'   by passing a valid `shiny.tag` which is a head tag with title and link tag.
    #' @param header (`shiny.tag` or `character(1)`)
    #'   The header of the app.
    #' @param footer (`shiny.tag` or `character(1)`)
    #'   The footer of the app.
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

      self
    },
    #' @description
    #' Check if the app has shiny errors. This checks for global shiny errors.
    #' Note that any shiny errors dependant on shiny server render will only be captured after the teal module tab
    #' is visited because shiny will not trigger server computations when the tab is invisible.
    #' So, navigate to the module tab you want to test before calling this function.
    #' Although, this catches errors hidden in the other module tabs if they are already rendered.
    expect_no_shiny_error = function() {
      expect_null(
        self$get_html(".shiny-output-error:not(.shiny-output-error-validation)"),
        info = "Shiny error is observed"
      )
    },
    #' @description
    #' Check if the app has no validation errors. This checks for global shiny validation errors.
    expect_no_validation_error = function() {
      expect_null(
        self$get_html(".shiny-output-error-validation"),
        info = "No validation error is observed"
      )
    },
    #' @description
    #' Check if the app has validation errors. This checks for global shiny validation errors.
    expect_validation_error = function() {
      expect_true(
        !is.null(self$get_html(".shiny-output-error-validation")),
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
    #' @param tabs (character) The tabs to navigate to. The order of the tabs is important,
    #' and it should start with the most parent level tab.
    #' Note: In case the teal tab group has duplicate names, the first tab will be selected,
    #' If you wish to select the second tab with the same name, use the suffix "_1".
    #' If you wish to select the third tab with the same name, use the siffix "_2" and so on.
    #'
    #' @return The `TealAppDriver` object invisibly.
    navigate_teal_tab = function(tabs) {
      root <- "root"
      for (tab in tabs) {
        self$set_input(
          sprintf("teal-main_ui-%s-active_tab", root),
          get_unique_labels(tab),
          wait_ = FALSE
        )
        root <- sprintf("%s-%s", root, get_unique_labels(tab))
      }
      self$wait_for_idle(timeout = private$idle_timeout)
      private$set_active_ns()
      invisible(self)
    },
    #' @description
    #' Get the active shiny name space for the Module content and the Filter panel.
    #' Note that in the case of the filter panel, the name space is constant when it is not moudle specific.
    #' However, module specific filter panel will have the name space linked with the module name space.
    #'
    #' @param component (character) The component to get the active name space for.
    #' Currently supported components are "module", "filter_panel", and "filter_manager".
    #'
    #' @return The active shiny name space of the component.
    get_active_ns = function(component) {
      switch(component,
        "module" = private$active_ns$module,
        "filter_panel" = private$active_ns$filter_panel,
        "filter_manager" = private$active_ns$filter_manager,
        stop("Unsupported component passed. Supported components are 'module', 'filter_panel', or 'filter_manager'.")
      )
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
    helper_NS = function(namespace, ...) { # nolint: object_name_linter.
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
    get_module_input = function(input_id) {
      self$get_value(input = sprintf("%s-%s", self$get_active_ns("module"), input_id))
    },
    #' @description
    #' Get the output from the module in the `teal` app.
    #' This function will only access outputs from the name space of the current active teal module.
    #'
    #' @param output_id (character) The shiny output id to get the value from.
    #'
    #' @return The value of the shiny output.
    get_module_output = function(output_id) {
      self$get_value(output = sprintf("%s-%s", self$get_active_ns("module"), output_id))
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
      self$set_inputs(
        !!sprintf("%s-%s", self$get_active_ns("module"), input_id) := value
      )
      invisible(self)
    },
    #' @description
    #' Get the active datasets that can be accessed via the filter panel of the current active teal module.
    get_active_filter_vars = function() {
      displayed_datasets_index <- sapply(
        self$get_html(
          sprintf(
            "#%s-active-filter_active_vars_contents > span",
            self$get_active_ns("filter_panel")
          )
        ),
        function(x) {
          style <- x %>%
            rvest::read_html() %>%
            rvest::html_element("span") %>%
            rvest::html_attr("style")
          style <- ifelse(is.na(style), "", style)
          style != "display: none;"
        }
      ) %>%
        unname()
      available_datasets <- self$get_html(
        sprintf(
          "#%s-active-filter_active_vars_contents",
          self$get_active_ns("filter_panel")
        )
      ) %>%
        read_html() %>%
        html_elements(".filter_panel_dataname") %>%
        html_text()
      available_datasets[displayed_datasets_index]
    },
    #' @description
    #' Get the active filter variables from a dataset in the `teal` app.
    #'
    #' @param dataset_name (character) The name of the dataset to get the filter variables from.
    get_active_data_filters = function(dataset_name) {
      sapply(
        self$get_html(
          sprintf(
            "#%s-active-%s-filter-cards .filter-card-varname",
            self$get_active_ns("filter_panel"),
            dataset_name
          )
        ),
        function(x) {
          x %>%
            rvest::read_html() %>%
            rvest::html_text() %>%
            gsub(pattern = "\\s", replacement = "")
        }
      ) %>%
        unname()
    },
    #' @description
    #' Get the active filter values from the active filter selection of dataset from the filter panel.
    #'
    #' @param dataset_name (character) The name of the dataset to get the filter values from.
    #' @param var_name (character) The name of the variable to get the filter values from.
    #' @param is_numeric (logical) If the variable is numeric or not.
    #'
    #' @return The value of the active filter selection.
    get_filter_selection_value = function(dataset_name, var_name, is_numeric = FALSE) {
      selection_suffix <- ifelse(is_numeric, "selection_manual", "selection")
      self$get_value(
        input = sprintf(
          "%s-active-%s-filter-%s_%s-inputs-%s",
          self$get_active_ns("filter_panel"),
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
          self$get_active_ns("filter_panel"),
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
    #' @param var_name (character) The name of the variable to remove from the filter panel.
    #'
    #' @return The `TealAppDriver` object invisibly.
    remove_filter_var = function(dataset_name, var_name) {
      self$click(
        selector = sprintf(
          "#%s-active-%s-filter-%s_%s-remove",
          self$get_active_ns("filter_panel"),
          dataset_name,
          dataset_name,
          var_name
        )
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
    set_filter_selection_value = function(dataset_name, var_name, input, is_numeric = FALSE) {
      selection_suffix <- ifelse(is_numeric, "selection_manual", "selection")
      self$set_inputs(
        !!sprintf(
          "%s-active-%s-filter-%s_%s-inputs-%s",
          self$get_active_ns("filter_panel"),
          dataset_name,
          dataset_name,
          var_name,
          selection_suffix
        ) := input
      )
      invisible(self)
    },
    #' @description
    #' Click on the filter manager show button.
    #'
    #' @return The `TealAppDriver` object invisibly.
    open_filter_manager = function() {
      active_ns <- self$get_active_ns("filter_manager")
      ns <- self$helper_NS(active_ns)

      self$click(ns("show"))
      self$wait_for_idle(500)
      invisible(self)
    }
  ),
  private = list(
    data = NULL,
    modules = NULL,
    filter = teal_slices(),
    active_ns = list(
      module = NULL,
      filter_panel = NULL,
      filter_manager = NULL
    ),
    idle_timeout = 20000, # 20 seconds
    load_timeout = 100000, # 100 seconds
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
      private$active_ns$module <- sprintf("%s-%s", active_ns, "module")

      component <- "filter_panel"
      if (!is.null(self$get_html(sprintf("#teal-main_ui-%s", component)))) {
        private$active_ns[[component]] <- sprintf("teal-main_ui-%s", component)
      } else {
        private$active_ns[[component]] <- sprintf("%s-module_%s", active_ns, component)
      }

      component <- "filter_manager"
      if (!is.null(self$get_html(sprintf("#teal-main_ui-%s-show", component)))) {
        private$active_ns[[component]] <- sprintf("teal-main_ui-%s", component)
      } else {
        private$active_ns[[component]] <- sprintf("%s-module_%s", active_ns, component)
      }
    }
  )
)
