library(shinytest2)
library(glue)
library(rvest)

default_idle_timeout <- 20000

#' Get the `AppDriver` to test the app created from `init`
#'
#' Create the `AppDriver` using the `init` parameters of `teal` to help with testing.
#'
#' @inheritParams init
#' @keywords internal
#' @return  (`AppDriver`) object which can be used to test the app using `shinytest2`.
get_test_app_object <- function(data,
                                modules,
                                filter = teal_slices(),
                                title = build_app_title(),
                                header = tags$p(),
                                footer = tags$p(),
                                id = character(0)) {
  app <- init(
    data = data,
    modules = modules,
    filter = filter,
    title = title,
    header = header,
    footer = footer,
    id = id
  )
  suppressWarnings(
    AppDriver$new(
      shinyApp(app$ui, app$server),
      name = "teal",
      variant = platform_variant(),
      load_timeout = 300000,
      seed = 123
    )
  )
}

# Check if the app has shiny errors
expect_no_shiny_error <- function(app) {
  expect_null(
    app$get_html(".shiny-output-error:not(.shiny-output-error-validation)"),
    info = "Shiny error is observed"
  )
}

# Check if the app has no validation errors
expect_no_validation_error <- function(app) {
  expect_null(
    app$get_html(".shiny-output-error-validation"),
    info = "No validation error is observed"
  )
}

# Check if the app has validation errors
expect_validation_error <- function(app) {
  expect_true(
    !is.null(app$get_html(".shiny-output-error-validation")),
    info = "Validation error is observed"
  )
}

# navigate_teal_tab(app, c("Nested Modules", "Sub Nested Modules", "Nested 4"))
navigate_teal_tab <- function(app, tabs) {
  root <- "root"
  for (tab in tabs) {
    app$set_inputs(
      !!(paste0("teal-main_ui-", root, "-active_tab")) := get_unique_labels(tab),
      wait_ = FALSE
    )
    root <- sprintf("%s-%s", root, get_unique_labels(tab))
  }
}


#' @param component (character) The component to interact with. Can be `module` or `filter`.
get_active_ns <- function(app, component = c("module", "filter_panel")) {
  component <- match.arg(component)

  if (component == "filter_panel") {
    if (!is.null(app$get_html("#teal-main_ui-filter_panel"))) {
      return("teal-main_ui-filter_panel")
    } else {
      component <- sprintf("module_%s", component)
    }
  }
  all_inputs <- app$get_values()$input
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
  sprintf("%s-%s", active_ns, component)
}

# get_module_input(app, "dataname")
get_module_input <- function(app, input_id) {
  active_ns <- get_active_ns(app, "module")
  app$get_value(input = sprintf("%s-%s", active_ns, input_id))
}

# get_module_output(app, "text")
get_module_output <- function(app, output_id) {
  active_ns <- get_active_ns(app, "module")
  app$get_value(output = sprintf("%s-%s", active_ns, output_id))
}


# get_app_modules(app)
get_app_module_tabs <- function(app) {
  lapply(
    app$get_html(selector = "ul.shiny-bound-input"),
    function(x) {
      el <- rvest::read_html(x)
      root_id <- el %>%
        html_node("ul") %>%
        html_attr("id") %>%
        gsub(pattern = "(^teal-main_ui-)|(-active_tab$)", replacement = "")
      tab_id <- el %>%
        html_nodes("li a") %>%
        html_attr("data-value")
      tab_name <- el %>%
        html_nodes("li a") %>%
        html_text()
      data.frame(
        root_id = root_id,
        tab_id = tab_id,
        tab_name = tab_name
      )
    }
  ) %>%
    do.call(what = rbind)
}

# get_active_filter_vars(app)
get_active_filter_vars <- function(app) {
  displayed_data_index <- sapply(
    app$get_html(
      sprintf(
        "#%s-active-filter_active_vars_contents > span",
        get_active_ns(app, "filter")
      )
    ),
    function(x) {
      style <- x %>%
        rvest::read_html() %>%
        rvest::html_node("span") %>%
        rvest::html_attr("style")
      style <- ifelse(is.na(style), "", style)
      style != "display: none;"
    }
  ) %>%
    unname()
  available_data <- app$get_html(
    sprintf(
      "#%s-active-filter_active_vars_contents",
      get_active_ns(app, "filter")
    )
  ) %>%
    read_html() %>%
    html_nodes(".filter_panel_dataname") %>%
    html_text()
  available_data[displayed_data_index]
}

# get_active_data_filters(app, "mtcars")
get_active_data_filters <- function(app, data_name) {
  sapply(
    app$get_html(
      sprintf(
        "#%s-active-%s-filter-cards .filter-card-varname",
        get_active_ns(app, "filter"),
        data_name
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
}

# get_active_selection_value(app, "iris", "Species")
get_active_selection_value <- function(app, data_name, filter_name, is_numeric = FALSE) {
  selection_suffix <- ifelse(is_numeric, "selection_manual", "selection")
  app$get_value(
    input = sprintf(
      "%s-active-%s-filter-%s_%s-inputs-%s",
      get_active_ns(app, "filter"),
      data_name,
      data_name,
      filter_name,
      selection_suffix
    )
  )
}

# add_filter_var(app, "mtcars", "wt")
add_filter_var <- function(app, data_name, var_name) {
  app$set_inputs(
    !!sprintf(
      "%s-add-%s-filter-var_to_add",
      get_active_ns(app, "filter"),
      data_name
    ) := var_name
  )
}

# remove_filter_var(app, "mtcars", "wt")
remove_filter_var <- function(app, data_name, filter_name) {
  app$click(
    selector = sprintf(
      "#%s-active-%s-filter-%s_%s-remove",
      get_active_ns(app, "filter"),
      data_name,
      data_name,
      filter_name
    )
  )
}

# set_active_selection_value(app, "mtcars", "cyl", "4")
set_active_selection_value <- function(app, data_name, filter_name, input, is_numeric = FALSE) {
  selection_suffix <- ifelse(is_numeric, "selection_manual", "selection")
  app$set_inputs(
    !!sprintf(
      "%s-active-%s-filter-%s_%s-inputs-%s",
      get_active_ns(app, "filter"),
      data_name,
      data_name,
      filter_name,
      selection_suffix
    ) := input
  )
}
