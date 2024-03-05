library(shinytest2)
library(glue)
library(htmltools)
library(rvest)

module_ns_shiny2 <- function(app) {
  source <- app$get_html("html", outer_html = TRUE)
  module_id <- rvest::html_attr(
    rvest::html_node(rvest::read_html(source), css = ".teal_module"),
    "id"
  )
  NS(paste0(module_id, "-module"))
}

global_fns <- NS("teal-main_ui-filter_panel")
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

add_new_filter_var <- function(app, data_name, var_name, module_id = "") {
  fns <- ifelse(
    module_id == "",
    global_fns,
    NS(glue("teal-main_ui-root-{module_id}-module_filter_panel"))
  )
  app$set_inputs(
    !!fns(
      glue("add-{data_name}-filter-var_to_add")
    ) := var_name
  )
}

remove_filter_var <- function(app, data_name, var_name, module_id = "") {
  fns <- ifelse(
    module_id == "",
    global_fns,
    NS(glue("teal-main_ui-root-{module_id}-module_filter_panel"))
  )
  app$click(
    fns(
      glue("active-{data_name}-filter-{data_name}_{var_name}-remove")
    )
  )
}

get_active_selection_value <- function(app, data_name, filter_name, is_numeric = FALSE, module_id = "") {
  selection_suffix <- ifelse(is_numeric, "selection_manual", "selection")
  fns <- ifelse(
    module_id == "",
    global_fns,
    NS(glue("teal-main_ui-root-{module_id}-module_filter_panel"))
  )
  app$get_value(
    input = fns(
      glue(
        "active-{data_name}-filter-{data_name}_{filter_name}-inputs-{selection_suffix}"
      )
    )
  )
}

# When a module specific filter panel is created, Every module gets it's own name-spaced filter panel
# So, the `module_id` is needed to interact with the filter panel in that case.
get_active_data_filters <- function(app, data_name, module_id = "") {
  filter_id <- ifelse(
    module_id == "",
    "filter_panel",
    glue("root-{module_id}-module_filter_panel")
  )

  sapply(
    app$get_html(
      glue(
        "#teal-main_ui-{filter_id}-active-{data_name}-filter-cards .filter-card-varname"
      )
    ),
    function(x) {
      x |>
        rvest::read_html() |>
        rvest::html_text() |>
        gsub(pattern = "\\s", replacement = "")
    }
  ) |>
    unname()
}

set_active_selection_value <- function(app, data_name, filter_name, input, is_numeric = FALSE, module_id = "") {
  selection_suffix <- ifelse(is_numeric, "selection_manual", "selection")
  fns <- ifelse(
    module_id == "",
    global_fns,
    NS(glue("teal-main_ui-root-{module_id}-module_filter_panel"))
  )
  app$set_inputs(
    !!fns(
      glue(
        "active-{data_name}-filter-{data_name}_{filter_name}-inputs-{selection_suffix}"
      )
    ) := input
  )
}

remove_active_selection <- function(app, data_name, filter_name, module_id = "") {
  fns <- ifelse(
    module_id == "",
    global_fns,
    NS(glue("teal-main_ui-root-{module_id}-module_filter_panel"))
  )
  app$click(
    fns(
      glue("active-{data_name}-filter-{data_name}_{filter_name}-remove")
    )
  )
}

navigate_teal_tab <- function(app, module_id, root_id = "root") {
  app$set_inputs(
    !!(paste0("teal-main_ui-", root_id, "-active_tab")) := module_id
  )
}
