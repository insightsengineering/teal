setOldClass("teal_module")
setOldClass("teal_modules")

#' Create `teal_module` and `teal_modules` objects
#'
#' @description
#' Create a nested tab structure to embed modules in a `teal` application.
#'
#' @details
#' `module()` creates an instance of a `teal_module` that can be placed in a `teal` application.
#' `modules()` shapes the structure of a the application by organizing `teal_module` within the navigation panel.
#' It wraps `teal_module` and `teal_modules` objects in a `teal_modules` object,
#' which results in a nested structure corresponding to the nested tabs in the final application.
#'
#' Note that for `modules()` `label` comes after `...`, so it must be passed as a named argument,
#' otherwise it will be captured by `...`.
#'
#' The labels `"global_filters"` and `"Report previewer"` are reserved
#' because they are used by the `mapping` argument of [teal_slices()]
#' and the report previewer module [reporter_previewer_module()], respectively.
#'
#' # Restricting datasets used by `teal_module`:
#'
#' The `datanames` argument controls which datasets are used by the module's server. These datasets,
#' passed via server's `data` argument, are the only ones shown in the module's tab.
#'
#' When `datanames` is set to `"all"`, all datasets in the data object are treated as relevant.
#' However, this may include unnecessary datasets, such as:
#' -	Proxy variables for column modifications
#' -	Temporary datasets used to create final ones
#' -	Connection objects
#'
#' Datasets which name is prefixed in `teal_data` by the dot (`.`) are not displayed in the `teal` application.
#' Please see the _"Hidden datasets"_ section in `vignette("including-data-in-teal-applications").
#'
#' # `datanames` with `transformators`
#' When transformators are specified, their `datanames` are added to the module's `datanames`, which
#' changes the behavior as follows:
#' - If `module(datanames)` is `NULL` and the `transformators` have defined `datanames`, the sidebar
#'   will appear showing the `transformators`' datasets, instead of being hidden.
#' - If `module(datanames)` is set to specific values and any `transformator` has `datanames = "all"`,
#'   the module may receive extra datasets that could be unnecessary
#'
#' @param label (`character(1)`) Label shown in the navigation item for the module or module group.
#'   For `modules()` defaults to `"root"`. See `Details`.
#' @param server (`function`) `shiny` module with following arguments:
#'  - `id` - `teal` will set proper `shiny` namespace for this module (see [shiny::moduleServer()]).
#'  - `input`, `output`, `session` - (optional; not recommended) When provided, then [shiny::callModule()]
#'    will be used to call a module. From `shiny` 1.5.0, the recommended way is to use
#'    [shiny::moduleServer()] instead which doesn't require these arguments.
#'  - `data` (optional) If the server function includes a `data` argument, it will receive a reactive
#'     expression containing the `teal_data` object.
#'  - `datasets` (optional) When provided, the module will be called with `FilteredData` object as the
#'    value of this argument. (See [`teal.slice::FilteredData`]).
#'  - `reporter` (optional) When provided, the module will be called with `Reporter` object as the value
#'    of this argument. (See [`teal.reporter::Reporter`]).
#'  - `filter_panel_api` (optional) When provided, the module will be called with `FilterPanelAPI` object
#'    as the value of this argument. (See [`teal.slice::FilterPanelAPI`]).
#'  - `...` (optional) When provided, `server_args` elements will be passed to the module named argument
#'    or to the `...`.
#' @param ui (`function`) `shiny` UI module function with following arguments:
#'  - `id` - `teal` will set proper `shiny` namespace for this module.
#'  - `...` (optional) When provided, `ui_args` elements will be passed to the module named argument
#'    or to the `...`.
#' @param filters (`character`) Deprecated. Use `datanames` instead.
#' @param datanames (`character`) Names of the datasets relevant to the item.
#' There are 2 reserved values that have specific behaviors:
#' - The keyword `"all"` includes all datasets available in the data passed to the teal application.
#' - `NULL` hides the sidebar panel completely.
#' - If `transformators` are specified, their `datanames` are automatically added to this `datanames`
#'   argument.
#' @param server_args (named `list`) with additional arguments passed on to the server function.
#' @param ui_args (named `list`) with additional arguments passed on to the UI function.
#' @param x (`teal_module` or `teal_modules`) Object to format/print.
#' @param transformators (`list` of `teal_transform_module`) that will be applied to transform module's data input.
#' To learn more check `vignette("transform-input-data", package = "teal")`.
#'
#' @param ...
#' - For `modules()`: (`teal_module` or `teal_modules`) Objects to wrap into a tab.
#' - For `format()` and `print()`: Arguments passed to other methods.
#'
#' @return
#' `module()` returns an object of class `teal_module`.
#'
#' `modules()` returns a `teal_modules` object which contains following fields:
#' - `label`: taken from the `label` argument.
#' - `children`: a list containing objects passed in `...`. List elements are named after
#' their `label` attribute converted to a valid `shiny` id.
#'
#' @name teal_modules
#' @aliases teal_module
#'
#' @examplesShinylive
#' library(teal)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' library(shiny)
#'
#' module_1 <- module(
#'   label = "a module",
#'   server = function(id, data) {
#'     moduleServer(
#'       id,
#'       module = function(input, output, session) {
#'         output$data <- renderDataTable(data()[["iris"]])
#'       }
#'     )
#'   },
#'   ui = function(id) {
#'     ns <- NS(id)
#'     tagList(dataTableOutput(ns("data")))
#'   },
#'   datanames = "all"
#' )
#'
#' module_2 <- module(
#'   label = "another module",
#'   server = function(id) {
#'     moduleServer(
#'       id,
#'       module = function(input, output, session) {
#'         output$text <- renderText("Another Module")
#'       }
#'     )
#'   },
#'   ui = function(id) {
#'     ns <- NS(id)
#'     tagList(textOutput(ns("text")))
#'   },
#'   datanames = NULL
#' )
#'
#' modules <- modules(
#'   label = "modules",
#'   modules(
#'     label = "nested modules",
#'     module_1
#'   ),
#'   module_2
#' )
#'
#' app <- init(
#'   data = teal_data(iris = iris),
#'   modules = modules
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @rdname teal_modules
#' @export
#'
module <- function(label = "module",
                   server = function(id, data, ...) moduleServer(id, function(input, output, session) NULL),
                   ui = function(id, ...) tags$p(paste0("This module has no UI (id: ", id, " )")),
                   filters,
                   datanames = "all",
                   server_args = NULL,
                   ui_args = NULL,
                   transformators = list()) {
  # argument checking (independent)
  ## `label`
  checkmate::assert_string(label)
  if (label == "global_filters") {
    stop(
      sprintf("module(label = \"%s\", ...\n  ", label),
      "Label 'global_filters' is reserved in teal. Please change to something else.",
      call. = FALSE
    )
  }
  if (label == "Report previewer") {
    stop(
      sprintf("module(label = \"%s\", ...\n  ", label),
      "Label 'Report previewer' is reserved in teal. Please change to something else.",
      call. = FALSE
    )
  }

  ## server
  checkmate::assert_function(server)
  server_formals <- names(formals(server))
  if (!(
    "id" %in% server_formals ||
      all(c("input", "output", "session") %in% server_formals)
  )) {
    stop(
      "\nmodule() `server` argument requires a function with following arguments:",
      "\n - id - `teal` will set proper `shiny` namespace for this module.",
      "\n - input, output, session (not recommended) - then `shiny::callModule` will be used to call a module.",
      "\n\nFollowing arguments can be used optionaly:",
      "\n - `data` - module will receive list of reactive (filtered) data specified in the `filters` argument",
      "\n - `datasets` - module will receive `FilteredData`. See `help(teal.slice::FilteredData)`",
      "\n - `reporter` - module will receive `Reporter`. See `help(teal.reporter::Reporter)`",
      "\n - `filter_panel_api` - module will receive `FilterPanelAPI`. (See [teal.slice::FilterPanelAPI]).",
      "\n - `...` server_args elements will be passed to the module named argument or to the `...`"
    )
  }

  if ("datasets" %in% server_formals) {
    warning(
      sprintf("Called from module(label = \"%s\", ...)\n  ", label),
      "`datasets` argument in the server is deprecated and will be removed in the next release. ",
      "Please use `data` instead.",
      call. = FALSE
    )
  }

  ## UI
  checkmate::assert_function(ui)
  ui_formals <- names(formals(ui))
  if (!"id" %in% ui_formals) {
    stop(
      "\nmodule() `ui` argument requires a function with following arguments:",
      "\n - id - `teal` will set proper `shiny` namespace for this module.",
      "\n\nFollowing arguments can be used optionally:",
      "\n - `...` ui_args elements will be passed to the module argument of the same name or to the `...`"
    )
  }

  if (any(c("data", "datasets") %in% ui_formals)) {
    stop(
      sprintf("Called from module(label = \"%s\", ...)\n  ", label),
      "UI with `data` or `datasets` argument is no longer accepted.\n  ",
      "If some UI inputs depend on data, please move the logic to your server instead.\n  ",
      "Possible solutions are renderUI() or updateXyzInput() functions."
    )
  }

  ## `filters`
  if (!missing(filters)) {
    datanames <- filters
    msg <-
      "The `filters` argument is deprecated and will be removed in the next release. Please use `datanames` instead."
    warning(msg)
  }

  ## `datanames` (also including deprecated `filters`)
  # please note a race condition between datanames set when filters is not missing and data arg in server function
  if (!is.element("data", server_formals) && !is.null(datanames)) {
    message(sprintf("module \"%s\" server function takes no data so \"datanames\" will be ignored", label))
    datanames <- NULL
  }
  checkmate::assert_character(datanames, min.len = 1, null.ok = TRUE, any.missing = FALSE)

  ## `server_args`
  checkmate::assert_list(server_args, null.ok = TRUE, names = "named")
  srv_extra_args <- setdiff(names(server_args), server_formals)
  if (length(srv_extra_args) > 0 && !"..." %in% server_formals) {
    stop(
      "\nFollowing `server_args` elements have no equivalent in the formals of the server:\n",
      paste(paste(" -", srv_extra_args), collapse = "\n"),
      "\n\nUpdate the server arguments by including above or add `...`"
    )
  }

  ## `ui_args`
  checkmate::assert_list(ui_args, null.ok = TRUE, names = "named")
  ui_extra_args <- setdiff(names(ui_args), ui_formals)
  if (length(ui_extra_args) > 0 && !"..." %in% ui_formals) {
    stop(
      "\nFollowing `ui_args` elements have no equivalent in the formals of UI:\n",
      paste(paste(" -", ui_extra_args), collapse = "\n"),
      "\n\nUpdate the UI arguments by including above or add `...`"
    )
  }

  ## `transformators`
  if (inherits(transformators, "teal_transform_module")) {
    transformators <- list(transformators)
  }
  checkmate::assert_list(transformators, types = "teal_transform_module")
  transform_datanames <- unlist(lapply(transformators, attr, "datanames"))
  combined_datanames <- if (identical(datanames, "all")) {
    "all"
  } else {
    union(datanames, transform_datanames)
  }

  structure(
    list(
      label = label,
      server = server,
      ui = ui,
      datanames = combined_datanames,
      server_args = server_args,
      ui_args = ui_args,
      transformators = transformators,
      path = label
    ),
    class = "teal_module"
  )
}

#' @rdname teal_modules
#' @export
#'
modules <- function(..., label = character(0)) {
  checkmate::assert_character(label, max.len = 1)
  submodules <- list(...)
  if (any(vapply(submodules, is.character, FUN.VALUE = logical(1)))) {
    stop(
      "The only character argument to modules() must be 'label' and it must be named, ",
      "change modules('lab', ...) to modules(label = 'lab', ...)"
    )
  }

  checkmate::assert_list(submodules, min.len = 1, any.missing = FALSE, types = c("teal_module", "teal_modules"))

  .update_modules_paths(
    structure(
      list(
        label = label,
        children = submodules
      ),
      class = "teal_modules"
    )
  )
}

# printing methods ----

#' @rdname teal_modules
#' @param is_last (`logical(1)`) Whether this is the last item in its parent's children list.
#'   Affects the tree branch character used (L- vs |-)
#' @param parent_prefix (`character(1)`) The prefix inherited from parent nodes,
#'   used to maintain the tree structure in nested levels
#' @param is_root (`logical(1)`) Whether this is the root node of the tree. Only used in
#'   format.teal_modules(). Determines whether to show "TEAL ROOT" header
#' @param what (`character`) Specifies which metadata to display.
#'   Possible values: "datasets", "properties", "ui_args", "server_args", "transformators"
#' @examples
#' mod <- module(
#'   label = "My Custom Module",
#'   server = function(id, data, ...) {},
#'   ui = function(id, ...) {},
#'   datanames = c("ADSL", "ADTTE"),
#'   transformators = list(),
#'   ui_args = list(a = 1, b = "b"),
#'   server_args = list(x = 5, y = list(p = 1))
#' )
#' cat(format(mod))
#' @export
format.teal_module <- function(
    x,
    is_last = FALSE,
    parent_prefix = "",
    what = c("datasets", "properties", "ui_args", "server_args", "decorators", "transformators"),
    ...) {
  empty_text <- ""
  branch <- if (is_last) "L-" else "|-"
  current_prefix <- paste0(parent_prefix, branch, " ")
  content_prefix <- paste0(parent_prefix, if (is_last) "   " else "|  ")

  format_list <- function(lst, empty = empty_text, label_width = 0) {
    if (is.null(lst) || length(lst) == 0) {
      empty
    } else {
      colon_space <- paste(rep(" ", label_width), collapse = "")

      first_item <- sprintf("%s (%s)", names(lst)[1], cli::col_silver(class(lst[[1]])[1]))
      rest_items <- if (length(lst) > 1) {
        paste(
          vapply(
            names(lst)[-1],
            function(name) {
              sprintf(
                "%s%s (%s)",
                paste0(content_prefix, "|  ", colon_space),
                name,
                cli::col_silver(class(lst[[name]])[1])
              )
            },
            character(1)
          ),
          collapse = "\n"
        )
      }
      if (length(lst) > 1) paste0(first_item, "\n", rest_items) else first_item
    }
  }

  bookmarkable <- isTRUE(attr(x, "teal_bookmarkable"))
  reportable <- "reporter" %in% names(formals(x$server))

  transformators <- if (length(x$transformators) > 0) {
    paste(sapply(x$transformators, function(t) attr(t, "label")), collapse = ", ")
  } else {
    empty_text
  }

  decorators <- if (length(x$server_args$decorators) > 0) {
    paste(sapply(x$server_args$decorators, function(t) attr(t, "label")), collapse = ", ")
  } else {
    empty_text
  }

  output <- pasten(current_prefix, cli::bg_white(cli::col_black(x$label)))

  if ("datasets" %in% what) {
    output <- paste0(
      output,
      content_prefix, "|- ", cli::col_yellow("Datasets         : "), paste(x$datanames, collapse = ", "), "\n"
    )
  }
  if ("properties" %in% what) {
    output <- paste0(
      output,
      content_prefix, "|- ", cli::col_blue("Properties:"), "\n",
      content_prefix, "|  |- ", cli::col_cyan("Bookmarkable  : "), bookmarkable, "\n",
      content_prefix, "|  L- ", cli::col_cyan("Reportable    : "), reportable, "\n"
    )
  }
  if ("ui_args" %in% what) {
    x$ui_args$decorators <- NULL
    ui_args_formatted <- format_list(x$ui_args, label_width = 19)
    output <- paste0(
      output,
      content_prefix, "|- ", cli::col_green("UI Arguments     : "), ui_args_formatted, "\n"
    )
  }
  if ("server_args" %in% what) {
    x$server_args$decorators <- NULL
    server_args_formatted <- format_list(x$server_args, label_width = 19)
    output <- paste0(
      output,
      content_prefix, "|- ", cli::col_green("Server Arguments : "), server_args_formatted, "\n"
    )
  }
  if ("decorators" %in% what) {
    output <- paste0(
      output,
      content_prefix, "|- ", cli::col_magenta("Decorators       : "), decorators, "\n"
    )
  }
  if ("transformators" %in% what) {
    output <- paste0(
      output,
      content_prefix, "L- ", cli::col_magenta("Transformators   : "), transformators, "\n"
    )
  }

  output
}

#' @rdname teal_modules
#' @examples
#' custom_module <- function(
#'     label = "label", ui_args = NULL, server_args = NULL,
#'     datanames = "all", transformators = list(), bk = FALSE) {
#'   ans <- module(
#'     label,
#'     server = function(id, data, ...) {},
#'     ui = function(id, ...) {
#'     },
#'     datanames = datanames,
#'     transformators = transformators,
#'     ui_args = ui_args,
#'     server_args = server_args
#'   )
#'   attr(ans, "teal_bookmarkable") <- bk
#'   ans
#' }
#'
#' dummy_transformator <- teal_transform_module(
#'   label = "Dummy Transform",
#'   ui = function(id) div("(does nothing)"),
#'   server = function(id, data) {
#'     moduleServer(id, function(input, output, session) data)
#'   }
#' )
#'
#' plot_transformator <- teal_transform_module(
#'   label = "Plot Settings",
#'   ui = function(id) div("(does nothing)"),
#'   server = function(id, data) {
#'     moduleServer(id, function(input, output, session) data)
#'   }
#' )
#'
#' static_decorator <- teal_transform_module(
#'   label = "Static decorator",
#'   server = function(id, data) {
#'     moduleServer(id, function(input, output, session) {
#'       reactive({
#'         req(data())
#'         within(data(), {
#'           plot <- plot +
#'             ggtitle("This is title") +
#'             xlab("x axis")
#'         })
#'       })
#'     })
#'   }
#' )
#'
#' complete_modules <- modules(
#'   custom_module(
#'     label = "Data Overview",
#'     datanames = c("ADSL", "ADAE", "ADVS"),
#'     ui_args = list(
#'       view_type = "table",
#'       page_size = 10,
#'       filters = c("ARM", "SEX", "RACE"),
#'       decorators = list(static_decorator)
#'     ),
#'     server_args = list(
#'       cache = TRUE,
#'       debounce = 1000,
#'       decorators = list(static_decorator)
#'     ),
#'     transformators = list(dummy_transformator),
#'     bk = TRUE
#'   ),
#'   modules(
#'     label = "Nested 1",
#'     custom_module(
#'       label = "Interactive Plots",
#'       datanames = c("ADSL", "ADVS"),
#'       ui_args = list(
#'         plot_type = c("scatter", "box", "line"),
#'         height = 600,
#'         width = 800,
#'         color_scheme = "viridis"
#'       ),
#'       server_args = list(
#'         render_type = "svg",
#'         cache_plots = TRUE
#'       ),
#'       transformators = list(dummy_transformator, plot_transformator),
#'       bk = TRUE
#'     ),
#'     modules(
#'       label = "Nested 2",
#'       custom_module(
#'         label = "Summary Statistics",
#'         datanames = "ADSL",
#'         ui_args = list(
#'           stats = c("mean", "median", "sd", "range"),
#'           grouping = c("ARM", "SEX")
#'         )
#'       ),
#'       modules(
#'         label = "Labeled nested modules",
#'         custom_module(
#'           label = "Subgroup Analysis",
#'           datanames = c("ADSL", "ADAE"),
#'           ui_args = list(
#'             subgroups = c("AGE", "SEX", "RACE"),
#'             analysis_type = "stratified"
#'           ),
#'           bk = TRUE
#'         )
#'       ),
#'       modules(custom_module(label = "Subgroup Analysis in non-labled modules"))
#'     )
#'   ),
#'   custom_module("Non-nested module")
#' )
#'
#' cat(format(complete_modules))
#' cat(format(complete_modules, what = c("ui_args", "server_args", "transformators")))
#' cat(format(complete_modules, what = c("decorators", "transformators")))
#' @export
format.teal_modules <- function(x, is_root = TRUE, is_last = FALSE, parent_prefix = "", ...) {
  if (is_root) {
    header <- pasten(cli::style_bold("TEAL ROOT"))
    new_parent_prefix <- "  " #' Initial indent for root level
  } else {
    if (!is.null(x$label)) {
      branch <- if (is_last) "L-" else "|-"
      header <- pasten(parent_prefix, branch, " ", cli::style_bold(x$label))
      new_parent_prefix <- paste0(parent_prefix, if (is_last) "   " else "|  ")
    } else {
      header <- ""
      new_parent_prefix <- parent_prefix
    }
  }

  if (length(x$children) > 0) {
    children_output <- character(0)
    n_children <- length(x$children)

    for (i in seq_along(x$children)) {
      child <- x$children[[i]]
      is_last_child <- (i == n_children)

      if (inherits(child, "teal_modules")) {
        children_output <- c(
          children_output,
          format(child,
            is_root = FALSE,
            is_last = is_last_child,
            parent_prefix = new_parent_prefix,
            ...
          )
        )
      } else {
        children_output <- c(
          children_output,
          format(child,
            is_last = is_last_child,
            parent_prefix = new_parent_prefix,
            ...
          )
        )
      }
    }

    paste0(header, paste(children_output, collapse = ""))
  } else {
    header
  }
}

#' @rdname teal_modules
#' @export
print.teal_module <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

#' @rdname teal_modules
#' @export
print.teal_modules <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

# utilities ----
## subset or modify modules ----

#' Append a `teal_module` to `children` of a `teal_modules` object
#' @keywords internal
#' @param modules (`teal_modules`)
#' @param module (`teal_module`) object to be appended onto the children of `modules`
#' @return A `teal_modules` object with `module` appended.
append_module <- function(modules, module) {
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_class(module, "teal_module")
  modules$children <- c(modules$children, list(module))
  labels <- vapply(modules$children, function(submodule) submodule$label, character(1))
  names(modules$children) <- get_unique_labels(labels)
  modules
}

#' Extract/Remove module(s) of specific class
#'
#' Given a `teal_module` or a `teal_modules`, return the elements of the structure according to `class`.
#'
#' @param modules (`teal_modules`)
#' @param class The class name of `teal_module` to be extracted or dropped.
#' @keywords internal
#' @return
#' - For `extract_module`, a `teal_module` of class `class` or `teal_modules` containing modules of class `class`.
#' - For `drop_module`, the opposite, which is all `teal_modules` of  class other than `class`.
#' @rdname module_management
extract_module <- function(modules, class) {
  if (inherits(modules, class)) {
    modules
  } else if (inherits(modules, "teal_module")) {
    NULL
  } else if (inherits(modules, "teal_modules")) {
    Filter(function(x) length(x) > 0L, lapply(modules$children, extract_module, class))
  }
}

#' @keywords internal
#' @return `teal_modules`
#' @rdname module_management
drop_module <- function(modules, class) {
  if (inherits(modules, class)) {
    NULL
  } else if (inherits(modules, "teal_module")) {
    modules
  } else if (inherits(modules, "teal_modules")) {
    do.call(
      "modules",
      c(Filter(function(x) length(x) > 0L, lapply(modules$children, drop_module, class)), label = modules$label)
    )
  }
}

## read modules ----

#' Does the object make use of the `arg`
#'
#' @param modules (`teal_module` or `teal_modules`) object
#' @param arg (`character(1)`) names of the arguments to be checked against formals of `teal` modules.
#' @return `logical` whether the object makes use of `arg`.
#' @rdname is_arg_used
#' @keywords internal
is_arg_used <- function(modules, arg) {
  checkmate::assert_string(arg)
  if (inherits(modules, "teal_modules")) {
    any(unlist(lapply(modules$children, is_arg_used, arg)))
  } else if (inherits(modules, "teal_module")) {
    is_arg_used(modules$server, arg) || is_arg_used(modules$ui, arg)
  } else if (is.function(modules)) {
    isTRUE(arg %in% names(formals(modules)))
  } else {
    stop("is_arg_used function not implemented for this object")
  }
}

#' Retrieve slot from `teal_modules`
#'
#' @param modules (`teal_modules`)
#' @param slot (`character(1)`)
#' @return A `list` containing the `slot` of the modules.
#' If the modules are nested, the function returns a nested `list` of values.
#' @keywords internal
modules_slot <- function(modules, slot) {
  checkmate::assert_string(slot)
  if (inherits(modules, "teal_modules")) {
    lapply(modules$children, modules_slot, slot = slot)
  } else {
    modules[[slot]]
  }
}

#' Retrieve `teal_bookmarkable` attribute from `teal_modules`
#'
#' @param modules (`teal_modules` or `teal_module`) object
#' @return named list of the same structure as `modules` with `TRUE` or `FALSE` values indicating
#' whether the module is bookmarkable.
#' @keywords internal
modules_bookmarkable <- function(modules) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  if (inherits(modules, "teal_modules")) {
    setNames(
      lapply(modules$children, modules_bookmarkable),
      vapply(modules$children, `[[`, "label", FUN.VALUE = character(1))
    )
  } else {
    attr(modules, "teal_bookmarkable", exact = TRUE)
  }
}

.label_to_id <- function(label) make.unique(gsub("[^[:alnum:]]", "_", tolower(label)), sep = "_")

.update_modules_paths <- function(modules, parent_label = NULL, ids = new.env()) {
  if (inherits(modules, "teal_modules")) {
    modules$children <- lapply(
      modules$children,
      .update_modules_paths,
      parent_label = if (length(parent_label)) paste(parent_label, modules$label, sep = " / ") else modules$label,
      ids = ids
    )
  } else if (inherits(modules, "teal_module")) {
    new_label <- if (length(parent_label)) paste(parent_label, modules$label, sep = " / ") else modules$label
    if (new_label %in% ids$values) {
      new_label <- utils::tail(make.unique(c(ids$values, new_label), sep = " - "), 1)
    }
    modules$path <- new_label
    ids$values <- c(ids$values, new_label)
  }
  modules
}
