#' Create `teal_module` and `teal_modules` objects
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
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
#' @param label (`character(1)`) Label shown in the navigation item for the module or module group.
#'   For `modules()` defaults to `"root"`. See `Details`.
#' @param server (`function`) `shiny` module with following arguments:
#'  - `id` - `teal` will set proper `shiny` namespace for this module (see [shiny::moduleServer()]).
#'  - `input`, `output`, `session` - (optional; not recommended) When provided, then [shiny::callModule()]
#'    will be used to call a module. From `shiny` 1.5.0, the recommended way is to use
#'    [shiny::moduleServer()] instead which doesn't require these arguments.
#'  - `data` (optional) When provided, the module will be called with `teal_data` object (i.e. a list of
#'    reactive (filtered) data specified in the `filters` argument) as the value of this argument.
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
#' @param datanames (`character`) A vector with `datanames` that are relevant for the item. The
#'   filter panel will automatically update the shown filters to include only
#'   filters in the listed datasets. `NULL` will hide the filter panel,
#'   and the keyword `"all"` will show filters of all datasets. `datanames` also determines
#'   a subset of datasets which are appended to the `data` argument in server function.
#' @param server_args (named `list`) with additional arguments passed on to the server function.
#' @param ui_args (named `list`) with additional arguments passed on to the UI function.
#' @param x (`teal_module` or `teal_modules`) Object to format/print.
#' @param indent (`integer(1)`) Indention level; each nested element is indented one level more.
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
                   server = function(id, ...) {
                     moduleServer(id, function(input, output, session) {})
                   },
                   ui = function(id, ...) {
                     tags$p(paste0("This module has no UI (id: ", id, " )"))
                   },
                   filters,
                   datanames = "all",
                   server_args = NULL,
                   ui_args = NULL) {
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
    logger::log_warn(msg)
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

  structure(
    list(
      label = label,
      server = server, ui = ui, datanames = unique(datanames),
      server_args = server_args, ui_args = ui_args
    ),
    class = "teal_module"
  )
}

#' @rdname teal_modules
#' @export
#'
modules <- function(..., label = "root") {
  checkmate::assert_string(label)
  submodules <- list(...)
  if (any(vapply(submodules, is.character, FUN.VALUE = logical(1)))) {
    stop(
      "The only character argument to modules() must be 'label' and it must be named, ",
      "change modules('lab', ...) to modules(label = 'lab', ...)"
    )
  }

  checkmate::assert_list(submodules, min.len = 1, any.missing = FALSE, types = c("teal_module", "teal_modules"))
  # name them so we can more easily access the children
  # beware however that the label of the submodules should not be changed as it must be kept synced
  labels <- vapply(submodules, function(submodule) submodule$label, character(1))
  names(submodules) <- make.unique(gsub("[^[:alnum:]]+", "_", labels), sep = "_")
  structure(
    list(
      label = label,
      children = submodules
    ),
    class = "teal_modules"
  )
}

# printing methods ----

#' @rdname teal_modules
#' @export
format.teal_module <- function(x, indent = 0, ...) {
  paste0(paste(rep(" ", indent), collapse = ""), "+ ", x$label, "\n", collapse = "")
}


#' @rdname teal_modules
#' @export
print.teal_module <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}


#' @rdname teal_modules
#' @export
format.teal_modules <- function(x, indent = 0, ...) {
  paste(
    c(
      paste0(rep(" ", indent), "+ ", x$label, "\n"),
      unlist(lapply(x$children, format, indent = indent + 1, ...))
    ),
    collapse = ""
  )
}


#' @rdname teal_modules
#' @export
print.teal_modules <- print.teal_module


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
  names(modules$children) <- make.unique(gsub("[^[:alnum:]]", "_", tolower(labels)), sep = "_")
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


#' Get module depth
#'
#' Depth starts at 0, so a single `teal.module` has depth 0.
#' Nesting it increases overall depth by 1.
#'
#' @inheritParams init
#' @param depth optional, integer determining current depth level
#'
#' @return Depth level for given module.
#' @keywords internal
modules_depth <- function(modules, depth = 0L) {
  checkmate::assert_multi_class(modules, c("teal_module", "teal_modules"))
  checkmate::assert_int(depth, lower = 0)
  if (inherits(modules, "teal_modules")) {
    max(vapply(modules$children, modules_depth, integer(1), depth = depth + 1L))
  } else {
    depth
  }
}

#' Retrieve labels from `teal_modules`
#'
#' @param modules (`teal_modules`)
#' @return A `list` containing the labels of the modules. If the modules are nested,
#' the function returns a nested `list` of labels.
#' @keywords internal
module_labels <- function(modules) {
  if (inherits(modules, "teal_modules")) {
    lapply(modules$children, module_labels)
  } else {
    modules$label
  }
}
