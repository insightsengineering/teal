#' Creates a `teal_modules` object.
#'
#' @description `r lifecycle::badge("stable")`
#' This function collects a list of `teal_modules` and `teal_module` objects and returns a `teal_modules` object
#' containing the passed objects.
#'
#' This function dictates what modules are included in a `teal` application. The internal structure of `teal_modules`
#' shapes the navigation panel of a `teal` application.
#'
#' @param ... (`teal_module` or `teal_modules`) see [module()] and [modules()] for more details
#' @param label (`character(1)`) label of modules collection (default `"root"`).
#' If using the `label` argument then it must be explicitly named.
#' For example `modules("lab", ...)` should be converted to `modules(label = "lab", ...)`
#'
#' @export
#'
#' @return object of class \code{teal_modules}. Object contains following fields
#' - `label`: taken from the `label` argument
#' - `children`: a list containing objects passed in `...`. List elements are named after
#' their `label` attribute converted to a valid `shiny` id.
#' @examples
#' library(shiny)
#'
#' app <- init(
#'   data = teal_data(dataset("iris", iris)),
#'   modules = modules(
#'     label = "Modules",
#'     modules(
#'       label = "Module",
#'       module(
#'         label = "Inner module",
#'         server = function(id, data) {
#'           moduleServer(
#'             id,
#'             module = function(input, output, session) {
#'               output$data <- renderDataTable(data[["iris"]]())
#'             }
#'           )
#'         },
#'         ui = function(id) {
#'           ns <- NS(id)
#'           tagList(dataTableOutput(ns("data")))
#'         },
#'         datanames = "all"
#'       )
#'     ),
#'     module(
#'       label = "Another module",
#'       server = function(id) {
#'         moduleServer(
#'           id,
#'           module = function(input, output, session) {
#'             output$text <- renderText("Another module")
#'           }
#'         )
#'       },
#'       ui = function(id) {
#'         ns <- NS(id)
#'         tagList(textOutput(ns("text")))
#'       },
#'       datanames = NULL
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
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

#' Append a `teal_module` to `children` of a `teal_modules` object
#' @keywords internal
#' @param modules `teal_modules`
#' @param module `teal_module` object to be appended onto the children of `modules`
#' @return `teal_modules` object with `module` appended
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
#' @param modules `teal_modules`
#' @param class The class name of `teal_module` to be extracted or dropped.
#' @keywords internal
#' @return
#' For `extract_module`, a `teal_module` of class `class` or `teal_modules` containing modules of class `class`.
#' For `drop_module`, the opposite, which is all `teal_modules` of  class other than `class`.
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

#' Does the object make use of the `arg`
#'
#' @param modules (`teal_module` or `teal_modules`) object
#' @param arg (`character(1)`) names of the arguments to be checked against formals of `teal` modules.
#' @return `logical` whether the object makes use of `arg`
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


#' Creates a `teal_module` object.
#'
#' @description `r lifecycle::badge("stable")`
#' This function embeds a `shiny` module inside a `teal` application. One `teal_module` maps to one `shiny` module.
#'
#' @param label (`character(1)`) Label shown in the navigation item for the module. Any label possible except
#'  `"global_filters"` - read more in `mapping` argument of [teal::teal_slices].
#' @param server (`function`) `shiny` module with following arguments:
#'  - `id` - teal will set proper shiny namespace for this module (see [shiny::moduleServer()]).
#'  - `input`, `output`, `session` - (not recommended) then [shiny::callModule()] will be used to call a module.
#'  - `data` (optional) module will receive a `tdata` object, a list of reactive (filtered) data specified in
#'     the `filters` argument.
#'  - `datasets` (optional) module will receive `FilteredData`. (See `[teal.slice::FilteredData]`).
#'  - `reporter` (optional) module will receive `Reporter`. (See [teal.reporter::Reporter]).
#   - `filter_panel_api` (optional) module will receive `FilterPanelAPI`. (See [teal.slice::FilterPanelAPI]).
#'  - `...` (optional) `server_args` elements will be passed to the module named argument or to the `...`.
#' @param ui (`function`) Shiny `ui` module function with following arguments:
#'  - `id` - teal will set proper shiny namespace for this module.
#'  - `data` (optional)  module will receive list of reactive (filtered) data specified in the `filters` argument.
#'  - `datasets` (optional)  module will receive `FilteredData`. (See `[teal.slice::FilteredData]`).
#'  - `...` (optional) `ui_args` elements will be passed to the module named argument or to the `...`.
#' @param filters (`character`) Deprecated. Use `datanames` instead.
#' @param datanames (`character`) A vector with `datanames` that are relevant for the item. The
#'   filter panel will automatically update the shown filters to include only
#'   filters in the listed datasets. `NULL` will hide the filter panel,
#'   and the keyword `'all'` will show filters of all datasets. `datanames` also determines
#'   a subset of datasets which are appended to the `data` argument in `server` function.
#' @param server_args (named `list`) with additional arguments passed on to the
#'   `server` function.
#' @param ui_args (named `list`) with additional arguments passed on to the
#'   `ui` function.
#'
#' @return object of class `teal_module`.
#' @export
#' @examples
#' library(shiny)
#'
#' app <- init(
#'   data = teal_data(dataset("iris", iris)),
#'   modules = list(
#'     module(
#'       label = "Module",
#'       server = function(id, data) {
#'         moduleServer(
#'           id,
#'           module = function(input, output, session) {
#'             output$data <- renderDataTable(data[["iris"]]())
#'           }
#'         )
#'       },
#'       ui = function(id) {
#'         ns <- NS(id)
#'         tagList(dataTableOutput(ns("data")))
#'       }
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
module <- function(label = "module",
                   server = function(id, ...) {
                     moduleServer(id, function(input, output, session) {}) # nolint
                   },
                   ui = function(id, ...) {
                     tags$p(paste0("This module has no UI (id: ", id, " )"))
                   },
                   filters,
                   datanames = "all",
                   server_args = NULL,
                   ui_args = NULL) {
  checkmate::assert_string(label)
  checkmate::assert_function(server)
  checkmate::assert_function(ui)
  checkmate::assert_character(datanames, min.len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_list(server_args, null.ok = TRUE, names = "named")
  checkmate::assert_list(ui_args, null.ok = TRUE, names = "named")

  if (!missing(filters)) {
    checkmate::assert_character(filters, min.len = 1, null.ok = TRUE, any.missing = FALSE)
    datanames <- filters
    msg <-
      "The `filters` argument is deprecated and will be removed in the next release. Please use `datanames` instead."
    logger::log_warn(msg)
    warning(msg)
  }

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
      "Label 'Report previewer' is reserved in teal.",
      call. = FALSE
    )
  }
  server_formals <- names(formals(server))
  if (!(
    "id" %in% server_formals ||
      all(c("input", "output", "session") %in% server_formals)
  )) {
    stop(
      "\nmodule() `server` argument requires a function with following arguments:",
      "\n - id - teal will set proper shiny namespace for this module.",
      "\n - input, output, session (not recommended) - then shiny::callModule will be used to call a module.",
      "\n\nFollowing arguments can be used optionaly:",
      "\n - `data` - module will receive list of reactive (filtered) data specified in the `filters` argument",
      "\n - `datasets` - module will receive `FilteredData`. See `help(teal.slice::FilteredData)`",
      "\n - `reporter` - module will receive `Reporter`. See `help(teal.reporter::Reporter)`",
      "\n - `filter_panel_api` - module will receive `FilterPanelAPI`. (See [teal.slice::FilterPanelAPI]).",
      "\n - `...` server_args elements will be passed to the module named argument or to the `...`"
    )
  }

  if (!is.element("data", server_formals) && !is.null(datanames)) {
    message(sprintf("module \"%s\" server function takes no data so \"datanames\" will be ignored", label))
    datanames <- NULL
  }

  srv_extra_args <- setdiff(names(server_args), server_formals)
  if (length(srv_extra_args) > 0 && !"..." %in% server_formals) {
    stop(
      "\nFollowing `server_args` elements have no equivalent in the formals of the `server`:\n",
      paste(paste(" -", srv_extra_args), collapse = "\n"),
      "\n\nUpdate the `server` arguments by including above or add `...`"
    )
  }

  ui_formals <- names(formals(ui))
  if (!"id" %in% ui_formals) {
    stop(
      "\nmodule() `ui` argument requires a function with following arguments:",
      "\n - id - teal will set proper shiny namespace for this module.",
      "\n\nFollowing arguments can be used optionaly:",
      "\n - `data` - module will receive list of reactive (filtered) data specied in the `filters` argument",
      "\n - `datasets` - module will receive `FilteredData`. See `help(teal.slice::FilteredData)`",
      "\n - `...` ui_args elements will be passed to the module argument of the same name or to the `...`"
    )
  }

  ui_extra_args <- setdiff(names(ui_args), ui_formals)
  if (length(ui_extra_args) > 0 && !"..." %in% ui_formals) {
    stop(
      "\nFollowing `ui_args` elements have no equivalent in the formals of `ui`:\n",
      paste(paste(" -", ui_extra_args), collapse = "\n"),
      "\n\nUpdate the `ui` arguments by including above or add `...`"
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


#' Get module depth
#'
#' Depth starts at 0, so a single `teal.module` has depth 0.
#' Nesting it increases overall depth by 1.
#'
#' @inheritParams init
#' @param depth optional, integer determining current depth level
#'
#' @return depth level for given module
#' @keywords internal
#'
#' @examples
#' mods <- modules(
#'   label = "d1",
#'   modules(
#'     label = "d2",
#'     modules(
#'       label = "d3",
#'       module(label = "aaa1"), module(label = "aaa2"), module(label = "aaa3")
#'     ),
#'     module(label = "bbb")
#'   ),
#'   module(label = "ccc")
#' )
#' stopifnot(teal:::modules_depth(mods) == 3L)
#'
#' mods <- modules(
#'   label = "a",
#'   modules(
#'     label = "b1", module(label = "c")
#'   ),
#'   module(label = "b2")
#' )
#' stopifnot(teal:::modules_depth(mods) == 2L)
modules_depth <- function(modules, depth = 0L) {
  checkmate::assert(
    checkmate::check_class(modules, "teal_module"),
    checkmate::check_class(modules, "teal_modules")
  )
  checkmate::assert_int(depth, lower = 0)
  if (inherits(modules, "teal_modules")) {
    max(vapply(modules$children, modules_depth, integer(1), depth = depth + 1L))
  } else {
    depth
  }
}


module_labels <- function(modules) {
  if (inherits(modules, "teal_modules")) {
    lapply(modules$children, module_labels)
  } else {
    modules$label
  }
}

#' Converts `teal_modules` to a string
#'
#' @param x (`teal_modules`) to print
#' @param indent (`integer`) indent level;
#'   each `submodule` is indented one level more
#' @param ... (optional) additional parameters to pass to recursive calls of `toString`
#' @return (`character`)
#' @export
#' @rdname modules
toString.teal_modules <- function(x, indent = 0, ...) { # nolint
  # argument must be `x` to be consistent with base method
  paste(c(
    paste0(rep(" ", indent), "+ ", x$label),
    unlist(lapply(x$children, toString, indent = indent + 1, ...))
  ), collapse = "\n")
}

#' Converts `teal_module` to a string
#'
#' @inheritParams toString.teal_modules
#' @param x `teal_module`
#' @param ... ignored
#' @export
#' @rdname module
toString.teal_module <- function(x, indent = 0, ...) { # nolint
  paste0(paste(rep(" ", indent), collapse = ""), "+ ", x$label, collapse = "")
}

#' Prints `teal_modules`
#' @param x `teal_modules`
#' @param ... parameters passed to `toString`
#' @export
#' @rdname modules
print.teal_modules <- function(x, ...) {
  s <- toString(x, ...)
  cat(s)
  return(invisible(s))
}

#' Prints `teal_module`
#' @param x `teal_module`
#' @param ... parameters passed to `toString`
#' @export
#' @rdname module
print.teal_module <- print.teal_modules
