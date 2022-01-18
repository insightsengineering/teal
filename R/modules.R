# Functions to create objects of class `teal_module` and `teal_modules` and
# related functions like depth, `toString` and `print`.


#' Create a collection of `module` and `modules` object
#'
#' @description `r lifecycle::badge("stable")`
#' Modules collects a tree of [module()] and [modules()]
#' objects. This is useful to define the navigation structure of a teal app.
#'
#' @param label (`character(1)`)\cr
#'   label of modules collection
#' @param ... (`teal_module` or `teal_modules`)\cr
#'   see [module()] and [modules()] for more details
#'
#' @export
#'
#' @return object of class \code{teal_modules}. Object contains following fields
#' - label: taken from `label` argument
#' - children: list containing objects passed in `...`. List elements are named after
#' their `label` attribute converted to valid `shiny` id.
#'
modules <- function(label, ...) {
  checkmate::assert_string(label)
  submodules <- list(...)
  is_right_class <- vapply(submodules, inherits, logical(1), c("teal_module", "teal_modules"))
  if (any(!is_right_class)) {
    stop(paste(
      "modules: not all arguments are of class teal_module or teal_modules. Indices:",
      toString(which(!is_right_class))
    ))
  }

  # name them so we can more easily access the children
  # beware however that the label of the submodules should not be changed as it must be kept synced
  labels <- vapply(submodules, function(submodule) submodule$label, character(1))
  names(submodules) <- make.unique(gsub("[^[:alnum:]]", "_", tolower(labels)), sep = "_")
  structure(
    list(
      label = label,
      children = submodules
    ),
    class = "teal_modules"
  )
}


#' Create the root modules container
#'
#' @description `r lifecycle::badge("stable")`
#' To be used with [init()] in the `modules` argument.
#'
#' @details
#' The function [modules()] can also be used. The purpose of this
#' function is to not confuse the end-user as the label of the top-module
#' will not be displayed as a tab name (because the root is only one element
#' which has multiple children).
#'
#' @inheritParams modules
#'
#' @export
#'
root_modules <- function(...) {
  if (nargs() == 0) {
    # we don't put this check at the modules level because we want to allow
    # empty modules that only have a filtering panel
    stop("You must provide at least one module.")
  }
  modules(label = "root", ...)
}


#' Create a module with a new shiny page
#'
#' @description `r lifecycle::badge("stable")`
#' Tab items allows you to add a shiny module to the teal app
#'
#' @param label (\code{character}) Label shown in the navigation item for the module.
#' @param server (\code{function}) Shiny server module function
#'   (see \code{\link[shiny]{callModule}} or \code{\link[shiny]{moduleServer}}).
#' @param ui (\code{function}) Shiny ui module function
#'   (see \code{\link[shiny]{callModule}} or \code{\link[shiny]{moduleServer}})
#'   with additional teal-specific \code{datasets} argument.
#' @param filters (\code{character}) A vector with datanames that are relevant for the item. The
#'   filter panel will automatically update the shown filters to include only
#'   filters in the listed datasets. \code{NULL} will hide the filter panel,
#'   and the keyword \code{'all'} will show the filters of all datasets. The
#'   argument can be thought of as \code{'active_datanames'} and may be renamed
#'   in future versions of teal.
#' @param server_args (\code{list}) Named list with additional arguments passed on to the
#'   server function. Note that the \code{FilteredDatasets} object gets
#'   automatically passed to the server function as arguments \code{datasets}.
#' @param ui_args (\code{list}) Named list with additional arguments passed on to the
#'   ui function.
#'
#' @return object of class `teal_module`.
#' @export
#'
module <- function(label, server, ui, filters, server_args = NULL, ui_args = NULL) {
  checkmate::assert_string(label)
  checkmate::assert_function(server)
  checkmate::assert_function(ui)
  checkmate::assert_character(filters, min.len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_list(server_args, null.ok = TRUE)
  checkmate::assert_list(ui_args, null.ok = TRUE)

  server_main_args <- names(formals(server))
  if (!(identical(server_main_args[1:4], c("input", "output", "session", "datasets")) ||
    identical(server_main_args[1:2], c("id", "datasets")))) {
    stop(paste(
      "module() server argument requires a function with ordered arguments:",
      "\ninput, output, session, and datasets (callModule) or id and datasets (moduleServer)"
    ))
  }

  if (length(formals(ui)) < 2 ||
    !identical(names(formals(ui))[[1]], "id") ||
    !identical(names(formals(ui))[[2]], "datasets") && !identical(names(formals(ui))[[2]], "...")
  ) {
    stop(
      "module() ui argument requires a function with two ordered arguments:",
      "\n- 'id'\n- 'datasets' or '...'"
    )
  }

  structure(
    list(
      label = label,
      server = server, ui = ui, filters = filters,
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
#' create_mod <- function(module_name) {
#'   module(
#'     module_name,
#'     server = function(input, output, session, datasets) {},
#'     ui = function(id, ...) {
#'       tags$p(id)
#'     },
#'     filters = "all"
#'   )
#' }
#' mods <- modules(
#'   "d1",
#'   modules(
#'     "d2",
#'     modules(
#'       "d3",
#'       create_mod("aaa1"), create_mod("aaa2"), create_mod("aaa3")
#'     ),
#'     create_mod("bbb")
#'   ),
#'   create_mod("ccc")
#' )
#' stopifnot(teal:::modules_depth(mods) == 3L)
#'
#' mods <- modules(
#'   "a",
#'   modules(
#'     "b1", create_mod("c")
#'   ),
#'   create_mod("b2")
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

#' Convert `teal_modules` to a string
#'
#' @description `r lifecycle::badge("stable")`
#' The first line prints the `modules` label.
#' The consecutive lines recursively list each submodule.
#'
#' @param x (`teal_modules`) to print
#' @param indent (`integer`) indent level;
#'   each submodule is indented one level more
#' @param ... (optional) additional parameters to pass to recursive calls of `toString`
#' @return (`character` value)
#' @export
toString.teal_modules <- function(x, indent = 0, ...) { # nolint
  # argument must be `x` to be consistent with base method
  paste(c(
    paste0(rep(" ", indent), "+ ", x$label),
    unlist(lapply(x$children, toString, indent = indent + 1, ...))
  ), collapse = "\n")
}

#' Convert `teal_module` to a string
#' @description `r lifecycle::badge("stable")`
#' @param x `teal_module`
#' @inheritParams toString.teal_modules
#' @param ... ignored
#' @export
toString.teal_module <- function(x, indent = 0, ...) { # nolint
  paste0(paste(rep(" ", indent), collapse = ""), "+ ", x$label, collapse = "")
}

#' Print `teal_modules`
#' @description `r lifecycle::badge("stable")`
#' @param x `teal_modules`
#' @param ... parameters passed to `toString`
#' @export
print.teal_modules <- function(x, ...) {
  s <- toString(x, ...)
  cat(s)
  return(invisible(s))
}

#' Print `teal_module`
#' @description `r lifecycle::badge("stable")`
#' @param x `teal_module`
#' @param ... parameters passed to `toString`
#' @export
print.teal_module <- print.teal_modules
