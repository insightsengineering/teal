# Functions to create objects of class `teal_module` and `teal_modules` and
# related functions like depth, `toString` and `print`.


#' Create a collection of `module` and `modules` object
#'
#' Modules collects a tree of `\link{module}` and `\link{modules}`
#' objects. This is useful to define the navigation structure of a teal app.
#'
#' @importFrom methods is
#'
#' @md
#' @param label label of modules collection
#' @param ... `\link{module}` and `\link{modules}` object
#'
#' @export
#'
#' @return object of class \code{teal_modules}
#'
modules <- function(label, ...) {
  stopifnot(is_character_single(label))

  submodules <- list(...)
  is_right_class <- vapply(submodules, function(x) is(x, "teal_module") || is(x, "teal_modules"), logical(1))
  if (any(!is_right_class)) {
    stop(paste(
      "modules: not all argument are of class teal_module or teal_modules. Indices:",
      toString(which(!is_right_class))
    ))
  }

  labels <- vapply(submodules, function(submodule) submodule$label, character(1))
  if (any(duplicated(labels))) {
    stop("Please choose unique labels for each tab. Currently, they are ", toString(labels))
  }

  # name them so we can more easily access the children
  # beware however that the label of the submodules should not be changed as it must be kept synced
  submodules <- setNames(
    submodules,
    labels
  )
  structure(
    list(label = label, children = submodules),
    class = "teal_modules"
  )
}


#' Create the root modules container
#'
#' To be used with `\link{init}` in the `modules` argument.
#'
#' The function `\link{modules}` can also be used. The purpose of this
#' function is to not confuse the end-user as the label of the top-module
#' will not be displayed as a tab name (because the root is only one element
#' which has multiple children).
#'
#' @md
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
#' Tab items allows you to add a shiny module to the teal app
#'
#' @param label (\code{character}) Label shown in the navigation item for the module.
#' @param server (\code{function}) Shiny server module function, see
#'   \code{\link[shiny]{callModule}}.
#' @param ui (\code{function}) Shiny ui module function (see \code{\link[shiny]{callModule}})
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
#'   ui function. The argument \code{'teal_datasets'} will always be
#'   replaced by the \code{FilteredData} object.
#'
#' @export
#'
module <- function(label, server, ui, filters, server_args = NULL, ui_args = NULL) {
  stopifnot(is_character_single(label))
  stopifnot(is.function(server))
  stopifnot(is.function(ui))
  stopifnot(is_character_vector(filters) || is.null(filters))
  stopifnot(is.null(server_args) || is.list(server_args))
  stopifnot(is.null(ui_args) || is.list(ui_args))

  if (any(vapply(server_args, function(x) identical(x, "teal_datasets"), logical(1)))) {
    warning("teal_datasets is now deprecated, the datasets object gets automatically passed to the server function")
    server_args <- Filter(function(x) !identical(x, "teal_datasets"), server_args)
  }

  if (!identical(names(formals(server))[1:4], c("input", "output", "session", "datasets"))) {
    stop("teal modules need the arguments input, output, session, and datasets in that order in their server function")
  }

  if (!identical(names(formals(ui))[[1]], "id")) {
    stop("teal modules need 'id' argument as a first argument in their ui function")
  }
  if (!identical(names(formals(ui))[[2]], "datasets") && !identical(names(formals(ui))[[2]], "...")) {
    stop("teal modules need 'datasets' or '...' argument as a second argument in their ui function")
  }

  structure(
    list(
      label = label, server = server, ui = ui, filters = filters,
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
#' @md
#' @inheritParams srv_shiny_module_arguments
#' @param depth optional, integer determining current depth level
#'
#' @return depth level for given module
#'
#' @importFrom methods is
#'
#' @examples
#' create_mod <- function(module_name) module(
#'   module_name,
#'   server = function(input, output, session, datasets) {},
#'   ui = function(id, ...) { tags$p(id) },
#'   filters = 'all'
#' )
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
#' stopifnot(teal:::modules_depth(mods) == 3)
#'
#' mods <- modules(
#'   "a",
#'   modules(
#'     "b1", create_mod("c")
#'   ),
#'   create_mod("b2")
#' )
#' stopifnot(teal:::modules_depth(mods) == 2)
modules_depth <- function(modules, depth = 0) {
  if (is(modules, "teal_modules")) {
    max(vapply(modules$children, modules_depth, numeric(1), depth = depth + 1))
  } else {
    stopifnot(is(modules, "teal_module"))
    depth
  }
}

#' Convert `teal_modules` to a string
#'
#' The first line prints the `modules` label.
#' The consecutive lines recursively list each submodule.
#'
#' @md
#' @param x `teal_modules` to print
#' @param indent `integer` indent level;
#'   each submodule is indented one level more
#' @param ... additional parameters to pass to recursive calls of `toString`
#' @return `single character` with lines separated by `\n`
#' @export
toString.teal_modules <- function(x, indent = 0, ...) { # nolint
  # argument must be `x` to be consistent with base method
  paste(c(
    paste0(rep(" ", indent), "+ ", x$label),
    ulapply(x$children, toString, indent = indent + 1, ...)
  ), collapse = "\n")
}

#' Convert `teal_module` to a string
#' @param x `teal_module`
#' @inheritParams toString.teal_modules
#' @param ... ignored
#' @export
toString.teal_module <- function(x, indent = 0, ...) { # nolint
  paste0(paste(rep(" ", indent), collapse = ""), "+ ", x$label, collapse = "")
}

#' Print `teal_modules`
#' @md
#' @param x `teal_modules`
#' @param ... parameters passed to `toString`
#' @export
print.teal_modules <- function(x, ...) {
  s <- toString(x, ...)
  cat(s)
  return(invisible(s))
}

print.teal_module <- print.teal_modules
