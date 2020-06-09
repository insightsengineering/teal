# Functions to create objects of class `teal_module` and `teal_modules` and
# related functions like depth, `toString` and `print`.


#' Create a collection of \code{module} and \code{modules} object
#'
#' Modules collects a tree of \code{\link{module}} and \code{\link{modules}}
#' objects. This is useful to define the navigation structure of a teal app.
#'
#' @importFrom methods is
#'
#' @param label label of modules collection
#' @param ... \code{\link{module}} and \code{\link{modules}} object
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

  labels <- lapply(submodules, function(submodule) submodule$label)
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
#' This is the root of all modules. It can contain a list of mixed types of
#' `teal_module` and `teal_modules`.
#' Sets the label to root in \code{\link{modules}}.
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
#' Tab items allows you to add a shiny module to the teal app
#'
#' @param label label shown in the navigation for the item
#' @param server shiny server module function, see
#'   \code{link[shiny]{callModule}}
#' @param ui shiny ui module function (see \code{link[shiny]{callModule}})
#'   with additional teal-specific \code{datasets} argument
#' @param filters a vector with datanames that are relevant for the item. The
#'   filter panel will automatically update the shown filters to include only
#'   filters in the listed data sets. \code{NULL} will hide the filter panel,
#'   and the keyword \code{'all'} will show the filters of all datasets.
#' @param server_args is a named list with additional arguments passed on to the
#'   server function. Note that the \code{FilteredDatasets} object gets
#'   automatically passed to the server function as arguments \code{datasets}.
#' @param ui_args is a named list with additional arguments passed on to the
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
  stopifnot(is.null(server_args) || is.list(server_args)) # sodo3: fully named list?
  stopifnot(is.null(ui_args) || is.list(ui_args))

  if (any(vapply(server_args, function(x) identical(x, "teal_datasets"), logical(1)))) {
    warning("teal_datasets is now deprecated, the datasets object gets automatically passed to the server function")
    # sodo3: why would you pass the string teal_datasets, it should rather check the name of the argument?
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
#' @param modules `teal.module` or `teal.modules` object
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


# Note that for functions to be dispatched with S3, they need to be exported. This will
# not actually export the function with the class specifier, but only make it available
# for S3 method dispatch, see
# https://stackoverflow.com/questions/18512528/how-to-export-s3-method-so-it-is-available-in-namespace

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
    unlist(lapply(x$children, toString, indent = indent + 1, ...))
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
