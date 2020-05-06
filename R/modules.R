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
    stop(paste("modules: not all argument are of class teal_module or teal_modules. Index:",
               paste(which(!is_right_class), collapse = ", ")))
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
#' `teal_module` and `teal_modules`. At depth
#' Sets the label to root in \code{\link{modules}}
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
  stopifnot(is.null(server_args) || is.list(server_args))
  stopifnot(is.null(ui_args) || is.list(ui_args))

  if (any(vapply(server_args, function(x) identical(x, "teal_datasets"), logical(1)))) {
    warning("teal_datasets is now deprecated, the datasets object gets automatically passed to the server function")
    # todo2: why would you pass the string teal_datasets, it should rather check the name of the argument?
    server_args <- Filter(function(x) !identical(x, "teal_datasets"), server_args)
  }

  if (!identical(names(formals(server))[1:4], c("input", "output", "session", "datasets"))) {
    stop("teal modules need the arguments input, output, session, and datasets in that order in their server function")
  }

  if (!identical(names(formals(ui)[1]), "id")) {
    stop("teal modules need 'id' argument as a first argument in their ui function")
  }
  if (!identical(names(formals(ui)[2]), "datasets") && !identical(names(formals(ui)[2]), "...")) {
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
#' m <- module(
#'   "aaa",
#'   server = function(input, output, session, datasets) {},
#'   ui = function(id, ...) {},
#'   filters = 'all'
#' )
#' x <- modules(
#'   "d1",
#'   modules(
#'     "d2",
#'     modules(
#'       "d3",
#'       m, m, m
#'     ),
#'     m
#'   ),
#'   m
#' )
#' teal:::modules_depth(x)
#'
#' x <- modules(
#'   "a",
#'   modules(
#'     "b", m
#'   ),
#'   m
#' )
#' teal:::modules_depth(x)
modules_depth <- function(modules, depth = 0) {
  if (is(modules, "teal_modules")) {
    max(vapply(modules$children, modules_depth, numeric(1), depth = depth + 1))
  } else {
    depth
  }
}

# turns a label into a valid html id
label_to_id <- function(label, idprefix = NULL) {
  stopifnot(is_character_single(label))
  stopifnot(is_character_single(idprefix) || is.null(idprefix))

  label <- gsub("^_|_$", "", gsub("[^[:alnum:]]", "_", label))
  if (!is.null(idprefix)) {
    idprefix <- gsub("^_|_$", "", gsub("[^[:alnum:]]", "_", idprefix))
    paste(idprefix, label, sep = "_")
  } else {
    label
  }
}


# somehow unexported S3 methods do not work as expected
# modules: teal_module or teal_modules
# is_root: whether top element of modules should be at the root, i.e. in no tabSet
# for teal_module: returns the ui
# for teal_modules: returns a tabsetPanel with each tab corresponding to one of its submodules
tab_nested_ui <- function(modules, datasets, idprefix, is_root = TRUE) {
  stopifnot(is_character_single(idprefix))
  stopifnot(is_logical_single(is_root))

  id <- label_to_id(modules$label, idprefix)
  return(switch(
    class(modules)[[1]],
    teal_modules = {
      .log("** UI id for modules is", id)

      do.call(
        tabsetPanel,
        c(
          # by giving an id, we can reactively respond to tab changes
          list(id = id, type = if (is_root) "pills" else "tabs"),
          unname(lapply(
            modules$children,
            function(submodule) {
              tabPanel(
                title = submodule$label, # also acts as value of input$tabsetId that this tabPanel is embedded in
                tab_nested_ui(submodule, datasets = datasets, idprefix = id, is_root = FALSE)
              )
            }
          ))
        )
      )
    },
    teal_module = {
      .log("UI id for module is", id)

      args <- isolate(resolve_teal_args(modules$ui_args, datasets))
      # we pass the unfiltered datasets as they may be needed to create the UI
      tagList(
        div(style = "margin-top: 25px;"),
        do.call(
          modules$ui,
          c(list(id = id, datasets = datasets), args)
        )
      )
    },
    stop("no default implementation for tab_nested_ui for class ", class(modules))
  ))
}

# todo: remove
tab_nested_ui2 <- function(modules, datasets, idprefix, is_root = TRUE) {
  id <- label_to_id(modules$label, idprefix)

  switch(
    class(modules)[[1]],
    teal_modules = {
      .log("** UI id for modules is", id)

      tsp <- do.call(
        tabsetPanel,
        c(
          list(id = id, type = if (is_root) "pills" else "tabs"),
          as.vector(lapply(modules$children, tab_nested_ui, datasets = datasets, idprefix = id, is_root = FALSE))
        )
      )

      # top element is a single element, so should not be included into a tabsetPanel,
      # so don't wrap it into tabPanel
      if (is_root) tsp else tabPanel(modules$label, tsp)
    },
    teal_module = {
      # returns a tabPanel to be included into a tabsetPanel
      .log("UI id for module is", id)

      args <- isolate(resolve_teal_args(modules$ui_args, datasets))

      # we pass the unfiltered datasets as they may be needed to create the UI
      tabPanel(
        modules$label,
        tagList(
          div(style = "margin-top: 25px;"),
          do.call(
            modules$ui,
            c(list(id = id, datasets = datasets), args)
          )
        )
      )
    },
    stop("no default implementation for tab_nested_ui for class ", class(modules))
  )
}

# recursively call callModule for (nested) teal modules
call_teal_modules <- function(modules, datasets, idprefix) {
  stopifnot(is_character_single(idprefix))
  id <- label_to_id(modules$label, idprefix)

  switch(
    class(modules)[[1]],
    teal_modules = {
      lapply(modules$children, call_teal_modules, datasets = datasets, idprefix = id)
    },
    teal_module = {
      .log("server tab_module  id:", id)
      modules <- resolve_teal_module(modules, datasets)
      do.call(
        callModule,
        c(
          list(module = modules$server, id = id, datasets = datasets),
          modules$server_args
        )
      )
    },
    stop("call_teal_modules does not support class ", class(modules))
  )
  return(invisible(NULL))
}

#' Returns single string
#' first line: modules label
#' consecutive lines list recursively each submodule
#' @export
toString.teal_modules <- function(modules, indent = 0, ...) { # nolint
  paste(c(
    paste0(rep(" ", indent), "+ ", modules$label),
    unlist(lapply(modules$children, toString, indent = indent + 1, ...))
  ), collapse = "\n")
}

#' Returns a single string that displays module label with indent
#' @export
toString.teal_module <- function(module, indent = 0, ...) { # nolint
  paste0(paste(rep(" ", indent), collapse = ""), "+ ", module$label, collapse = "")
}

#' @export
print.teal_modules <- function(x, ...) {
  s <- toString(x)
  cat(s)
  invisible(s)
}

#' @export
print.teal_module <- print.teal_modules
