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

  args <- list(...)

  class_check <- vapply(args, function(x) is(x, "teal_module") || is(x, "teal_modules"), logical(1))

  if (any(!class_check)) {
    stop(paste("modules: not all argument are of class teal_module or teal_modules. Index:",
               paste(which(!class_check), collapse = ", ")))
  }

  structure(list(label = label, modules = args), class = "teal_modules")
}


#' Create the root modules container
#'
#' sets the label to root in \code{\link{modules}}
#'
#' @inheritParams modules
#'
#' @export
#'
root_modules <- function(...) {
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
  stopifnot(is_character_vector(filters))
  stopifnot(is.null(server_args) || is.list(server_args))
  stopifnot(is.null(ui_args) || is.list(ui_args))

  if (any(vapply(server_args, function(x) identical(x, "teal_datasets"), logical(1)))) {
    warning("teal_datasets is now deprecated, the datasets object gets automatically passed to the server function")
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
    list(label = label, server = server, ui = ui, filters = filters,
         server_args = server_args, ui_args = ui_args),
    class = "teal_module"
  )
}


#' check that modules has not more than depth 2
#'
#' @param x \code{teal.modules} object
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
modules_depth <- function(x, depth = 0) {
  children_depth <- if (is(x, "teal_modules")) {
    vapply(x$modules, modules_depth, numeric(1), depth = depth + 1)
  } else {
    depth
  }
  max(children_depth)
}



# turns a label into a valid html id
label_to_id <- function(label, prefix = NULL) {
  label <- gsub("^_|_$", "", gsub("[^[:alnum:]]", "_", label))
  if (!is.null(prefix)) {
    prefix <- gsub("^_|_$", "", gsub("[^[:alnum:]]", "_", prefix))
    paste(prefix, label, sep = "_")
  } else {
    label
  }
}


# somehow unexported S3 methods do not work as expected
create_ui <- function(x, datasets, idprefix, is_root = FALSE) {
  switch(
    class(x),
    teal_module = create_ui_teal_module(x, datasets, idprefix, is_root),
    teal_modules = create_ui_teal_modules(x, datasets, idprefix, is_root),
    stop("no default implementation for create_ui")
  )
}

create_ui_teal_modules <- function(x, datasets, idprefix, is_root = FALSE) {

  id <- label_to_id(x$label, idprefix)

  .log("** UI id for modules is", id)

  tsp <- do.call(
    tabsetPanel,
    c(
      list(id = id, type = if (is_root) "pills" else "tabs"),
      as.vector(lapply(x$modules, create_ui, datasets = datasets, idprefix = id))
    )
  )

  if (is_root) tsp else tabPanel(x$label, tsp)
}

create_ui_teal_module <- function(x, datasets, idprefix, is_root = FALSE) {

  args <- Map(function(arg) if (identical(arg, "teal_datasets")) datasets else arg, x$ui_args)

  uiid <- label_to_id(x$label, idprefix)

  .log("UI id for module is", uiid)

  # we pass the unfiltered datasets as they may be needed to create the UI
  tabPanel(
    x$label,
    tagList(
      div(style = "margin-top: 25px;"),
      do.call(
        x$ui,
        c(list(id = uiid, datasets = datasets), args)
      )
    )
  )
}


call_modules <- function(x, datasets, idprefix) {
  switch(
    class(x),
    teal_modules = call_modules_teal_modules(x, datasets, idprefix),
    teal_module = call_modules_teal_module(x, datasets, idprefix),
    stop("no default implementation for call_modules")
  )
}

call_modules_teal_modules <- function(x, datasets, idprefix) {
  id <- label_to_id(x$label, idprefix)

  lapply(x$modules, call_modules, datasets = datasets, idprefix = id)

  invisible(NULL)
}

call_modules_teal_module <- function(x, datasets, idprefix) {

  id <-  label_to_id(x$label, idprefix)

  .log("server tab_module  id:", id)

  do.call(
    callModule,
    c(
     list(module = x$server, id = id),
     datasets = datasets,
     x$server_args
    )
  )

  invisible(NULL)
}


#' @export
toString.teal_modules <- function(x, ...) { # nolint
   paste(unlist(lapply(x, function(xi) toString(xi, ...))), collapse = "\n")
}

#' @export
toString.teal_module <- function(x, indent = 0, ...) { # nolint
  paste0(paste0(rep(" ", indent), collapse = ""), "+ ", x$label)
}

#' @export
print.teal_modules <- function(x, ...) {
  s <- toString(x)
  cat(s)
  invisible(s)
}

#' @export
print.teal_module <- function(x, ...) {
  s <- toString(x)
  cat(s)
  invisible(s)
}
