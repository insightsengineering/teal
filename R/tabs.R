

#' Create a tabItem/tabsItem collection
#'
#' @export
#'
#' @return object of class \code{teal_tabs}
#'
tabs <- function(...) {
  args <- list(...)

  class_check <- vapply(args, function(x) {is(x, "teal_tab_item") || is(x, "teal_tabs_item")}, logical(1))

  if (any(!class_check)) {
    stop(paste("tabs: not all argument are of class teal_tab_item or teal_tabs_item. Index:",
               paste(which(!class_check), collapse = ", ")))
  }

  class(args) <- "teal_tabs"
  args
}


#' @export
tab_item <- function(label, server, ui, filters, server_args=NULL, ui_args=NULL) {

  force(label); force(server); force(ui); force(filters)

  structure(list(label = label, server = server, ui = ui, filters = filters,
                 server_args = server_args, ui_args = ui_args), class="teal_tab_item")
}


#' @export
tabs_item <- function(label, tabs) {

  if (any(!vapply(tabs, function(x) is(x, "teal_tab_item"), logical(1))))
    stop("tabs_item: not all argument are of class teal_tab_item")

  structure(list(label = label, tabs=tabs), class="teal_tabs_item")
}

#' @export
data_table_item <- function(label = "data table") {
  tab_item(
    label,
    server = srv_page_data_table,
    ui = ui_page_data_table,
    filters = "all",
    server_args = list(datasets='teal_datasets'),
    ui_args = list(datasets='teal_datasets')
  )
}

#' @export
variable_browser_item <- function(label = "variable browser") {
  tab_item(
    label,
    server = srv_page_variable_browser,
    ui = ui_page_variable_browser,
    filters = "all",
    server_args = list(datasets='teal_datasets'),
    ui_args = list(datasets='teal_datasets')
  )
}





label_to_id <- function(label, prefix = NULL) {
  x <- gsub("[[:space:]]+", "_", label)
  if (!is.null(prefix)) paste(prefix, x , sep=".") else x
}

main_nav_id <- "teal_nav"

## create ui part
ui_tabs <- function(x, datasets, idprefix = main_nav_id) {
  tp <- do.call(
    shiny::tabsetPanel,
    c(
      list(id = main_nav_id, type = "pills"),
      as.vector(lapply(x, function(xi) {
        if (class(xi) == "teal_tab_item") ui_tab_item(xi, datasets, idprefix) else ui_tabs_item(xi, datasets, idprefix)
      }))
    )
  )
  tp
}

ui_tab_item <- function(x, datasets, idprefix) {
  args <- Map(function(arg) {if(identical(arg, "teal_datasets")) datasets else arg}, x$ui_args)

  uiid <- label_to_id(x$label, idprefix)

  .log("UI id for tab_item is", uiid)

  shiny::tabPanel(x$label, tagList(div(style="margin-top: 25px;"), do.call(x$ui, c(list(uiid), args))))
}

ui_tabs_item <- function(x, datasets, idprefix) {

  id <- label_to_id(x$label, main_nav_id)

  .log("** UI id for tabs_item is", id)

  tabPanel(
    x$label,
    do.call(
      shiny::tabsetPanel,
      c(
        list(id = id),
        as.vector(lapply(x$tabs, function(xi)ui_tab_item(xi, datasets, label_to_id(x$label, idprefix))))
      )
    )
  )
}


server_tabs <- function(x, datasets, idprefix = main_nav_id) {
  lapply(x, function(xi) if (class(xi) == "teal_tab_item") server_tab_item(xi, datasets, idprefix) else server_tabs_item(xi, datasets, idprefix))
  invisible(NULL)
}

server_tab_item <- function(x, datasets, idprefix) {

  args <- Map(function(arg) {if(identical(arg, "teal_datasets")) datasets else arg}, x$server_args)

  id <-  label_to_id(x$label, idprefix)

  .log("server tab_item  id:", id)

  do.call(
    shiny::callModule,
    c(
     list(module = x$server, id = id),
     args
    )
  )
  invisible(NULL)
}

server_tabs_item <- function(x, datasets, idprefix) {
  server_tabs(x$tabs, datasets, label_to_id(x$label, idprefix))
  invisible(NULL)
}

#' @export
toString.teal_tabs <- function(x, ...) {
   paste(unlist(lapply(x, function(xi) toString(xi, ...))) , collapse = "\n")
}

#' @export
toString.teal_tab_item <- function(x, indent = 0, ...) {
  paste0(paste0(rep(" ", indent), collapse = ""), "+ ", x$label)
}

#' @export
toString.teal_tabs_item <- function(x, ...) {
  paste0(paste0("* ", x$label, ":\n"), toString(x$tabs, indent = 4))
}

#' @export
print.teal_tabs <- function(x, ...) {
  s <- toString(x)
  cat(s)
  invisible(s)
}

#' @export
print.teal_tab_item <- function(x, ...) {
  s <- toString(x)
  cat(s)
  invisible(s)
}

#' @export
print.teal_tabs_item <- function(x, ...) {
  s <- toString(x)
  cat(s)
  invisible(s)
}

