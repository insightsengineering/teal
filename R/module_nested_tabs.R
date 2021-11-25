# Nesting teal modules in a tab UI

#' Create a UI of nested tabs of `teal_modules`
#'
#' Each `teal_modules` is translated to a `tabsetPanel` and each
#' of its children is put into one tab. The UI of a `teal_module`
#' is obtained by calling the `ui` function on it.
#'
#' The `datasets` argument is required to resolve the teal arguments in an
#' isolated context (with respect to reactivity)
#'
#' @param id module id
#' @inheritParams srv_shiny_module_arguments
#' @return depending on class of `modules`:
#'   - `teal_module`: instantiated UI of the module
#'   - `teal_modules`: `tabsetPanel` with each tab corresponding to recursively
#'     calling this function on it
#' @examples
#' mods <- teal:::get_dummy_modules()
#' datasets <- teal:::get_dummy_datasets()
#' app <- shinyApp(
#'   ui = function() {
#'     tagList(
#'       teal:::include_teal_css_js(),
#'       textOutput("info"),
#'       fluidPage( # needed for nice tabs
#'         teal:::ui_nested_tabs("dummy", modules = mods, datasets = datasets)
#'       )
#'     )
#'   },
#'   server = function(input, output, session) {
#'     active_module <- teal:::srv_nested_tabs("dummy", datasets = datasets, modules = mods)
#'     output$info <- renderText({
#'       paste0("The currently active tab name is ", active_module()$label)
#'     })
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }
ui_nested_tabs <- function(id, modules, datasets) {
  stopifnot(
    # `modules` class tested below
    is(datasets, "FilteredData")
  )

  ns <- NS(id)

  # recursively creates the tabsetted UI within the `ns`,
  # within the `ns`, the tabset panel forms another hierarchy, parent ids within
  # the `ns` are prefixed to the children labels to obtain their ids
  # each module within the `ns` is the label plus the id of its parent (id_parent)
  # is_root: whether top element of modules should be at the root,
  # i.e. in no `tabSet`, ignored if `teal_module`
  # depth: stores the depth of the module nesting
  create_ui <- function(modules, id_parent, is_root = TRUE, depth = 0) {
    stopifnot(
      is_character_single(id_parent) || is.null(id_parent),
      is_logical_single(is_root)
    )

    id <- label_to_id(modules$label, id_parent)
    res <- switch(
      class(modules)[[1]],
      teal_modules = {
        do.call(
          tabsetPanel,
          c(
            # by giving an id, we can reactively respond to tab changes
            list(id = ns(id), type = if (is_root) "pills" else "tabs"),
            unname(lapply(
              modules$children,
              function(submodules) {
                tabPanel(
                  title = submodules$label, # also acts as value of input$tabsetId that this tabPanel is embedded in
                  create_ui(modules = submodules, id_parent = id, is_root = FALSE, depth = depth + 1)
                )
              }
            ))
          )
        )
      },
      teal_module = {
        args <- isolate(resolve_delayed(modules$ui_args, datasets))
        # we pass the unfiltered datasets as they may be needed to create the UI
        tagList(
          if (depth >= 2) div(style = "margin-top: 25px;"),
          do.call(
            modules$ui,
            c(list(id = ns(id), datasets = datasets), args)
          )
        )
      },
      stop("unknown class ", class(modules), ", id_parent ", id_parent)
    )
    return(res)
  }
  return(create_ui(modules, id_parent = NULL))
}

#' Server function that returns currently active module
#'
#' @inheritParams srv_shiny_module_arguments
#' @return `reactive` which returns the active module that corresponds to the selected tab
srv_nested_tabs <- function(id, datasets, modules) {
  # modules checked below through recursion
  stopifnot(
    is(datasets, "FilteredData")
  )

  moduleServer(id = id, module = function(input, output, session) {
    # recursively call `callModule` on (nested) teal modules ----
    call_modules <- function(modules, id_parent) {
      id <- label_to_id(modules$label, prefix = id_parent)

      switch(
        class(modules)[[1]],
        teal_modules = {
          lapply(modules$children, call_modules, id_parent = id)
        },
        teal_module = {
          modules$server_args <- resolve_delayed(modules$server_args, datasets)
          is_module_server <- isTRUE("id" %in% names(formals(modules$server)))
          if (is_module_server) {
            do.call(modules$server, c(list(id = id, datasets = datasets), modules$server_args))
          } else {
            do.call(
              callModule,
              c(
                list(module = modules$server, id = id, datasets = datasets),
                modules$server_args
              )
            )
          }
        },
        stop("unsupported class ", class(modules), ", id_parent is ", id_parent)
      )
      return(invisible(NULL))
    }
    call_modules(modules, id_parent = NULL)

    # figure out currently active module -> `active_datanames` ----

    # the call to `ui_tabs_with_filters` creates inputs that watch the tabs prefixed by `teal_modules`
    # we observe them and react whenever a tab is clicked by:
    # - displaying only the relevant datasets in the right hand filter in the
    # sections: filter info, filtering vars per dataname and add filter variable per dataname
    # recursively goes down tabs to figure out the active module
    get_active_module <- function(modules, id_parent) {
      id <- label_to_id(modules$label, id_parent)
      res <- switch(
        class(modules)[[1]],
        teal_modules = {
          # id is the id of the tabset, the corresponding input element states which tab is selected
          active_submodule_label <- input[[id]]
          stopifnot(!is.null(active_submodule_label))
          get_active_module(modules$children[[active_submodule_label]], id_parent = id)
        },
        teal_module = {
          stopifnot(is.null(input[[id]])) # id should not exist
          modules
        },
        stop("unknown module class ", class(modules))
      )
      return(res)
    }
    active_module <- eventReactive(
      eventExpr = input[[label_to_id(modules$label)]], # this reacts only on the root tabs - nested tabs ignored
      ignoreNULL = TRUE,
      valueExpr = get_active_module(modules, id_parent = NULL)
    )
    return(active_module)
  })

}
