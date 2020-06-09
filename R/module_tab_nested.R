# Nesting teal modules in a tab UI

#' @md
#' @param id module id
#' @param modules `teal_module` or `teal_modules`,
#'   each `teal_module` should have been constructed via `\link{module}`
#'   which performs the checks, i.e. `module$ui` must be a function
#'   with the right arguments
#' @param datasets `FilteredData`
#'
#' @return depending on class of `modules`:
#'   - `teal_module`: instantiated UI of the module
#'   - `teal_modules`: `tabsetPanel` with each tab corresponding to recursively
#'     calling this function on it
#'
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
#' mods
#' datasets <- get_dummy_datasets()
#' shinyApp(
#'   ui = function() {
#'     tagList(
#'       include_teal_css_js(),
#'       fluidPage( # needed for nice tabs
#'         ui_tab_nested("dummy", modules = mods, datasets = datasets)
#'       ),
#'       textOutput("info")
#'     )
#'   },
#'   server = function(input, output, session) {
#'     active_module <- callModule(srv_tab_nested, "dummy", modules = mods, datasets = datasets)
#'     output$info <- renderText({
#'       paste0("The currently active module is ", active_module()$label)
#'     })
#'   }
#' ) %>% invisible() # to not run
ui_tab_nested <- function(id, modules, datasets) {
  stopifnot(
    # `modules` class tested below
    is(datasets, "FilteredData")
  )

  ns <- NS(id)

  # recursively creates the tabsetted UI within the `ns`,
  # within the `ns`, the tabset panel forms another hierarchy, parent ids within
  # the `ns` are prefixed to the children labels to obtain their ids
  # each module within the `ns` is the label plus the id of its parent (idprefix)
  #' is_root whether top element of modules should be at the root,
  #'   i.e. in no `tabSet`, ignored if `teal_module`
  create_ui <- function(modules, idprefix, is_root = TRUE) {
    stopifnot(
      is_character_single(idprefix) || is.null(idprefix),
      is_logical_single(is_root)
    )

    id <- label_to_id(modules$label, idprefix)
    return(switch(
      class(modules)[[1]],
      teal_modules = {
        .log("** UI id for modules is", ns(id))

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
                  create_ui(modules = submodules, idprefix = id, is_root = FALSE)
                )
              }
            ))
          )
        )
      },
      teal_module = {
        .log("UI id for module is", ns(id))

        args <- isolate(resolve_teal_args(modules$ui_args, datasets))
        # we pass the unfiltered datasets as they may be needed to create the UI
        tagList(
          div(style = "margin-top: 25px;"),
          do.call(
            modules$ui,
            c(list(id = ns(id), datasets = datasets), args)
          )
        )
      },
      stop("no default implementation for ui_tab_nested for class ", class(modules), " id_prefix ", idprefix)
    ))
  }
  return(create_ui(modules, idprefix = NULL))
  #todo: idprefix -> id_prefix, parent_id
}

# todo: doc
#' @return `reactive` which returns the active module that corresponds to the selected tab
srv_tab_nested <- function(input, output, session, modules, datasets) {
  # modules checked below through recursion
  stopifnot(
    is(datasets, "FilteredData")
  )

  # recursively call `callModule` on (nested) teal modules ----
  call_modules <- function(modules, idprefix) {
    id <- label_to_id(modules$label, idprefix)

    switch(
      class(modules)[[1]],
      teal_modules = {
        lapply(modules$children, call_modules, idprefix = id)
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
      stop("unsupported class ", class(modules), ", idprefix is ", idprefix)
    )
    return(invisible(NULL))
  }
  call_modules(modules, idprefix = NULL)

  # figure out currently active module -> `active_datanames` ----

  # the call to ui_modules_with_filters creates inputs that watch the tabs prefixed by teal_modules
  # we observe them and react whenever a tab is clicked by:
  # - displaying only the relevant datasets in the right hand filter in the
  # sections: filter info, filtering vars per dataname and add filter var per dataname
  # recursively goes down tabs to figure out the active module
  figure_out_active_module <- function(modules, idprefix) {
    id <- label_to_id(modules$label, idprefix)
    return(switch(
      class(modules)[[1]],
      teal_modules = {
        # id is the id of the tabset, the corresponding input element states which tab is selected
        active_submodule_label <- input[[id]]
        stopifnot(!is.null(active_submodule_label))
        figure_out_active_module(modules$children[[active_submodule_label]], idprefix = id)
      },
      teal_module = {
        stopifnot(is.null(input[[id]])) # id should not exist
        modules
      },
      stop("unknown module class ", class(modules))
    ))
  }

  active_module <- reactive({
    # inputs may be NULL when UI hasn't loaded yet, but this expression still is evaluated
    req(!is.null(input[[label_to_id(modules$label)]]))

    figure_out_active_module(modules, idprefix = NULL)
  })
  return(active_module)
}

