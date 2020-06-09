# Nesting teal modules in a tab UI

#' @md
#' @param modules `teal_module` or `teal_modules`,
#'   each `teal_module` should have been constructed via `\link{module}`
#'   which performs the checks, i.e. `module$ui` must be a function
#'   with the right arguments
#' @param datasets `FilteredData`
#' @param is_root whether top element of modules should be at the root,
#'   i.e. in no `tabSet`, ignored if `teal_module`
#'
#' @return depending on class of `modules`:
#'   - `teal_module`: instantiated UI of the module
#'   - `teal_modules`: `tabsetPanel` with each tab corresponding to recursively
#'     calling this function on it
#'
#'     todo: example
ui_tab_nested <- function(modules, datasets, idprefix, is_root = TRUE) {
  stopifnot(
    # `modules` class tested below
    is(datasets, "FilteredData"),
    is_character_single(idprefix),
    is_logical_single(is_root)
  )

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
                ui_tab_nested(submodule, datasets = datasets, idprefix = id, is_root = FALSE)
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
    stop("no default implementation for ui_tab_nested for class ", class(modules))
  ))
}

#srv_tab_nested <- function(input, output, session, modules) #todo
# recursively call `callModule` for (nested) teal modules
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
