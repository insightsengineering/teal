#' Module to transform `reactive` `teal_data`
#'
#' Module calls multiple [`module_teal_data`] in sequence so that `reactive teal_data` output
#' from one module is handed over to the following module's input.
#'
#' @inheritParams module_teal_data
#' @inheritParams teal_modules
#' @return `reactive` `teal_data`
#'
#'
#' @name module_transform_data
#' @keywords internal
NULL

#' @rdname module_transform_data
ui_transform_data <- function(id, transforms, class = "well") {
  checkmate::assert_string(id)
  checkmate::assert_list(transforms, "teal_transform_module", null.ok = TRUE)
  ns <- NS(id)
  labels <- lapply(transforms, function(x) attr(x, "label"))
  ids <- get_unique_labels(labels)
  names(transforms) <- ids

  lapply(
    names(transforms),
    function(name) {
      data_mod <- transforms[[name]]
      wrapper_id <- ns(sprintf("wrapper_%s", name))
      div( # todo: accordion?
        # class .teal_validated changes the color of the boarder on error in ui_validate_reactive_teal_data
        #   For details see tealValidate.js file.
        class = c(class, "teal_validated"),
        title = attr(data_mod, "label"),
        tags$span(
          class = "text-primary mb-4",
          icon("fas fa-square-pen"),
          attr(data_mod, "label")
        ),
        tags$i(
          class = "remove pull-right fa fa-angle-down",
          style = "cursor: pointer;",
          title = "fold/expand transform panel",
          onclick = sprintf("togglePanelItems(this, '%s', 'fa-angle-right', 'fa-angle-down');", wrapper_id)
        ),
        div(
          id = wrapper_id,
          ui_teal_data(id = ns(name), data_module = transforms[[name]])
        )
      )
    }
  )
}

#' @rdname module_transform_data
srv_transform_data <- function(id, data, transforms, modules, failure_callback = function(data) {invisible(NULL)},
                               is_transformer_failed = reactiveValues()) {
  checkmate::assert_string(id)
  assert_reactive(data)
  checkmate::assert_list(transforms, "teal_transform_module", null.ok = TRUE)
  checkmate::assert_class(modules, "teal_module")

  if (length(transforms) == 0L) {
    return(data)
  }

  labels <- lapply(transforms, function(x) attr(x, "label"))
  ids <- get_unique_labels(labels)
  names(transforms) <- ids

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal_data_modules initializing.")
    transformed_data <- Reduce(
      function(previous_result, name) {
        srv_teal_data(
          id = name,
          data = previous_result,
          data_module = transforms[[name]],
          modules = modules,
          failure_callback = failure_callback,
          is_transformer_failed = is_transformer_failed
        )
      },
      x = names(transforms),
      init = data
    )

    is_previous_failed <- reactive({
      #browser()
      idx_this <- which(names(is_transformer_failed) == id)

      is_transformer_failed_list <- reactiveValuesToList(is_transformer_failed)
      idx_failures <- which(unlist(is_transformer_failed_list))

      any(idx_failures < idx_this)
    })

    observeEvent(is_previous_failed(), {
      # this hides transformers only on the first opening of the module
      # need to move it somewhere else where it reacts to changes to transformers input
      # - maybe failure_callback of srv_transform_data
      #hide_transformers(is_transformer_failed, session)
      cat('\n', session$ns(is_previous_failed()), '\n')
    })

    transformed_data
  })
}

hide_transformers <- function(is_transformer_failed, session) {
  is_transformer_failed_list <- reactiveValuesToList(is_transformer_failed)

  which_is_failing <- which(unlist(is_transformer_failed_list))

  if (length(which_is_failing) && length(is_transformer_failed_list) > 2) {

    which_first_to_hide <- which_is_failing[1]+1
    transformers_to_hide <- which_first_to_hide:length(is_transformer_failed_list)

    sapply(transformers_to_hide, function(x){
      selector <-
        paste0("#", gsub("-data_transform", "", session$ns(character(0))),
               " > div > div.col-sm-3.teal_secondary_col > div:nth-child(", 2+x,")")
      #cat('\nhide', selector, '\n')
      shinyjs::hide(selector = selector)
    })
    transformers_to_show <- 1:which_is_failing[1]
  } else {
    transformers_to_show <- 1:length(is_transformer_failed_list)
  }

  sapply(transformers_to_show, function(x){
    selector <-
      paste0("#", gsub("-data_transform", "", session$ns(character(0))),
             " > div > div.col-sm-3.teal_secondary_col > div:nth-child(", 2+x,")")
    #cat('\nshow', selector, '\n')
    shinyjs::show(selector = selector)
  })
}
