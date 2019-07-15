ui_filter_items <- function(id, dataname) {

  ns <- NS(id)

  div(class = paste0("teal_filter_", dataname),
      uiOutput(ns("uifilters"))
  )

}

#' @importFrom shinyWidgets pickerOptions
srv_filter_items <- function(input, output, session, datasets, dataname, container = div) {

  uistate <- reactiveValues(filters_shown = character(0))

  observeEvent(datasets$get_filter_state(dataname, reactive = TRUE), {

    fs <- datasets$get_filter_state(dataname)
    current <- uistate$filters_shown

    .log("filter items observer updated for data", dataname)

    if (!identical(names(fs), current)) {
      uistate$filters_shown <- names(fs)
    }

  })


  output$uifilters <- renderUI({

    uistate$filters_shown

    .log("update uiFilters")

    fs_data <- datasets$get_filter_state(dataname)

    ns <- session$ns

    if (is.null(fs_data) || length(fs_data) == 0) {
      div()
    } else {

      els <- lapply(names(fs_data), function(var) {

        fi <- datasets$get_filter_info(dataname, var)
        fs <- datasets$get_filter_state(dataname, var)

        id <- paste0("var_", label_to_id(var))
        id_rm <- paste0("rm_", label_to_id(var))

        varlabel <- tagList(
          tags$span(paste0(dataname, ".", var)),
          tags$div(
            tags$small(paste0("[", fi$label, "]"), style = "font-weight:normal"),
            actionLink(ns(id_rm), "remove", style = "font-weight:normal; float:right;")
          )
        )

        el <- if (fi$type == "choices") {
          if (length(fi$choices) > 5) {
            pickerInput(
              ns(id),
              varlabel,
              choices =  fi$choices,
              selected = fs,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                liveSearch = (length(fi$choices) > 20),
                noneSelectedText = "Select a value"
              ),
              width = "100%"
            )
          } else {
            checkboxGroupInput(
              ns(id),
              varlabel,
              choices =  fi$choices,
              selected = fs,
              width = "100%"
            )
          }
        } else if (fi$type == "range") {
          sliderInput(
            ns(id),
            varlabel,
            min = floor(fi$range[1] * 100) / 100,
            max = ceiling(fi$range[2] * 100) / 100,
            value = fs,
            width = "100%"
          )
        } else if (fi$type == "logical") {
          radioButtons(
            ns(id),
            varlabel,
            choices = fi$choices,
            selected = fs,
            inline = TRUE,
            width = "100%"
          )
        } else {
          tags$p(paste(var, "in data", dataname, "has unknown type:", fi$type))
        }

        create_listener(id, id_rm, var)

        container(el)
      })

      do.call(tagList, els)
    }

  })


  ## fiter change listeners
  id_has_bindings <- character(0) # store which variables already have listeners

  create_listener <- function(id, id_rm, varname) {

    force(varname)

    if (!(id %in% id_has_bindings)) {
      observe({
        value <- input[[id]]

        type <- datasets$get_filter_type(dataname, varname)

        .log("Filter Observer: '", id, "', type '", type, "', with value: ",
             if (is.null(value)) "NULL" else value, sep = "")

        if (type == "range") {
          if (length(value) == 2) {
            datasets$set_filter_state(dataname, varname, value)
          }
        } else if (type == "choices") {
          datasets$set_filter_state(dataname, varname, if (length(value) == 0) character(0) else value)
        } else if (type == "logical") {
          if (!is.null(value)) datasets$set_filter_state(dataname, varname, value)
        }

      })

      observeEvent(input[[id_rm]], {

        .log("Remove Filter:", id)

        datasets$remove_filter(dataname, varname)

      })

      id_has_bindings <<- c(id_has_bindings, id)
    }
  }

  NULL

}
