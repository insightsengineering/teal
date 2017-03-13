

srv_filter_items <- function(input, output, session, datasets, dataname, container=div) {

  uistate <- reactiveValues(filters_shown=character(0))

  observeEvent(datasets$get_filter_state(dataname, reactive = TRUE), {

    fs <- datasets$get_filter_state(dataname)
    current <- uistate$filters_shown

    # session$ns("")
    .log("filter items observer updated for data", dataname)

    if(!identical(names(fs), current)) {
      uistate$filters_shown <- names(fs)
    }

  })


  output$uifilters <- renderUI({

    uistate$filters_shown

    .log("update uiFilters")

    ns <- session$ns

    fs <- datasets$get_filter_state(dataname)

    if (is.null(fs) || length(fs) == 0) {
      #      tags$p("No filter variables selected")
      div()
    } else {

      els <- lapply(names(fs), function(var) {

        fi <- datasets$get_filter_info(dataname, var)
        fs <- datasets$get_filter_state(dataname, var)

        id <- paste0("var_",var)
        id_rm <- paste0("rm_", var)

        varlabel <- tagList(tags$span(paste0(toupper(dataname),".", var)), actionLink(ns(id_rm), "remove", style="font-weight:normal;"))

        el <- if (fi$type == "choices") {
          if (length(fi$choices) > 5) {
            selectInput(ns(id), varlabel,
                        choices =  fi$choices,
                        selected = fs,
                        multiple = TRUE)
          } else {
            checkboxGroupInput(ns(id), varlabel,
                               choices =  fi$choices,
                               selected = fs)
          }
        } else if (fi$type == "range") {
          sliderInput(ns(id), varlabel,
                      min = fi$range[1], max = fi$range[2],
                      value = fs,
                      width = "100%")
        } else {
          tags$p(paste(var, "in data", dataname, "has unknown type:", type$class))
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

        .log("Filter Observer:", id, "with value", value)

        type <- datasets$get_filter_type(dataname, varname)

        if (type == "range") {
          if (length(value) == 2) {
            datasets$set_filter_state(dataname, varname, value)
          }
        } else if (type == "choices") {
          val_state <- if(length(value) == 0) character(0) else value
          datasets$set_filter_state(dataname, varname, val_state)
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
