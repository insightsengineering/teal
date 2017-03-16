
srv_page_variable_browser <- function(input, output, session, datasets) {


  # useful to pass on to parent program
  plot_var <- reactiveValues(data=NULL, variable=NULL)

  current_rows <- new.env()


  lapply(datasets$datanames(), function(name) {

    .log("variable label table:", name)

    ui_id <- paste0("variable_browser_", name)

    output[[ui_id]] <- DT::renderDataTable({

      df <- datasets$get_data(name, filtered = FALSE, reactive = TRUE)

      if(is.null(df)) {

        current_rows[[name]] <- character(0)
        data.frame(Variable = character(0), Label=character(0), stringsAsFactors = FALSE)

      } else {

        labels <- unlist(lapply(df, function(x) {
          lab <- attr(x, "label")
          if (is.null(lab)) "" else lab
        }))

        if (name != "asl") {
          asl_vars <- names(datasets$get_data("asl", filtered = FALSE, reactive = FALSE))
          labels <- labels[!(names(labels) %in% asl_vars)]
        }

        # re-arrange labels
        vo <- NULL #  variable_order_of_interest[[name]]
        if (!is.null(vo)) {
          names_labels <- names(labels)
          labels <- labels[c(intersect(vo, names_labels), setdiff(names_labels, vo))]
        }

        current_rows[[name]] <- names(labels)
        data.frame(Variable = names(labels), Label=labels, stringsAsFactors = FALSE)
      }

    }, rownames = FALSE, selection = list(mode='single', target='row', selected=1), server = TRUE)


    ui_id_sel <- paste0(ui_id, "_rows_selected")
    observeEvent(input[[ui_id_sel]], {
      plot_var$data <- name
      plot_var$variable <- current_rows[[name]][input[[ui_id_sel]]]
    })

  })

  observe({plot_var$active <- tolower(input$tsp)})

  output$variable_plot <- renderPlot({

    data <- plot_var$data
    varname <- plot_var$variable
    active <- input$tsp
    type <- input$raw_or_filtered

    validate(
      need(data, "no data selected"),
      need(varname, "no variable selected"),
      need(active, "no tab active"),
      need(type, "select what type of data to plot")
    )

    .log("plot variable", varname, "for data", data, "(", type, ")", " | active:", active)

    validate(need(tolower(active) == tolower(data), "select a variable"))

    df <- datasets$get_data(data, filtered = (type=="filtered"), reactive = TRUE)

    if (is.null(varname)) {
      validate(need(NULL, "no valid variable was selected"))
    } else {

      validate(need(datasets$has_variable(data, varname), "variable not available"))

      var <- df[[varname]]
      Dvarname <- paste0(toupper(data),".",varname)

      grid::grid.newpage()

      plot_grob <- if (is.factor(var) || is.character(var)) {
        groups <- unique(as.character(var))
        if (length(groups) > 30) {
          grid::textGrob( paste0(Dvarname, ":\n  ", paste(groups[1:min(10, length(groups))], collapse = "\n  "), "\n   ...") ,
                    x=grid::unit(1, "line"), y=grid::unit(1,"npc")-grid::unit(1,"line"),
                    just=c("left", "top"))
        } else {
          p <- ggplot2::qplot(var) + ggplot2::xlab(Dvarname) + ggplot2::theme_light() + ggplot2::coord_flip()
          ggplot2::ggplotGrob(p)
        }
      } else if (is.numeric(var)) {
        ## histogram
        p <- ggplot2::qplot(var) + ggplot2::xlab(Dvarname) + ggplot2::theme_light() + ggplot2::coord_flip()
        ggplot2::ggplotGrob(p)
      } else {
        grid::textGrob(str(var), x=grid::unit(1, "line"), y=grid::unit(1,"npc")-grid::unit(1,"line"))
      }

      grid::grid.draw(plot_grob)

    }
  })



  warning_messages <- reactiveValues(varinfo = "", i = 0)
  observeEvent(input$add_filter_variable, {

    dataname   <- plot_var$data
    varname  <- plot_var$variable
    active <- plot_var$active

    .log("add filter variable", dataname, "and", varname, "and active:", active)

    if (!is.null(dataname) && identical(dataname, active)) {
      if (!is.null(varname)) {
        if (datasets$get_filter_type(dataname, varname) == "unknown") {
          warning_messages$varinfo <- paste("variable", paste(toupper(dataname), varname, sep="."), "can currently not be used as a filter variable.")
        } else {
          datasets$set_default_filter_state(dataname, varname)
          warning_messages$varinfo <- ""
          .log("filter added:", varname)
        }
        warning_messages$i <- warning_messages$i + 1
      }
    }

  })


  output$warning <- renderUI({
    warning_messages$i
    msg <- warning_messages$varinfo

    if (is.null(msg)  || msg == "" ) {
      div(style="display: none;")
    } else {
      div(class="text-warning", style="margin-bottom: 15px;", msg)
    }
  })



  NULL

}
