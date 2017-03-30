
#' Create a variable browser item that plots a variable summary
#'
#' The variable browser provides a table with variable names and labels and a
#' plot the visualizes the content of a particular variable.
#'
#' @inheritParams tab_item
#'
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

## ui function

ui_page_variable_browser <- function(id, datasets) {

  ns <- NS(id)

  div(
    class="row",
    div(
      class="col-md-6",
      # variable browser
      div(class="well", style="background: transparent;",
          do.call(
            tabsetPanel,
            c(id = ns("tsp"),
              do.call(tagList, setNames(lapply(datasets$datanames(), function(domain) {
                ui_id <- paste0('variable_browser_', domain)
                tabPanel(domain, div(
                  style="margin-top: 15px;",
                  DT::dataTableOutput(ns(ui_id), width="100%")))
              }), NULL)
              )
            )
          ),
          checkboxInput(ns("showAslVars"),  "Show ASL variables datasets other than ASL", value = FALSE)
      )
    ),
    div(
      class="col-md-6",
      div(
        class="well",
        style="padding-bottom: 0px",
        plotOutput(ns("variable_plot"), height="500px"),
        div(
          class="clearfix",
          style="margin-top: 15px;",
          div(
            class="pull-left",
            radioButtons(ns("raw_or_filtered"), NULL, choices = c("unfiltered data"='raw', "filtered data"='filtered'), selected="filtered", inline = TRUE)
          ),
          actionLink(ns("add_filter_variable"), "add as filter variable", class="pull-right")
        ),
        uiOutput(ns("warning"))
      )
    )
  )
}


## server function

srv_page_variable_browser <- function(input, output, session, datasets) {


  # useful to pass on to parent program
  plot_var <- reactiveValues(data=NULL, variable=NULL)

  current_rows <- new.env()

  asl_vars <- names(datasets$get_data('ASL'))


  lapply(datasets$datanames(), function(name) {

    .log("variable label table:", name)

    ui_id <- paste0("variable_browser_", name)

    output[[ui_id]] <- DT::renderDataTable({

      df <- datasets$get_data(name, filtered = FALSE, reactive = TRUE)

      showAslVars <- input$showAslVars

      if(is.null(df)) {

        current_rows[[name]] <- character(0)
        data.frame(Variable = character(0), Label=character(0), stringsAsFactors = FALSE)

      } else {

        labels <- unlist(lapply(df, function(x) {
          lab <- attr(x, "label")
          if (is.null(lab)) "" else lab
        }))

        if (!showAslVars && name != "ASL") {
          asl_vars <- names(datasets$get_data("ASL", filtered = FALSE, reactive = FALSE))
          labels <- labels[!(names(labels) %in% asl_vars)]
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
      Dvarname <- paste0(data,".",varname)

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

        grid::textGrob(
          paste(strwrap(capture.output(str(var)), width = .9* grid::convertWidth(grid::unit(1, "npc"), "char", TRUE)), collapse = "\n"),
          x=grid::unit(1, "line"), y=grid::unit(1,"npc")-grid::unit(1,"line"), just=c("left", "top")
        )

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

        if (dataname != "ASL" && varname %in% asl_vars) {
          warning_messages$varinfo <- paste("You can not add an ASL variable from any dataset other than ASL. Switch to the ASL data and add the variable from there.")
        } else if (datasets$get_filter_type(dataname, varname) == "unknown") {
          warning_messages$varinfo <- paste("variable", paste(dataname, varname, sep="."), "can currently not be used as a filter variable.")
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
