
#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @param label menu label
#' @param pre_output html tags appended below the output
#' @param post_output html tags appended after the output
#'
#' @export
#' @importFrom ggplot2 aes_string ggplot geom_point
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- teal::init(
#'   data = list(ASL = generate_sample_data('ASL'),
#'               AAE = generate_sample_data('AAE')),
#'   tabs(
#'      tm_data_table(),
#'      tm_variable_browser(),
#'      tm_scatterplot("Scatterplot Choices",
#'                     dataname = 'AAE',
#'                     xvar = 'AESDY', yvar = 'AEEDY',
#'                     color_by = "none", color_by_choices = c("none", "AESTMF", "ANLFL")
#'      ),
#'      tm_scatterplot("Scatterplot No Color Choices",
#'                     dataname = 'ASL',
#'                     xvar = 'AGE', yvar = 'TRTDUR'
#'      )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'
#' }
tm_scatterplot <- function(label, dataname, xvar, yvar,
                     xvar_choices = xvar, yvar_choices = yvar,
                     color_by = NULL, color_by_choices=color_by,
                     pre_output=NULL, post_output=NULL,
                     plot_height = 600) {

  if (!(xvar %in% xvar_choices)) stop("xvar is not in xvar_choices")
  if (!(yvar %in% yvar_choices)) stop("yvar is not in yvar_choices")
  if (!is.null(color_by) && !(color_by %in% color_by_choices)) stop("color_by is not in color_by_choices")

  tab_item(
    label = label,
    server = srv_scatterplot,
    ui = ui_scatterplot,
    server_args = list(datasets = 'teal_datasets', dataname),
    ui_args = list(dataname, xvar, yvar,
                   xvar_choices, yvar_choices,
                   color_by, color_by_choices,
                   pre_output, post_output,
                   plot_height = plot_height),
    filters = dataname
  )

}


ui_scatterplot <- function(id, dataname, xvar, yvar,
                     xvar_choices, yvar_choices,
                     color_by, color_by_choices,
                     pre_output, post_output, plot_height) {


  if (plot_height < 200 || plot_height > 2000) stop("plot_height must be between 200 and 2000")

  ns <- NS(id)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(ns("xvar"), "x variable", xvar_choices, xvar, multiple = FALSE),
      optionalSelectInput(ns("yvar"), "y variable", yvar_choices, yvar, multiple = FALSE),
      optionalSelectInput(ns("color_by"), "color by", color_by_choices, color_by, multiple = FALSE),

      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      sliderInput(ns("plot_height"), "plot height", 200, 2000, plot_height, ticks = FALSE),
      sliderInput(ns("alpha"), "opacity", 0, 1, 1, 0.05, ticks = FALSE),
      sliderInput(ns("size"), "point size", 0.2, 12, 3, 0.2, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )

}

srv_scatterplot <- function(input, output, session, datasets, dataname) {


  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("scatterplot"), height=plot_height)
  })

  output$scatterplot <- renderPlot({

    ANL <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    xvar <- input$xvar
    yvar <- input$yvar
    alpha <- input$alpha
    color_by <- input$color_by
    size <- input$size

    if (identical(color_by, "none")) color_by <- NULL

    as.global(ANL)
    as.global(xvar)
    as.global(yvar)
    as.global(alpha)
    as.global(color_by)
    as.global(size)

    validate(need(alpha, "need alpha"))
    validate(need(!is.null(ANL) && is.data.frame(ANL), "no data left"))
    validate(need(nrow(ANL) > 0 , "no observations left"))
    validate(need(xvar, "no valid x variable selected"))
    validate(need(yvar, "no valid y variable selected"))
    validate(need(xvar %in% names(ANL),
                  paste("variable", xvar, " is not available in data", dataname)))
    validate(need(yvar %in% names(ANL),
                  paste("variable", yvar, " is not available in data", dataname)))


    if (is.null(color_by)) {
      # @start_plot_no_color
      p <- ggplot(ANL, aes_string(x = xvar, y = yvar)) +
        geom_point(alpha = alpha, size = size)
      # @end_plot_no_color
    } else {
      # @start_plot_color
      p <- ggplot(ANL, aes_string(x = xvar, y = yvar, color = color_by)) +
        geom_point(alpha = alpha, size = size)
      # @end_plot_color
    }

    p

  })

  observeEvent(input$show_rcode, {

    xvar <- input$xvar
    yvar <- input$yvar
    alpha <- input$alpha
    size <- input$size
    color_by <- input$color_by

    if (identical(color_by, "none"))  color_by <- NULL

    str_header <- output_header(
      title = paste("Scatterplot of", yvar, "vs.", xvar),
      description = "",
      libraries = c(),
      data = setNames(list(datasets$get_data(dataname, reactive=FALSE, filtered = FALSE)), dataname),
      git_repo = "http://github.roche.com/Rpackages/teal/R/tm_scatterplot.R"
    )

    str_filter <- get_filter_txt(dataname, datasets)

    chunks <- parse_code_chunks(txt = capture.output(teal:::srv_scatterplot))

    code <- paste(
      c(
        "\n",
        str_header, "\n\n",
        str_filter, "\n\n",
        if (is.null(color_by)) chunks$plot_no_color else chunks$plot_color, "\n",
        "p", "\n"
      ), collapse = ""
    )


    showModal(modalDialog(
      title = "R Code for the Current Spaghetti Plot",
      tags$pre(tags$code(class="R", code)),
      easyClose = TRUE,
      size = "l"
    ))

  })

}


