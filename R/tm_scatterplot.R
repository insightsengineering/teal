
#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @inheritParams module
#' @inheritParams standard_layout
#' @param xvar variable name of x varbiable
#' @param yvar variable name of y variable
#' @param xvar_choices vector with variable names of possible x variables. If
#'   missing or identincal to \code{xvar} then the plot will be fixed to the
#'   \code{xvar}.
#' @param yvar_choices vector with variable names of possible y variables. If
#'   missing or identincal to \code{xvar} then the plot will be fixed to the
#'   \code{yvar}.
#' @param color_by variable name of variable that defines the color encoding. If
#'   \code{NULL} then no color encoding option will be displayed. Note
#'   \code{_none_} is a keyword and means that no color encoding should be used.
#' @param color_by_choices vector with variable names that can be used for color
#'   encodings. If missing or identical to \code{color_by} then the color
#'   encoding of the scatterplot points will be fixed to the \code{color_by}
#'   variable.
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with vlaue, min and max.
#' @param alpha if scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically
#'   then it can be a vector of length three with vlaue, min and max.
#' @param size if scalar then the plot points sizes will have a fixed opacity.
#'   If a slider should be presented to adjust the plot point sizes dynamically
#'   then it can be a vector of length three with vlaue, min and max.
#'
#'
#' @export
#' @importFrom ggplot2 aes_string ggplot geom_point
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- teal::init(
#'   data = list(ASL = generate_sample_data('ASL'),
#'               AAE = generate_sample_data('AAE')),
#'   root_modules(
#'      tm_data_table(),
#'      tm_variable_browser(),
#'      tm_scatterplot("Scatterplot Choices",
#'                     dataname = 'AAE',
#'                     xvar = 'AESDY', yvar = 'AEEDY', xvar_choices =  c('AESDY',  'AEEDY'),
#'                     color_by = "_none_", color_by_choices = c("_none_", "AESTMF", "ANLFL")
#'      ),
#'      tm_scatterplot("Scatterplot No Color Choices",
#'                     dataname = 'ASL',
#'                     xvar = 'AGE', yvar = 'TRTDUR', size = 3, alpha = 1, plot_height=600
#'      )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'
#' }
tm_scatterplot <- function(label,
                           dataname,
                           xvar, yvar,
                           xvar_choices = xvar, yvar_choices = yvar,
                           color_by = NULL,
                           color_by_choices = color_by,
                           plot_height = c(600, 200, 2000),
                           alpha = c(1, 0, 1),
                           size = c(4, 1, 12),
                           pre_output=NULL, post_output=NULL) {

  args <- as.list(environment())

  module(
    label = label,
    server = srv_scatterplot,
    ui = ui_scatterplot,
    server_args = list(datasets = 'teal_datasets', dataname),
    ui_args = args,
    filters = dataname
  )

}


ui_scatterplot <- function(id, label,
                           dataname, xvar, yvar,
                           xvar_choices, yvar_choices,
                           color_by, color_by_choices,
                           plot_height, alpha, size,
                           pre_output, post_output) {


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

      if (all(c(
        length(plot_height) == 1,
        length(size) == 1,
        length(alpha) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;")
      },
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", plot_height, ticks = FALSE),
      optionalSliderInputValMinMax(ns("alpha"), "opacity", alpha, ticks = FALSE),
      optionalSliderInputValMinMax(ns("size"), "point size", size, ticks = FALSE)
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

    if (color_by %in% c("", "_none_")) color_by <- NULL


    validate(need(alpha, "need alpha"))
    validate(need(!is.null(ANL) && is.data.frame(ANL), "no data left"))
    validate(need(nrow(ANL) > 0 , "no observations left"))
    validate(need(xvar, "no valid x variable selected"))
    validate(need(yvar, "no valid y variable selected"))
    validate(need(xvar %in% names(ANL),
                  paste("variable", xvar, " is not available in data", dataname)))
    validate(need(yvar %in% names(ANL),
                  paste("variable", yvar, " is not available in data", dataname)))



    p <- ggplot(ANL, aes_string(x = xvar, y = yvar, color = color_by)) +
      geom_point(alpha = alpha, size = size)


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

    if (color_by %in% c("", "_none_")) color_by <- NULL

    str_header <- get_rcode_header(
      title = paste("Scatterplot of", yvar, "vs.", xvar),
      description = "",
      libraries = c(),
      data = setNames(list(datasets$get_data(dataname, reactive=FALSE, filtered = FALSE)), dataname),
      git_repo = "http://github.roche.com/Rpackages/teal/R/tm_scatterplot.R"
    )

    str_filter <- get_filter_txt(dataname, datasets)

    chunks <- parse_code_chunks(txt = capture.output(teal:::srv_scatterplot))

    plot_code <-  if (is.null(color_by)) chunks$plot_no_color else chunks$plot_color



    plot_code_subst <- plot_code %>%
      sub("ggplot(ANL", paste0("ggplot(", dataname, "_FILTERED"), ., fixed = TRUE) %>%
      sub("x = xvar", paste0("x = ", xvar), .) %>%
      sub("y = yvar", paste0("y = ", yvar), .) %>%
      sub("alpha = alpha", paste0("alpha = ", alpha), .) %>%
      sub("size = size", paste0("size = ", size), .)


    code <- paste(
      c(
        "\n",
        str_header, "\n",
        str_filter, "\n",
        plot_code_subst, "\n",
        "p", "\n"
      ), collapse = "\n"
    )


    showModal(modalDialog(
      title = "R Code for the Current Spaghetti Plot",
      tags$pre(tags$code(class="R", code)),
      easyClose = TRUE,
      size = "l"
    ))

  })

}


