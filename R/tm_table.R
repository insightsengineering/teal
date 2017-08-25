
#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @param label menu label
#' @param pre_output html tags appended below the output
#' @param post_output html tags appended after the output
#'
#' @export
#' @importFrom xtable xtable
#' @importFrom xtable print.xtable
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- teal::init(
#'   data = list(ASL = generate_sample_data('ASL')),
#'   tabs(
#'      tm_data_table(),
#'      tm_variable_browser(),
#'      tm_table("Table Choices", 'ASL', xvar = 'SEX', yvar = 'RACE',
#'       xvar_choices = c('SEX', 'RACE', 'STUDYID'),
#'       yvar_choices = c('RACE', 'SAFFL')),
#'      tm_table("Table No Choices", 'ASL', 'SEX', 'RACE')
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#' }
tm_table <- function(label, dataname, xvar, yvar,
                     xvar_choices = xvar, yvar_choices = yvar,
                     useNA = c("ifany", "no","always"),
                     pre_output=NULL, post_output=NULL) {

  useNA <- match.arg(useNA)

  if (!(xvar %in% xvar_choices)) stop("xvar is not in xvar_choices")
  if (!(yvar %in% yvar_choices)) stop("xvar is not in xvar_choices")


  tab_item(
    label = label,
    server = srv_table,
    ui = ui_table,
    server_args = list(datasets = 'teal_datasets', dataname),
    ui_args = list(xvar, yvar,
                   xvar_choices, yvar_choices, useNA,
                   pre_output, post_output),
    filters = dataname
  )

}


ui_table <- function(id, xvar, yvar,
                     xvar_choices, yvar_choices,
                     useNA = c("no", "ifany", "always"),
                     pre_output, post_output) {


  ns <- NS(id)

  sel_x <- selectInput(ns("xvar"), label = "x variable (row)",
                       choices = xvar_choices, selected = xvar, multiple = FALSE)

  sel_y <- selectInput(ns("yvar"), label = "y variable (column)",
                       choices = yvar_choices, selected = yvar, multiple = FALSE)

  fluidRow(
    div(
      class="col-md-3",
      div(
        class = "well",
        tags$label("Encodings", class="text-primary"),
        if (length(xvar_choices) == 1) {
          div(
            hidden(sel_x),
            tags$label("x variable (row):"),
            tags$p(xvar)
          )
        } else {
          sel_x
        },
        if (length(yvar_choices) == 1) {
          div(
            hidden(sel_y),
            tags$label("y variable (column):"),
            tags$p(yvar)
          )
        } else {
          sel_y
        },
        radioButtons(ns("useNA"), label = "Display Missing Values",
                     choices = c("no", "ifany", "always"), selected = useNA)
      ),
      div(class="form-group", actionButton(ns("show_rcode"), "Show R Code", width = "100%"))
      # div(class="form-group", actionButton(ns("create_pdf"), "Generate PDF", width = "100%"))
    ),
    div(
      class = "col-md-9",
      div(
        class="well",
        div(id = "pre-output", pre_output),
        div(tableOutput(ns('table'))),
        div(id = "post-output", post_output)
      )
    )
  )

}

srv_table <- function(input, output, session, datasets, dataname) {


  output$table <- renderTable({

    ANL <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    xvar <- input$xvar
    yvar <- input$yvar
    useNA <- input$useNA

    teal:::as.global(xvar)
    teal:::as.global(yvar)

    validate(need(!is.null(ANL) && is.data.frame(ANL), "no data left"))
    validate(need(nrow(ANL) > 0 , "no observations left"))
    validate(need(xvar, "no valid x variable selected"))
    validate(need(yvar, "no valid y variable selected"))
    validate(need(xvar %in% names(ANL),
                  paste("variable", xvar, " is not available in data", dataname)))
    validate(need(yvar %in% names(ANL),
                  paste("variable", yvar, " is not available in data", dataname)))


    tbl <- table(ANL[[xvar]], ANL[[yvar]], useNA = useNA)
    ##knitr::kable(tbl)

    #.GlobalEnv$tbl <- tbl

    #HTML(print.xtable(xtable(tbl), type="html", html.table.attributes =  c("class=table")))


    as.data.frame.matrix(tbl, row.names = rownames(tbl))

  }, rownames=TRUE, bordered=TRUE, html.table.attributes = 'style="background-color:white;"')

  observeEvent(input$show_rcode, {

    xvar <- input$xvar
    yvar <- input$yvar
    useNA <- input$useNA


    str_header <- output_header(
      title = paste("Cross-Table of", yvar, "vs.", xvar),
      description = "",
      libraries = c(),
      data = setNames(list(datasets$get_data(dataname, reactive=FALSE, filtered = FALSE)), dataname),
      git_repo = "http://github.roche.com/Rpackages/teal/R/tm_table.R"
    )

    str_filter <- get_filter_txt(dataname, datasets)

    code <- paste(
      c(
        "\n",
        str_header, "\n\n",
        str_filter, "\n\n",
        paste0("with(",dataname, "_FILTERED, table(", xvar, ", ", yvar,", useNA = '", useNA, "'))"), "\n"
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


