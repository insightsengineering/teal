#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @param label menu label
#' @param dataname name of dataset used to generate table
#' @param xvar variable name of x varbiable
#' @param yvar variable name of y variable
#' @param xvar_choices vector with variable names of possible x variables. If
#'   missing or identincal to \code{xvar} then the table will be fixed to the
#'   \code{xvar}.
#' @param yvar_choices vector with variable names of possible y variables. If
#'   missing or identincal to \code{xvar} then the table will be fixed to the
#'   \code{yvar}.
#' @param useNA optional pre-selected option indicating how to utilize NA in
#'   table display. One of \code{'ifany'}, \code{'always'}, \code{'no'}. If
#'   missing then \code{'ifany'} will be used. If vector then only the first
#'   one will be used.
#' @param pre_output html tags appended below the output
#' @param post_output html tags appended after the output
#'
#' @export
#'
#' @importFrom xtable xtable
#' @importFrom xtable print.xtable
#'
#' @examples
#'
#' \dontrun{
#' library(random.cdisc.data)
#'
#' ASL <- radsl(seed = 1)
#'
#' attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1)"
#'
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   root_modules(
#'      tm_data_table(),
#'      tm_variable_browser(),
#'      tm_table("Table Choices", 'ASL', xvar = 'SEX', yvar = 'RACE',
#'               xvar_choices = c('SEX', 'RACE', 'STUDYID'),
#'               yvar_choices = c('RACE', 'SAFFL')),
#'      tm_table("Table No Choices", 'ASL', 'SEX', 'RACE',
#'               pre_output = helpText("Titles"),
#'               post_output = helpText("Footnotes"))
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#' }
tm_table <- function(label,
                     dataname,
                     xvar, yvar,
                     xvar_choices = xvar, yvar_choices = yvar,
                     useNA = c("ifany", "no", "always"),
                     pre_output = NULL, post_output = NULL) {

  args <- as.list(environment())

  args$useNA <- match.arg(useNA)


  module(
    label = label,
    server = srv_table,
    ui = ui_table,
    server_args = list(dataname = dataname),
    ui_args = args,
    filters = dataname
  )

}


ui_table <- function(id, label, dataname, xvar, yvar,
                     xvar_choices, yvar_choices,
                     useNA,
                     pre_output, post_output) {


  ns <- NS(id)


  standard_layout(
    output = tableOutput(ns("table")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(ns("xvar"), "x variable (row)", xvar_choices, xvar, multiple = FALSE),
      optionalSelectInput(ns("yvar"), "y variable (column)", yvar_choices, yvar, multiple = FALSE),
      radioButtons(ns("useNA"), label = "Display Missing Values",
                   choices = c("no", "ifany", "always"), selected = useNA),
      checkboxInput(ns("margins"), "Add margins", value = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )

}


#' @import stats
srv_table <- function(input, output, session, datasets, dataname) {

  output$table <- renderTable({

    ANL <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    xvar <- input$xvar
    yvar <- input$yvar
    useNA <- input$useNA
    add_margins <- input$margins

    validate(need(!is.null(ANL) && is.data.frame(ANL), "no data left"))
    validate(need(nrow(ANL) > 0 , "no observations left"))
    validate(need(xvar, "no valid x variable selected"))
    validate(need(yvar, "no valid y variable selected"))
    validate(need(xvar %in% names(ANL),
                  paste("variable", xvar, " is not available in data", dataname)))
    validate(need(yvar %in% names(ANL),
                  paste("variable", yvar, " is not available in data", dataname)))


    tbl <- table(ANL[[xvar]], ANL[[yvar]], useNA = useNA)

    if (add_margins) tbl <- addmargins(tbl)

    as.data.frame.matrix(tbl, row.names = rownames(tbl))

  }, rownames=TRUE, bordered=TRUE, html.table.attributes = 'style="background-color:white;"')

  observeEvent(input$show_rcode, {

    xvar <- input$xvar
    yvar <- input$yvar
    useNA <- input$useNA
    add_margins <- input$margins

    str_header <- get_rcode_header(
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
        if (add_margins) {
          paste0("with(", dataname, "_FILTERED, addmargins(table(", xvar, ", ", yvar,", useNA = '", useNA, "')))")
        } else {
          paste0("with(", dataname, "_FILTERED, table(", xvar, ", ", yvar,", useNA = '", useNA, "'))")
        },
        "\n"
      ), collapse = ""
    )


    showRCodeModal(
      title = "R Code for the Current Table",
      rcode = code
    )

  })

}


