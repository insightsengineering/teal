#' Validate that dataset has a minimum number of observations
#'
#' @description `r lifecycle::badge("stable")`
#' @param x a data.frame
#' @param min_nrow minimum number of rows in \code{x}
#' @param complete \code{logical} default \code{FALSE} when set to \code{TRUE} then complete cases are checked.
#' @param allow_inf \code{logical} default \code{TRUE} when set to \code{FALSE} then error thrown if any values are
#'   infinite.
#' @param msg (`character(1)`) additional message to display alongside the default message.
#'
#' @details This function is a wrapper for `shiny::validate`.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' ui <- fluidPage(
#'   sliderInput("obs", "Max Age",
#'     min = 0, max = 100, value = 50
#'   ),
#'   plotOutput("plot")
#' )
#'
#' server <- function(input, output) {
#'   output$plot <- renderPlot({
#'     ADSL_f <- ADSL[ADSL$AGE <= input$obs, ]
#'     validate_has_data(ADSL_f, min_nrow = 10, complete = FALSE, msg = "Please adjust your Max Age")
#'
#'     hist(ADSL_f$AGE, breaks = 5)
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
#'
validate_has_data <- function(x,
                              min_nrow = NULL,
                              complete = FALSE,
                              allow_inf = TRUE,
                              msg = NULL) {
  stopifnot(
    "Please provide a character vector in msg argument of validate_has_data." = is.character(msg) || is.null(msg)
  )
  validate(need(!is.null(x) && is.data.frame(x), "No data left."))
  if (!is.null(min_nrow)) {
    if (complete) {
      complete_index <- stats::complete.cases(x)
      validate(need(
        sum(complete_index) > 0 && nrow(x[complete_index, , drop = FALSE]) >= min_nrow,
        paste(c(paste("Number of complete cases is less than:", min_nrow), msg), collapse = "\n")
      ))
    } else {
      validate(need(
        nrow(x) >= min_nrow,
        paste(
          c(paste("Minimum number of records not met: >=", min_nrow, "records required."), msg),
          collapse = "\n"
        )
      ))
    }

    if (!allow_inf) {
      validate(need(
        all(vapply(x, function(col) !is.numeric(col) || !any(is.infinite(col)), logical(1))),
        "Dataframe contains Inf values which is not allowed."
      ))
    }
  }
}

#' Validate that dataset has unique rows for key variables
#'
#' @description `r lifecycle::badge("stable")`
#' @param x a data.frame
#' @param key a vector of ID variables from \code{x} that identify unique records
#'
#' @details This function is a wrapper for `shiny::validate`.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' ui <- fluidPage(
#'   sliderInput("obs", "Max Age",
#'     min = 0, max = 100, value = 50
#'   ),
#'   verbatimTextOutput("age_summary")
#' )
#'
#' server <- function(input, output) {
#'   output$age_summary <- renderText({
#'     ADSL_f <- ADSL[ADSL$AGE <= input$obs, ]
#'     validate_one_row_per_id(ADSL_f, key = c("STUDYID"))
#'
#'     paste0("Mean age :", mean(ADSL_f$AGE))
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
#'
validate_one_row_per_id <- function(x, key = c("USUBJID", "STUDYID")) {
  validate(need(!any(duplicated(x[key])), paste("Found more than one row per id.")))
}

#' Validates that vector includes all expected values
#'
#' @description `r lifecycle::badge("stable")`
#' @param x values to test. All must be in \code{choices}
#' @param choices a vector to test for values of \code{x}
#' @param msg warning message to display
#'
#' @details This function is a wrapper for `shiny::validate`.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADRS <- synthetic_cdisc_data("latest")$adrs
#'
#' ui <- fluidPage(
#'   selectInput(
#'     "rsp",
#'     "Select response parameter",
#'     choices = c("BESRSPI", "INVET", "CBRSPI"),
#'     selected = "BESRSPI",
#'     multiple = FALSE
#'   ),
#'   verbatimTextOutput("rsp_summary")
#' )
#'
#' server <- function(input, output) {
#'   output$rsp_summary <- renderPrint({
#'     validate_in(input$rsp, ADRS$PARAMCD, "Parameter does not exist.")
#'     nrow(ADRS[ADRS$PARAMCD == input$rsp, ])
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
#'
validate_in <- function(x, choices, msg) {
  validate(need(length(x) > 0 && length(choices) > 0 && all(x %in% choices), msg))
}

#' Validates that vector has length greater than 0
#'
#' @description `r lifecycle::badge("stable")`
#' @param x vector
#' @param msg message to display
#'
#' @details This function is a wrapper for `shiny::validate`.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' ui <- fluidPage(
#'   selectInput("ref_arm", "Select reference treatment",
#'     choices = c("ARM A", "ARM B", "ARM X"), selected = "ARM A"
#'   ),
#'   selectInput("comp_arm", "Select comparison treatment",
#'     choices = c("ARM C", "ARM Y", "ARM Z"), selected = "ARM C"
#'   ),
#'   verbatimTextOutput("arm_summary")
#' )
#'
#' server <- function(input, output) {
#'   output$arm_summary <- renderText({
#'     ref_arm <- ADSL$ARMCD[input$ref_arm == ADSL$ARMCD]
#'     comp_arm <- ADSL$ARMCD[input$comp_arm == ADSL$ARMCD]
#'
#'     validate_has_elements(ref_arm, "Need reference treatment.")
#'     validate_has_elements(comp_arm, "Need comparison treatment.")
#'
#'     paste0(
#'       "Number of patients in: reference treatment=",
#'       length(ref_arm), " comparions treatment=", length(comp_arm)
#'     )
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
validate_has_elements <- function(x, msg) {
  validate(need(length(x) > 0, msg))
}

#' Validates no intersection between two vectors
#'
#' @description `r lifecycle::badge("stable")`
#' @param x vector
#' @param y vector
#' @param msg message to display if \code{x} and \code{y} intersect
#'
#' @details This function is a wrapper for `shiny::validate`.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' ui <- fluidPage(
#'   selectInput("ref_arm", "Select reference treatment",
#'     choices = c("ARM A", "ARM B", "ARM C"),
#'     selected = "ARM A"
#'   ),
#'   selectInput("comp_arm", "Select comparison treatment",
#'     choices = c("ARM A", "ARM B", "ARM C"),
#'     selected = "ARM C"
#'   ),
#'   verbatimTextOutput("arm_summary")
#' )
#'
#' server <- function(input, output) {
#'   output$arm_summary <- renderText({
#'     ref_arm <- ADSL$ARMCD[input$ref_arm == ADSL$ARMCD]
#'     comp_arm <- ADSL$ARMCD[input$comp_arm == ADSL$ARMCD]
#'
#'     validate_no_intersection(
#'       comp_arm, ref_arm,
#'       "reference and comparison treatments cannot overlap"
#'     )
#'     paste0(
#'       "Number of patients in: reference treatment=", length(ref_arm),
#'       " comparions treatment=", length(comp_arm)
#'     )
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
#'
validate_no_intersection <- function(x, y, msg) {
  validate(need(length(intersect(x, y)) == 0, msg))
}


#' Validates that dataset contains specific variable
#'
#' @description `r lifecycle::badge("stable")`
#' @param data a data.frame
#' @param varname name of variable in \code{data}
#' @param msg message to display if \code{data} does not include \code{varname}
#'
#' @details This function is a wrapper for `shiny::validate`.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' ui <- fluidPage(
#'   selectInput("arm", "Select treatment",
#'     choices = c("ARM", "ARMCD", "ACTARM", "TRT"),
#'     selected = "ARM", multiple = TRUE
#'   ),
#'   verbatimTextOutput("arm_summary")
#' )
#'
#' server <- function(input, output) {
#'   output$arm_summary <- renderText({
#'     validate_has_variable(ADSL, input$arm)
#'
#'     paste0("Selected treatment variables: ", paste(input$arm, collapse = ", "))
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
validate_has_variable <- function(data, varname, msg) {
  if (length(varname) != 0) {
    has_vars <- all(varname %in% names(data))
    has_all <- all(has_vars)

    if (!has_all) {
      if (missing(msg)) {
        dataname <- deparse(substitute(data))
        msg <- paste(
          dataname, "does not have the required variables:",
          paste(varname[!has_vars], collapse = ", "),
          "."
        )
      }
      validate(need(FALSE, msg))
    }
  }
}

#' Validate that variables has expected number of levels
#'
#' @description `r lifecycle::badge("stable")`
#' @param x variable name. If \code{x} is not a factor, the unique values
#'   are treated as levels.
#' @param min_levels cutoff for minimum number of levels of \code{x}
#' @param max_levels cutoff for maximum number of levels of \code{x}
#' @param var_name name of variable being validated for use in
#'   validation message
#'
#' @details If the number of levels of \code{x} is less than \code{min_levels}
#'   or greater than \code{max_levels} the validation will fail.
#'   This function is a wrapper for `shiny::validate`.
#'
#' @export
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' ui <- fluidPage(
#'   selectInput("arm", "Select treatment",
#'     choices = c("ARM", "ARMCD", "STUDYID"), selected = "ARM"
#'   ),
#'   verbatimTextOutput("arm_summary")
#' )
#'
#' server <- function(input, output) {
#'   output$arm_summary <- renderText({
#'     validate_n_levels(ADSL[[input$arm]],
#'       min_levels = 2, max_levels = 15,
#'       var_name = input$arm
#'     )
#'     paste0(
#'       "Levels of selected treatment variable: ",
#'       paste(levels(ADSL[[input$arm]]),
#'         collapse = ", "
#'       )
#'     )
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
validate_n_levels <- function(x, min_levels = 1, max_levels = 12, var_name) {
  x_levels <- if (is.factor(x)) {
    levels(x)
  } else {
    unique(x)
  }

  if (!is.null(min_levels) && !(is.null(max_levels))) {
    validate(need(
      length(x_levels) >= min_levels && length(x_levels) <= max_levels,
      sprintf(
        "%s variable needs minimum %s level(s) and maximum %s level(s).",
        var_name, min_levels, max_levels
      )
    ))
  } else if (!is.null(min_levels)) {
    validate(need(
      length(x_levels) >= min_levels,
      sprintf("%s variable needs minimum %s levels(s)", var_name, min_levels)
    ))
  } else if (!is.null(max_levels)) {
    validate(need(
      length(x_levels) <= max_levels,
      sprintf("%s variable needs maximum %s level(s)", var_name, max_levels)
    ))
  }
}
