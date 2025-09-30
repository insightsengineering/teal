#' @title `TealReportCard`
#' @description `r lifecycle::badge("experimental")`
#' Child class of [`teal.reporter::ReportCard`] that is used for `teal` specific applications.
#' In addition to the parent methods, it supports rendering `teal` specific elements such as
#' the source code, the encodings panel content and the filter panel content as part of the
#' meta data.
#' @export
#' @importFrom methods as
#'
TealReportCard <- R6::R6Class( # nolint: object_name.
  classname = "TealReportCard",
  inherit = teal.reporter::ReportCard,
  public = list(
    #' @description Appends the source code to the `content` meta data of this `TealReportCard`.
    #'
    #' @param src (`character(1)`) code as text.
    #' @param ... any `rmarkdown` `R` chunk parameter and its value.
    #' But `eval` parameter is always set to `FALSE`.
    #' @return Object of class `TealReportCard`, invisibly.
    #' @examples
    #' card <- TealReportCard$new()$append_src(
    #'   "plot(iris)"
    #' )
    #' card$get_content()[[1]]
    append_src = function(src, ...) {
      super$append_rcode(text = src, ...)
    },
    #' @description Appends the filter state list to the `content` and `metadata` of this `TealReportCard`.
    #'  If the filter state list has an attribute named `formatted`, it appends it to the card otherwise it uses
    #'  the default `yaml::as.yaml` to format the list.
    #'  If the filter state list is empty, nothing is appended to the `content`.
    #'
    #' @param fs (`teal_slices`) object returned from [teal_slices()] function.
    #' @return `self`, invisibly.
    append_fs = function(fs) {
      checkmate::assert_class(fs, "teal_slices")
      if (length(fs) > 0) {
        self$append_content(teal.reporter::code_chunk(.teal_slice_to_yaml(fs), eval = FALSE, lang = "verbatim"))
      }
      invisible(self)
    },
    #' @description Appends the encodings list to the `content` and `metadata` of this `TealReportCard`.
    #'
    #' @param encodings (`list`) list of encodings selections of the `teal` app.
    #' @return `self`, invisibly.
    #' @examples
    #' card <- TealReportCard$new()$append_encodings(list(variable1 = "X"))
    #' card$get_content()[[1]]
    #'
    append_encodings = function(encodings) {
      checkmate::assert_list(encodings)
      self$append_text("Selected Options", "header3")
      if (requireNamespace("yaml", quietly = TRUE)) {
        self$append_text(yaml::as.yaml(encodings, handlers = list(
          POSIXct = function(x) format(x, "%Y-%m-%d"),
          POSIXlt = function(x) format(x, "%Y-%m-%d"),
          Date = function(x) format(x, "%Y-%m-%d")
        )), "verbatim")
      } else {
        stop("yaml package is required to format the encodings list")
      }
      self$append_metadata("Encodings", encodings)
      invisible(self)
    }
  )
)

.teal_slice_to_yaml <- function(fs) {
  checkmate::assert_class(fs, "teal_slices")
  states_list <- lapply(fs, function(x) {
    x_list <- shiny::isolate(as.list(x))
    if (
      inherits(x_list$choices, c("integer", "numeric", "Date", "POSIXct", "POSIXlt")) &&
        length(x_list$choices) == 2 &&
        length(x_list$selected) == 2
    ) {
      x_list$range <- paste(x_list$selected, collapse = " - ")
      x_list["selected"] <- NULL
    }
    if (!is.null(x_list$arg)) {
      x_list$arg <- if (x_list$arg == "subset") "Genes" else "Samples"
    }

    x_list <- x_list[
      c("dataname", "varname", "experiment", "arg", "expr", "selected", "range", "keep_na", "keep_inf")
    ]
    names(x_list) <- c(
      "Dataset name", "Variable name", "Experiment", "Filtering by", "Applied expression",
      "Selected Values", "Selected range", "Include NA values", "Include Inf values"
    )

    Filter(Negate(is.null), x_list)
  })

  if (requireNamespace("yaml", quietly = TRUE)) {
    yaml::as.yaml(states_list)
  } else {
    stop("yaml package is required to format the filter state list")
  }
}

#' @noRd
ui_add_reporter <- function(id) {
  ns <- shiny::NS(id)
  bslib::tooltip(
    id = ns("reporter_tooltip"),
    trigger = shinyjs::disabled(
      shiny::tags$div(
        id = ns("report_add_wrapper"),
        shiny::uiOutput(ns("report_add_body"))
      )
    ),
    class = "teal add-reporter-container",
    shiny::textOutput(ns("report_add_reason"))
  )
}

#' @noRd
srv_add_reporter <- function(id, module_out, reporter) {
  if (is.null(reporter)) {
    return(FALSE)
  } # early exit
  moduleServer(id, function(input, output, session) {
    mod_out_r <- reactive({
      if (!is.null(module_out) && is.reactive(module_out)) {
        tryCatch(module_out(), error = function(e) e)
      }
    })

    doc_out <- reactive({
      teal_data_handled <- mod_out_r()
      tcard <- if (inherits(teal_data_handled, "teal_report")) {
        teal.reporter::teal_card(teal_data_handled)
      } else if (inherits(teal_data_handled, "teal_data")) {
        teal.reporter::teal_card(as(teal_data_handled, "teal_report"))
      } else if (inherits(teal_data_handled, "teal_card")) {
        teal_data_handled
      }

      if (length(tcard)) .collapse_subsequent_chunks(tcard)
    })

    reason_r <- reactive({
      if (is.null(mod_out_r())) {
        "No report content available from this module."
      } else if (inherits(mod_out_r(), "error")) {
        "The module returned an error, check it for errors."
      } else if (is.null(doc_out())) {
        "The module does not support reporter functionality."
      } else if (!inherits(doc_out(), "teal_card")) {
        "Report content not in a valid format, check the module for errors."
      } else if (isFALSE(attr(mod_out_r(), "teal.enable_report"))) {
        "The report functionality is disabled for this module."
      } else {

      }
    })

    if (!is.null(reporter)) {
      output$report_add_body <- shiny::renderUI({
        teal.reporter::add_card_button_ui(session$ns("reporter_add"), label = "Add to Report")
      })

      output$report_add_reason <- shiny::renderText({
        trimws(reason_r()) %||% "Click here to add this module's output to the report."
      })

      observeEvent(reason_r(), ignoreNULL = FALSE, {
        shinyjs::toggleState("report_add_wrapper", condition = is.null(reason_r()))
      })

      teal.reporter::add_card_button_srv("reporter_add", reporter = reporter, card_fun = doc_out)
    }
  })
}

#' Disable the report for a `teal_module`
#'
#' Convenience function that disables the user's ability to add the module
#' to the report previewer.
#' @param x (`teal_module`) a `teal_module` object.
#' @return modified data object that indicates that it should disable the reporter functionality.
#' @export
#' @examples
#' app <- init(
#'   data = within(teal_data(), iris <- iris),
#'   modules = modules(
#'     example_module(label = "example teal module") |> disable_report()
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
disable_report <- function(x) {
  checkmate::assert_multi_class(x, c("teal_module", "teal_modules"))
  after(x, server = function(data) {
    attr(data, "teal.enable_report") <- FALSE
    data
  })
}
