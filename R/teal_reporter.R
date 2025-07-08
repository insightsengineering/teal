#' @title `TealReportCard`
#' @description `r lifecycle::badge("experimental")`
#' Child class of [`teal.reporter::ReportCard`] that is used for `teal` specific applications.
#' In addition to the parent methods, it supports rendering `teal` specific elements such as
#' the source code, the encodings panel content and the filter panel content as part of the
#' meta data.
#' @export
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
    #' card$get_content()[[1]]$get_content()
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
        self$append_content(code_chunk(.teal_slice_to_yaml(fs), eval = FALSE, lang = "verbatim"))
      }
      invisible(self)
    },
    #' @description Appends the encodings list to the `content` and `metadata` of this `TealReportCard`.
    #'
    #' @param encodings (`list`) list of encodings selections of the `teal` app.
    #' @return `self`, invisibly.
    #' @examples
    #' card <- TealReportCard$new()$append_encodings(list(variable1 = "X"))
    #' card$get_content()[[1]]$get_content()
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
  ),
  private = list(
    dispatch_block = function(block_class) {
      if (exists(block_class, getNamespace("teal"))) {
        # for block classes which are in teal (TealSlicesBlock)
        get(block_class)
      } else {
        # other block classes are in teal.reporter so we need to use super (ReporterCard) class
        super$dispatch_block(block_class)
      }
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
ui_add_reporter <- function(id) uiOutput(NS(id, "reporter_add_container"))

#' @noRd
srv_add_reporter <- function(id, module_out, reporter) {
  if (is.null(reporter)) {
    return(FALSE)
  } # early exit
  moduleServer(id, function(input, output, session) {
    mod_out_r <- reactive({
      req(module_out)
      if (is.reactive(module_out)) {
        module_out()
      }
    })

    doc_out <- reactive({
      req(mod_out_r())
      teal_data_handled <- tryCatch(mod_out_r(), error = function(e) e)
      tcard <- if (inherits(teal_data_handled, "teal_report")) {
        teal.reporter::teal_card(teal_data_handled)
      } else if (inherits(teal_data_handled, "teal_data")) {
        teal.reporter::teal_card(teal.reporter::as.teal_report(teal_data_handled))
      } else if (inherits(teal_data_handled, "teal_card")) {
        teal_data_handled
      }

      if (length(tcard)) .collapse_subsequent_chunks(tcard)
    })

    .call_once_when(!is.null(doc_out()), {
      output$reporter_add_container <- renderUI({
        tags$div(
          class = "teal add-reporter-container",
          teal.reporter::add_card_button_ui(session$ns("reporter_add"))
        )
      })
      teal.reporter::add_card_button_srv("reporter_add", reporter = reporter, card_fun = doc_out)
    })



    observeEvent(doc_out(), ignoreNULL = FALSE, {
      shinyjs::toggleState("reporter_add_container", condition = inherits(doc_out(), "teal_card"))
    })
  })
}

#' Disable the report for a `teal_module`
#'
#' Convenience function that disables the user's ability to add the module
#' to the report previewer.
#' @param x (`teal_module`) a `teal_module` object.
#' @return `NULL` that indicates that it should disable the reporter functionality.
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
  checkmate::assert_class(x, "teal_module")
  after(x, server = function(data) {
    teal.reporter::teal_card(data) <- teal.reporter::teal_card()
    NULL
  })
}
