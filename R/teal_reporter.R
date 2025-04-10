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
      checkmate::assert_character(src, min.len = 0, max.len = 1)
      params <- list(...)
      params$eval <- FALSE
      rblock <- RcodeBlock$new(src)
      rblock$set_params(params)
      self$append_content(rblock)
      self$append_metadata("SRC", src)
      invisible(self)
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
      self$append_text("Filter State", "header3")
      if (length(fs)) {
        self$append_content(TealSlicesBlock$new(fs))
      } else {
        self$append_text("No filters specified.")
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

#' @title `TealSlicesBlock`
#' @docType class
#' @description
#' Specialized `TealSlicesBlock` block for managing filter panel content in reports.
#' @keywords internal
TealSlicesBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "TealSlicesBlock",
  inherit = teal.reporter:::TextBlock,
  public = list(
    #' @description Returns a `TealSlicesBlock` object.
    #'
    #' @details Returns a `TealSlicesBlock` object with no content and no parameters.
    #'
    #' @param content (`teal_slices`) object returned from [teal_slices()] function.
    #' @param style (`character(1)`) string specifying style to apply.
    #'
    #' @return Object of class `TealSlicesBlock`, invisibly.
    #'
    initialize = function(content = teal_slices(), style = "verbatim") {
      self$set_content(content)
      self$set_style(style)
      invisible(self)
    },

    #' @description Sets content of this `TealSlicesBlock`.
    #' Sets content as `YAML` text which represents a list generated from `teal_slices`.
    #' The list displays limited number of fields from `teal_slice` objects, but this list is
    #' sufficient to conclude which filters were applied.
    #' When `selected` field in `teal_slice` object is a range, then it is displayed as a "min"
    #'
    #'
    #' @param content (`teal_slices`) object returned from [teal_slices()] function.
    #' @return `self`, invisibly.
    set_content = function(content) {
      checkmate::assert_class(content, "teal_slices")
      if (length(content) != 0) {
        states_list <- lapply(content, function(x) {
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
          super$set_content(yaml::as.yaml(states_list))
        } else {
          stop("yaml package is required to format the filter state list")
        }
      }
      private$teal_slices <- content
      invisible(self)
    },
    #' @description Create the `TealSlicesBlock` from a list.
    #'
    #' @param x (`named list`) with two fields `text` and `style`.
    #' Use the `get_available_styles` method to get all possible styles.
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' TealSlicesBlock <- getFromNamespace("TealSlicesBlock", "teal")
    #' block <- TealSlicesBlock$new()
    #' block$from_list(list(text = "sth", style = "default"))
    #'
    from_list = function(x) {
      checkmate::assert_list(x)
      checkmate::assert_names(names(x), must.include = c("text", "style"))
      super$set_content(x$text)
      super$set_style(x$style)
      invisible(self)
    },
    #' @description Convert the `TealSlicesBlock` to a list.
    #'
    #' @return `named list` with a text and style.
    #' @examples
    #' TealSlicesBlock <- getFromNamespace("TealSlicesBlock", "teal")
    #' block <- TealSlicesBlock$new()
    #' block$to_list()
    #'
    to_list = function() {
      content <- self$get_content()
      list(
        text = if (length(content)) content else "",
        style = self$get_style()
      )
    }
  ),
  private = list(
    style = "verbatim",
    teal_slices = NULL # teal_slices
  )
)

#' Server function for `ReportDocument` card class.
#' @keywords internal
add_document_button_srv <- function(id, reporter, r_card_fun) {
  checkmate::assert_class(r_card_fun, "reactive")
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c(
      "add_report_card_button", "download_button", "reset_reporter",
      "add_card_ok", "download_data", "reset_reporter_ok",
      "label", "comment"
    ))

    ns <- session$ns

    add_modal <- function() {
      div(
        class = "teal-widgets reporter-modal",
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Add a Card to the Report"),
          shiny::tags$hr(),
          shiny::textInput(
            ns("label"),
            "Card Name",
            value = "",
            placeholder = "Add the card title here",
            width = "100%"
          ),
          shiny::tags$script(
            shiny::HTML(
              sprintf(
                "
                $('#shiny-modal').on('shown.bs.modal', () => {
                  $('#%s').focus()
                })
                ",
                ns("label")
              )
            )
          ),
          footer = shiny::div(
            shiny::tags$button(
              type = "button",
              class = "btn btn-secondary",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            shiny::tags$button(
              id = ns("add_card_ok"),
              type = "button",
              class = "btn btn-primary action-button",
              `data-val` = shiny::restoreInput(id = ns("add_card_ok"), default = NULL),
              NULL,
              "Add Card"
            )
          )
        )
      )
    }

    shiny::observeEvent(input$add_report_card_button, {
      shiny::showModal(add_modal())
    })

    # the add card button is disabled when clicked to prevent multi-clicks
    # please check the ui part for more information
    shiny::observeEvent(input$add_card_ok, {
      if (inherits(r_card_fun, "try-error")) {
        msg <- paste0(
          "The card could not be added to the report. ",
          "Have the outputs for the report been created yet? If not please try again when they ",
          "are ready. Otherwise contact your application developer"
        )
        warning(msg)
        shiny::showNotification(
          msg,
          type = "error"
        )
      } else {

        card <- r_card_fun()
        #card <- to_markdown(card)
        lcard <- list(card)
        names(lcard) <- input$label

        reporter$append_cards(lcard)

        shiny::showNotification(sprintf("The card added successfully."), type = "message")
        shiny::removeModal()
      }
    })
  })
}

