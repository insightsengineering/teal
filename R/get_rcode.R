#' Returns R Code from a teal module
#'
#' @description `r lifecycle::badge("deprecated")`
#' Return the R-code used to create a teal::teal] module analysis. This function
#' will return all analysis code as a character string. In case of a good setup it will
#' not only return the code used create the module analysis, but also the code used by
#' the app author to create the app. The main feature of this function is encapsulating
#' the R code to merge datasets by [teal.transform::merge_datasets()] and all the R code stored inside
#' code [teal.code::chunks].
#'
#' @param datasets (`list`) list of `FilteredData` available inside the
#'  server function of any [teal::teal] module.
#' @param datanames (`character`)\cr
#'  names of datasets which code should be returned for. Due to fact that
#'  `teal` filter panel depending on `"ADSL"`, code for `ADSL`
#'  is always returned even if not specified.
#' @param chunks (`chunks`) \cr
#'  object of class `chunks` that stores code chunks. These code
#'  chunks are used in [teal::teal] to enable reproducibility. Normally these chunks
#'  are stored within the [shiny::shiny-package] session. The default value
#'  can normally be used.
#' @param selected_chunk_ids (`character` vector)\cr
#'   vector of code chunks to be shown
#'   in the code. If chunk id's are available this can be used to limit the
#'   chunks that appear in the `"Show R-Code"` modal. Please only use this
#'   feature if all chunks were set with designated IDs.
#'
#' @param session (`environment`) deprecated.
#'
#' @inheritParams get_rcode_header
#'
#' @note
#'  The `teal.load_nest_code` option is being used to customize the code that sets correct library paths
#'  with all packages available. If empty (the default), a placeholder string is being used.
#'
#' @export
#'
#' @return Return the R Code needed to reproduce a teal module. The [get_rcode_header()] part allows
#'    to install the module. Additionally if the user filtered data by
#'    teal inherited functions, the code to filter the data is included. If the teal module
#'    is using [teal.transform::data_extract_srv()] the extraction and merging
#'    code will be returned, too.
#'    If code chunks were used, these will also be used to derive module R Code.
#'
#' @examples
#' \dontrun{
#' show_rcode_modal(
#'   title = "R Code for a Regression Plot",
#'   rcode = get_rcode(
#'     datasets = datasets,
#'     title = title,
#'     description = description
#'   )
#' )
#' }
#' @references [show_rcode_modal()], [get_rcode_header()]
get_rcode <- function(datasets = NULL,
                      datanames = `if`(is.null(datasets), datasets, datasets$datanames()),
                      chunks = teal.code::get_chunks_object(),
                      selected_chunk_ids = character(0),
                      session = NULL,
                      title = NULL,
                      description = NULL) {
  checkmate::assert_class(datasets, "FilteredData", null.ok = TRUE)

  lifecycle::deprecate_warn(
    when = "0.12.1",
    what = "get_rcode()",
    details = "Reproducibility in teal apps has changed.
      See the teal.code package and example modules for further details"
  )

  if (!inherits(chunks, "chunks")) {
    stop("No code chunks given")
  }
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(description, null.ok = TRUE)
  rlang::push_options(width = 120)

  if (!is.null(session)) {
    lifecycle::deprecate_warn("0.12.1", "get_rcode(session)")
  }

  if (!is.null(datasets)) {
    if (inherits(datasets, "CDISCFilteredData")) {
      datanames <- unique(c(datanames, unlist(lapply(datanames, datasets$get_parentname))))
    }
    str_header <- paste(
      c(get_rcode_header(title = title, description = description), ""),
      collapse = "\n"
    )
    str_install <- paste(c(get_rcode_str_install(), ""), collapse = "\n")
    str_libs <- paste(get_rcode_libraries(), "\n")

    hashes <- calculate_hashes(datanames, datasets)
    str_code <- c(get_datasets_code(datanames, datasets, hashes), teal.slice::get_filter_expr(datasets, datanames))
  } else {
    str_header <- get_rcode_header(title = title, description = description)
    str_install <- character(0)
    str_libs <- character(0)
    str_code <- character(0)
  }
  str_chunks <- paste0(
    chunks$get_rcode(chunk_ids = selected_chunk_ids),
    collapse = "\n"
  )

  code_not_to_style <- paste(
    c(
      "\n",
      str_header,
      str_install,
      str_libs
    ),
    collapse = "\n"
  )


  code_to_style <- paste(
    c(
      str_code,
      "",
      str_chunks,
      "\n"
    ),
    collapse = "\n"
  )

  # remove error with curly brace
  code_to_style <- gsub("}\n\\s*else", "} else", code_to_style)
  code_to_style <- paste0(styler::style_text(code_to_style), collapse = "\n")
  paste(code_not_to_style, code_to_style, sep = "\n")
}


#' Get datasets code
#'
#' Get combined code from `FilteredData` and from `CodeClass` object.
#'
#' @param datanames (`character`) names of datasets to extract code from
#' @param datasets (`FilteredData`) object
#' @param hashes named (`list`) of hashes per dataset
#'
#' @return `character(3)` containing following elements:
#'  - code from `CodeClass` (data loading code)
#'  - hash check of loaded objects
#'
#' @keywords internal
get_datasets_code <- function(datanames, datasets, hashes) {
  str_code <- datasets$get_code(datanames)
  if (length(str_code) == 0 || (length(str_code) == 1 && str_code == "")) {
    str_code <- "message('Preprocessing is empty')"
  } else if (length(str_code) > 0) {
    str_code <- paste0(str_code, "\n\n")
  }

  if (!datasets$get_check()) {
    check_note_string <- paste0(
      c(
        "message(paste(\"Reproducibility of data import and preprocessing was not explicitly checked\",",
        "   \" ('check = FALSE' is set). Contact app developer if this is an issue.\n\"))"
      ),
      collapse = "\n"
    )
    str_code <- paste0(str_code, "\n\n", check_note_string, "\n\n")
  }

  str_hash <- paste(
    paste0(
      vapply(
        datanames,
        function(dataname) {
          sprintf(
            "stopifnot(%s == %s)",
            deparse1(bquote(rlang::hash(.(as.name(dataname))))),
            deparse1(hashes[[dataname]])
          )
        },
        character(1)
      ),
      collapse = "\n"
    ),
    "\n\n"
  )

  c(str_code, str_hash)
}

## Module ----
#' Server part of get R code module
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' @inheritParams get_rcode
#' @inheritParams shiny::moduleServer
#'
#' @param modal_title optional, (`character`) title of the modal
#' @param code_header optional, (`character`) header inside R
#' @param disable_buttons optional, (`reactive`)
#' a shiny reactive value. Should be a single boolean value, indicating whether to disable
#' or enable the show R code and Debug info buttons. Default: `reactiveVal(FALSE)`.
#'
#' @export
#'
get_rcode_srv <- function(id,
                          datasets,
                          datanames = datasets$datanames(),
                          modal_title = "R Code",
                          code_header = "Automatically generated R code",
                          disable_buttons = reactiveVal(FALSE)) {
  checkmate::assert_class(disable_buttons, c("reactive", "function"))

  lifecycle::deprecate_warn(
    when = "0.12.1",
    what = "get_rcode_srv()",
    with = "teal.widgets::verbatim_popup_srv()",
    details = "Show R Code behaviour has changed,
      see example modules in vignettes for more details"
  )

  moduleServer(id, function(input, output, server) {
    chunks <- teal.code::get_chunks_object(parent_idx = 1L)
    observeEvent(input$show_rcode, {
      progress <- Progress$new()
      progress$set(message = "Getting R Code", value = 0)
      show_rcode_modal(
        title = modal_title,
        rcode = get_rcode(
          datasets = datasets,
          datanames = datanames,
          chunks = chunks,
          title = code_header
        )
      )
      progress$set(message = "Getting R Code", value = 1)
      progress$close()
    })

    teal.code::get_eval_details_srv(
      id = "show_eval_details",
      chunks = chunks
    )

    observeEvent(disable_buttons(), {
      if (disable_buttons()) {
        shinyjs::disable("show_rcode")
        shinyjs::disable("show_eval_details-evaluation_details")
      } else {
        shinyjs::enable("show_rcode")
        shinyjs::enable("show_eval_details-evaluation_details")
      }
    })
  })
}

#' Ui part of get R code module
#'
#' @description `r lifecycle::badge("deprecated")`
#' @param id (`character`) id of shiny module
#'
#' @return (`shiny.tag`)
#'
#' @export
get_rcode_ui <- function(id) {
  lifecycle::deprecate_warn(
    when = "0.12.1",
    what = "get_rcode_ui()",
    with = "teal.widgets::verbatim_popup_ui()",
    details = "Show R Code behaviour has changed,
      see example modules in vignettes for more details"
  )

  ns <- NS(id)
  tagList(
    tags$div(actionButton(ns("show_rcode"), "Show R code", width = "100%")),
    tags$div(teal.code::get_eval_details_ui(ns("show_eval_details")))
  )
}
