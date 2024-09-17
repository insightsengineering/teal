#' Get client timezone
#'
#' User timezone in the browser may be different to the one on the server.
#' This script can be run to register a `shiny` input which contains information about the timezone in the browser.
#'
#' @param ns (`function`) namespace function passed from the `session` object in the `shiny` server.
#'  For `shiny` modules this will allow for proper name spacing of the registered input.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#'
get_client_timezone <- function(ns) {
  script <- sprintf(
    "Shiny.setInputValue(`%s`, Intl.DateTimeFormat().resolvedOptions().timeZone)",
    ns("timezone")
  )
  shinyjs::runjs(script) # function does not return anything
  invisible(NULL)
}

#' Resolve the expected bootstrap theme
#' @noRd
#' @keywords internal
get_teal_bs_theme <- function() {
  bs_theme <- getOption("teal.bs_theme")

  if (is.null(bs_theme)) {
    bs_theme <- bslib::bs_theme()
  }

  if (!checkmate::test_class(bs_theme, "bs_theme")) {
    warning(
      "Assertion on 'teal.bs_theme' option value failed: ",
      checkmate::check_class(bs_theme, "bs_theme"),
      ". The default bslib Bootstrap theme will be used."
    )
    bs_theme <- bslib::bs_theme()
  }

  bs_theme
}

#' Return parentnames along with datanames.
#' @noRd
#' @keywords internal
include_parent_datanames <- function(dataname, join_keys) {
  ordered_datanames <- dataname
  for (i in dataname) {
    parents <- character(0)
    while (length(i) > 0) {
      parent_i <- teal.data::parent(join_keys, i)
      parents <- c(parent_i, parents)
      i <- parent_i
    }
    ordered_datanames <- c(parents, dataname, ordered_datanames)
  }
  unique(ordered_datanames)
}

#' Create a `FilteredData`
#'
#' Create a `FilteredData` object from a `teal_data` object.
#'
#' @param x (`teal_data`) object
#' @param datanames (`character`) vector of data set names to include; must be subset of `datanames(x)`
#' @return A `FilteredData` object.
#' @keywords internal
teal_data_to_filtered_data <- function(x, datanames = .teal_data_ls(x)) {
  checkmate::assert_class(x, "teal_data")
  checkmate::assert_character(datanames, min.chars = 1L, any.missing = FALSE)
  # Otherwise, FilteredData will be created in the modules' scope later
  teal.slice::init_filtered_data(
    x = Filter(
      length,
      sapply(datanames, function(dn) x[[dn]], simplify = FALSE)
    ),
    join_keys = teal.data::join_keys(x)
  )
}

#' Template function for `TealReportCard` creation and customization
#'
#' This function generates a report card with a title,
#' an optional description, and the option to append the filter state list.
#'
#' @param title (`character(1)`) title of the card (unless overwritten by label)
#' @param label (`character(1)`) label provided by the user when adding the card
#' @param description (`character(1)`) optional, additional description
#' @param with_filter (`logical(1)`) flag indicating to add filter state
#' @param filter_panel_api (`FilterPanelAPI`) object with API that allows the generation
#' of the filter state in the report
#'
#' @return (`TealReportCard`) populated with a title, description and filter state.
#'
#' @export
report_card_template <- function(title, label, description = NULL, with_filter, filter_panel_api) {
  checkmate::assert_string(title)
  checkmate::assert_string(label)
  checkmate::assert_string(description, null.ok = TRUE)
  checkmate::assert_flag(with_filter)
  checkmate::assert_class(filter_panel_api, classes = "FilterPanelAPI")

  card <- teal::TealReportCard$new()
  title <- if (label == "") title else label
  card$set_name(title)
  card$append_text(title, "header2")
  if (!is.null(description)) card$append_text(description, "header3")
  if (with_filter) card$append_fs(filter_panel_api$get_filter_state())
  card
}


#' Check `datanames` in modules
#'
#' This function ensures specified `datanames` in modules match those in the data object,
#' returning error messages or `TRUE` for successful validation.
#'
#' @param modules (`teal_modules`) object
#' @param datanames (`character`) names of datasets available in the `data` object
#'
#' @return A `character(1)` containing error message or `TRUE` if validation passes.
#' @keywords internal
check_modules_datanames <- function(modules, datanames) {
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  checkmate::assert_character(datanames)

  recursive_check_datanames <- function(modules, datanames) {
    # check teal_modules against datanames
    if (inherits(modules, "teal_modules")) {
      result <- lapply(modules$children, function(module) recursive_check_datanames(module, datanames = datanames))
      result <- result[vapply(result, Negate(is.null), logical(1L))]
      list(
        string = do.call(c, as.list(unname(sapply(result, function(x) x$string)))),
        html = function(with_module_name = TRUE) {
          tagList(
            lapply(
              result,
              function(x) x$html(with_module_name = with_module_name)
            )
          )
        }
      )
    } else {
      extra_datanames <- setdiff(modules$datanames, c("all", datanames))
      if (length(extra_datanames)) {
        list(
          string = build_datanames_error_message(
            modules$label,
            datanames,
            extra_datanames,
            tags = list(
              span = function(..., .noWS = NULL) { # nolint: object_name
                trimws(paste(..., sep = ifelse(is.null(.noWS), " ", ""), collapse = " "))
              },
              code = function(x) toString(dQuote(x, q = FALSE))
            ),
            tagList = function(...) trimws(paste(...))
          ),
          # Build HTML representation of the error message with <pre> formatting
          html = function(with_module_name = TRUE) {
            tagList(
              build_datanames_error_message(
                if (with_module_name) modules$label,
                datanames,
                extra_datanames
              ),
              tags$br(.noWS = "before")
            )
          }
        )
      }
    }
  }
  check_datanames <- recursive_check_datanames(modules, datanames)
  if (length(check_datanames)) {
    check_datanames
  } else {
    TRUE
  }
}

#' Check `datanames` in filters
#'
#' This function checks whether `datanames` in filters correspond to those in `data`,
#' returning character vector with error messages or `TRUE` if all checks pass.
#'
#' @param filters (`teal_slices`) object
#' @param datanames (`character`) names of datasets available in the `data` object
#'
#' @return A `character(1)` containing error message or TRUE if validation passes.
#' @keywords internal
check_filter_datanames <- function(filters, datanames) {
  checkmate::assert_class(filters, "teal_slices")
  checkmate::assert_character(datanames)

  # check teal_slices against datanames
  out <- unlist(sapply(
    filters, function(filter) {
      dataname <- shiny::isolate(filter$dataname)
      if (!dataname %in% datanames) {
        sprintf(
          "- Filter '%s' refers to dataname not available in 'data':\n %s not in (%s)",
          shiny::isolate(filter$id),
          dQuote(dataname, q = FALSE),
          toString(dQuote(datanames, q = FALSE))
        )
      }
    }
  ))


  if (length(out)) {
    paste(out, collapse = "\n")
  } else {
    TRUE
  }
}

#' Function for validating the title parameter of `teal::init`
#'
#' Checks if the input of the title from `teal::init` will create a valid title and favicon tag.
#' @param shiny_tag (`shiny.tag`) Object to validate for a valid title.
#' @keywords internal
validate_app_title_tag <- function(shiny_tag) {
  checkmate::assert_class(shiny_tag, "shiny.tag")
  checkmate::assert_true(shiny_tag$name == "head")
  child_names <- vapply(shiny_tag$children, `[[`, character(1L), "name")
  checkmate::assert_subset(c("title", "link"), child_names, .var.name = "child tags")
  rel_attr <- shiny_tag$children[[which(child_names == "link")]]$attribs$rel
  checkmate::assert_subset(
    rel_attr,
    c("icon", "shortcut icon"),
    .var.name = "Link tag's rel attribute",
    empty.ok = FALSE
  )
}

#' Build app title with favicon
#'
#' A helper function to create the browser title along with a logo.
#'
#' @param title (`character`) The browser title for the `teal` app.
#' @param favicon (`character`) The path for the icon for the title.
#' The image/icon path can be remote or the static path accessible by `shiny`, like the `www/`
#'
#' @return A `shiny.tag` containing the element that adds the title and logo to the `shiny` app.
#' @export
build_app_title <- function(
    title = "teal app",
    favicon = "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png") {
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(favicon, null.ok = TRUE)
  tags$head(
    tags$title(title),
    tags$link(
      rel = "icon",
      href = favicon,
      sizes = "any"
    )
  )
}

#' Application ID
#'
#' Creates App ID used to match filter snapshots to application.
#'
#' Calculate app ID that will be used to stamp filter state snapshots.
#' App ID is a hash of the app's data and modules.
#' See "transferring snapshots" section in ?snapshot.
#'
#' @param data (`teal_data` or `teal_data_module`) as accepted by `init`
#' @param modules (`teal_modules`) object as accepted by `init`
#'
#' @return A single character string.
#'
#' @keywords internal
create_app_id <- function(data, modules) {
  checkmate::assert_multi_class(data, c("teal_data", "teal_data_module"))
  checkmate::assert_class(modules, "teal_modules")

  data <- if (inherits(data, "teal_data")) {
    as.list(teal.code::get_env(data))
  } else if (inherits(data, "teal_data_module")) {
    deparse1(body(data$server))
  }
  modules <- lapply(modules, defunction)

  rlang::hash(list(data = data, modules = modules))
}

#' Go through list and extract bodies of encountered functions as string, recursively.
#' @keywords internal
#' @noRd
defunction <- function(x) {
  if (is.list(x)) {
    lapply(x, defunction)
  } else if (is.function(x)) {
    deparse1(body(x))
  } else {
    x
  }
}

#' Get unique labels
#'
#' Get unique labels for the modules to avoid namespace conflicts.
#'
#' @param labels (`character`) vector of labels
#'
#' @return (`character`) vector of unique labels
#'
#' @keywords internal
get_unique_labels <- function(labels) {
  make.unique(gsub("[^[:alnum:]]", "_", tolower(labels)), sep = "_")
}

#' Remove ANSI escape sequences from a string
#' @noRd
strip_style <- function(string) {
  checkmate::assert_string(string)

  gsub(
    "(?:(?:\\x{001b}\\[)|\\x{009b})(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])|\\x{001b}[A-M]",
    "",
    string,
    perl = TRUE,
    useBytes = TRUE
  )
}

#' Convert character list to human readable html with commas and "and"
#' @noRd
paste_datanames_character <- function(x,
                                      tags = list(span = shiny::tags$span, code = shiny::tags$code),
                                      tagList = shiny::tagList) { # nolint: object_name.
  checkmate::assert_character(x)
  do.call(
    tagList,
    lapply(seq_along(x), function(.ix) {
      tagList(
        tags$code(x[.ix]),
        if (.ix != length(x)) {
          tags$span(ifelse(.ix == length(x) - 1, " and ", ", "))
        }
      )
    })
  )
}

#' Build datanames error string for error message
#'
#' tags and tagList are overwritten in arguments allowing to create strings for
#' logging purposes
#' @noRd
build_datanames_error_message <- function(label = NULL,
                                          datanames,
                                          extra_datanames,
                                          tags = list(span = shiny::tags$span, code = shiny::tags$code),
                                          tagList = shiny::tagList) { # nolint: object_name.
  tags$span(
    tags$span(ifelse(length(extra_datanames) > 1, "Datasets", "Dataset")),
    paste_datanames_character(extra_datanames, tags, tagList),
    tags$span(
      paste0(
        ifelse(length(extra_datanames) > 1, "are missing", "is missing"),
        ifelse(is.null(label), ".", sprintf(" for tab '%s'.", label))
      )
    ),
    if (length(datanames) >= 1) {
      tagList(
        tags$span(ifelse(length(datanames) > 1, "Datasets", "Dataset")),
        tags$span("available in data:"),
        tagList(
          tags$span(
            paste_datanames_character(datanames, tags, tagList),
            tags$span(".", .noWS = "outside"),
            .noWS = c("outside")
          )
        )
      )
    } else {
      tags$span("No datasets are available in data.")
    }
  )
}
