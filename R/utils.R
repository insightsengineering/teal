#' The default favicon for the teal app.
#' @keywords internal
.teal_favicon <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png"

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
.include_parent_datanames <- function(datanames, join_keys) {
  ordered_datanames <- datanames
  for (current in datanames) {
    parents <- character(0L)
    while (length(current) > 0) {
      current <- teal.data::parent(join_keys, current)
      parents <- c(current, parents)
    }
    ordered_datanames <- c(parents, ordered_datanames)
  }

  unique(ordered_datanames)
}

#' Create a `FilteredData`
#'
#' Create a `FilteredData` object from a `teal_data` object.
#'
#' @param x (`teal_data`) object
#' @param datanames (`character`) vector of data set names to include; must be subset of `names(x)`
#' @return A `FilteredData` object.
#' @keywords internal
teal_data_to_filtered_data <- function(x, datanames = names(x)) {
  checkmate::assert_class(x, "teal_data")
  checkmate::assert_character(datanames, min.chars = 1L, any.missing = FALSE)
  # Otherwise, FilteredData will be created in the modules' scope later
  teal.slice::init_filtered_data(
    x = Filter(length, sapply(datanames, function(dn) x[[dn]], simplify = FALSE)),
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
#' These functions check if specified `datanames` in modules match those in the data object,
#' returning error messages or `TRUE` for successful validation. Two functions return error message
#' in different forms:
#' - `check_modules_datanames` returns `character(1)` for basic assertion usage
#' - `check_modules_datanames_html` returns `shiny.tag.list` to display it in the app.
#'
#' @param modules (`teal_modules`) object
#' @param datanames (`character`) names of datasets available in the `data` object
#'
#' @return `TRUE` if validation passes, otherwise `character(1)` or `shiny.tag.list`
#' @keywords internal
check_modules_datanames <- function(modules, datanames) {
  out <- check_modules_datanames_html(modules, datanames)
  if (inherits(out, "shiny.tag.list")) {
    out_with_ticks <- gsub("<code>|</code>", "`", toString(out))
    out_text <- gsub("<[^<>]+>", "", toString(out_with_ticks))
    trimws(gsub("[[:space:]]+", " ", out_text))
  } else {
    out
  }
}

#' @rdname check_modules_datanames
check_reserved_datanames <- function(datanames) {
  reserved_datanames <- datanames[datanames %in% c("all", ".raw_data")]
  if (length(reserved_datanames) == 0L) {
    return(NULL)
  }

  tags$span(
    to_html_code_list(reserved_datanames),
    sprintf(
      "%s reserved for internal use. Please avoid using %s as %s.",
      pluralize(reserved_datanames, "is", "are"),
      pluralize(reserved_datanames, "it", "them"),
      pluralize(reserved_datanames, "a dataset name", "dataset names")
    )
  )
}

#' @rdname check_modules_datanames
check_modules_datanames_html <- function(modules, datanames) {
  check_datanames <- check_modules_datanames_recursive(modules, datanames)
  show_module_info <- inherits(modules, "teal_modules") # used in two contexts - module and app

  reserved_datanames <- check_reserved_datanames(datanames)

  if (!length(check_datanames)) {
    out <- if (is.null(reserved_datanames)) {
      TRUE
    } else {
      shiny::tagList(reserved_datanames)
    }
    return(out)
  }
  shiny::tagList(
    reserved_datanames,
    lapply(
      check_datanames,
      function(mod) {
        tagList(
          tags$span(
            tags$span(pluralize(mod$missing_datanames, "Dataset")),
            to_html_code_list(mod$missing_datanames),
            tags$span(
              sprintf(
                "%s missing%s.",
                pluralize(mod$missing_datanames, "is", "are"),
                if (show_module_info) sprintf(" for module '%s'", mod$label) else ""
              )
            )
          ),
          if (length(datanames) >= 1) {
            tagList(
              tags$span(pluralize(datanames, "Dataset")),
              tags$span("available in data:"),
              tagList(
                tags$span(
                  to_html_code_list(datanames),
                  tags$span(".", .noWS = "outside"),
                  .noWS = c("outside")
                )
              )
            )
          } else {
            tags$span("No datasets are available in data.")
          },
          tags$br(.noWS = "before")
        )
      }
    )
  )
}

#' Recursively checks modules and returns list for every datanames mismatch between module and data
#' @noRd
check_modules_datanames_recursive <- function(modules, datanames) { # nolint: object_name_length
  checkmate::assert_multi_class(modules, c("teal_module", "teal_modules"))
  checkmate::assert_character(datanames)
  if (inherits(modules, "teal_modules")) {
    unlist(
      lapply(modules$children, check_modules_datanames_recursive, datanames = datanames),
      recursive = FALSE
    )
  } else {
    missing_datanames <- setdiff(modules$datanames, c("all", datanames))
    if (length(missing_datanames)) {
      list(list(
        label = modules$label,
        missing_datanames = missing_datanames
      ))
    }
  }
}

#' Convert character vector to html code separated with commas and "and"
#' @noRd
to_html_code_list <- function(x) {
  checkmate::assert_character(x)
  do.call(
    tagList,
    lapply(seq_along(x), function(.ix) {
      tagList(
        tags$code(x[.ix]),
        if (.ix != length(x)) {
          if (.ix == length(x) - 1) tags$span(" and ") else tags$span(", ", .noWS = "before")
        }
      )
    })
  )
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
  lifecycle::deprecate_soft(
    when = "0.16.0",
    what = "build_app_title()",
    details = "Use `modify_title()` on the object created using the `init`."
  )
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
    as.list(data)
  } else if (inherits(data, "teal_data_module")) {
    deparse1(body(data$server))
  }
  modules <- lapply(modules, defunction)

  # Suppress warnings of type: `package:MultiAssayExperiment' may not be available when loading`
  # This is because the package namespace may be part of the `data` object
  suppressWarnings(rlang::hash(list(data = data, modules = modules)))
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

#' @keywords internal
#' @noRd
pasten <- function(...) paste0(..., "\n")

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
          tags$span(if (.ix == length(x) - 1) " and " else ", ")
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
    tags$span(pluralize(extra_datanames, "Dataset")),
    paste_datanames_character(extra_datanames, tags, tagList),
    tags$span(
      sprintf(
        "%s missing%s",
        pluralize(extra_datanames, "is", "are"),
        if (is.null(label)) "" else sprintf(" for tab '%s'", label)
      )
    ),
    if (length(datanames) >= 1) {
      tagList(
        tags$span(pluralize(datanames, "Dataset")),
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

#' Smart `rbind`
#'
#' Combine `data.frame` objects which have different columns
#'
#' @param ... (`data.frame`)
#' @keywords internal
.smart_rbind <- function(...) {
  dots <- list(...)
  checkmate::assert_list(dots, "data.frame", .var.name = "...")
  Reduce(
    x = dots,
    function(x, y) {
      all_columns <- union(colnames(x), colnames(y))
      x[setdiff(all_columns, colnames(x))] <- NA
      y[setdiff(all_columns, colnames(y))] <- NA
      rbind(x, y)
    }
  )
}

#' Pluralize a word depending on the size of the input
#'
#' @param x (`object`) to check length for plural.
#' @param singular (`character`) singular form of the word.
#' @param plural (optional `character`) plural form of the word. If not given an "s"
#' is added to the singular form.
#'
#' @return A `character` that correctly represents the size of the `x` argument.
#' @keywords internal
pluralize <- function(x, singular, plural = NULL) {
  checkmate::assert_string(singular)
  checkmate::assert_string(plural, null.ok = TRUE)
  if (length(x) == 1L) { # Zero length object should use plural form.
    singular
  } else {
    if (is.null(plural)) {
      sprintf("%ss", singular)
    } else {
      plural
    }
  }
}

#' @keywords internal
.dropdown_button <- function(id = NULL, label, icon) {
  tags$span(
    class = "teal dropdown-button",
    tags$a(
      id = id,
      class = "action-button",
      role = "button",
      style = "text-decoration: none;",
      bsicons::bs_icon(icon, class = "text-primary"),
      label,
      bsicons::bs_icon("chevron-down", class = "text-primary dropdown-arrow")
    )
  )
}

#' @keywords internal
.expand_button <- function(id, label, icon) {
  tags$span(
    class = "teal expand-button",
    htmltools::htmlDependency(
      name = "teal-busy-disable",
      version = utils::packageVersion("teal"),
      package = "teal",
      src = "js",
      script = "busy-disable.js"
    ),
    shinyjs::useShinyjs(),
    tags$button(
      id = id,
      class = "action-button teal-busy-disable",
      role = "button",
      style = "text-decoration: none;",
      tags$span(class = "icon", bsicons::bs_icon(icon, class = "text-primary")),
      tags$span(class = "label", label)
    )
  )
}


#' @keywords internal
.primary_button <- function(id, label, icon = NULL) {
  tags$a(
    id = id,
    class = "teal primary-button action-button",
    role = "button",
    style = "text-decoration: none;",
    if (!is.null(icon)) {
      bsicons::bs_icon(icon, class = "text-primary")
    },
    label
  )
}
