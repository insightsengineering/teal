#' Get client timezone
#'
#' User timezone in the browser may be different to the one on the server.
#' This script can be run to register a `shiny` input which contains information about the timezone in the browser.
#'
#' @param ns (`function`) namespace function passed from the `session` object in the `shiny` server.
#'  For `shiny` modules this will allow for proper name spacing of the registered input.
#'
#' @return (`shiny`) input variable accessible with `input$tz` which is a (`character`)
#'  string containing the timezone of the browser/client.
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
    NULL
  } else if (!inherits(bs_theme, "bs_theme")) {
    warning("teal.bs_theme has to be of a bslib::bs_theme class, the default shiny bootstrap is used.")
    NULL
  } else {
    bs_theme
  }
}

#' Return parentnames along with datanames.
#' @noRd
#' @keywords internal
include_parent_datanames <- function(dataname, join_keys) {
  parents <- character(0)
  for (i in dataname) {
    while (length(i) > 0) {
      parent_i <- teal.data::parent(join_keys, i)
      parents <- c(parent_i, parents)
      i <- parent_i
    }
  }

  unique(c(parents, dataname))
}

#' Create a `FilteredData`
#'
#' Create a `FilteredData` object from a `teal_data` object.
#'
#' @param x (`teal_data`) object
#' @param datanames (`character`) vector of data set names to include; must be subset of `datanames(x)`
#' @return A `FilteredData` object.
#' @keywords internal
teal_data_to_filtered_data <- function(x, datanames = teal_data_datanames(x)) {
  checkmate::assert_class(x, "teal_data")
  checkmate::assert_character(datanames, min.chars = 1L, any.missing = FALSE)

  ans <- teal.slice::init_filtered_data(
    x = sapply(datanames, function(dn) x[[dn]], simplify = FALSE),
    join_keys = teal.data::join_keys(x)
  )
  # Piggy-back pre-processing code for datasets of interest so that filtering code can be appended later.
  attr(ans, "preprocessing_code") <- teal.data::get_code(x, datanames = datanames, check_names = FALSE)
  attr(ans, "verification_status") <- x@verified
  ans
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
  checkmate::assert_class(modules, "teal_modules")
  checkmate::assert_character(datanames)

  recursive_check_datanames <- function(modules, datanames) {
    # check teal_modules against datanames
    if (inherits(modules, "teal_modules")) {
      sapply(modules$children, function(module) recursive_check_datanames(module, datanames = datanames))
    } else {
      extra_datanames <- setdiff(modules$datanames, c("all", datanames))
      if (length(extra_datanames)) {
        sprintf(
          "- Module '%s' uses datanames not available in 'data': (%s) not in (%s)",
          modules$label,
          toString(dQuote(extra_datanames, q = FALSE)),
          toString(dQuote(datanames, q = FALSE))
        )
      }
    }
  }
  check_datanames <- unlist(recursive_check_datanames(modules, datanames))
  if (length(check_datanames)) {
    paste(check_datanames, collapse = "\n")
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


#' Create filterable data for modules
#'
#' Converts input data to a `FilteredData` object(s) to allow filtering before passing data to individual modules.
#'
#' @param data (`teal_data`)
#' @param modules (`teal_modules`) object
#' @param filters (`teal_slices`) object
#' @param filtered_data_singleton A result of `teal_data_to_filtered_data` applied to `data`.
#' @return Returns list of same shape as `modules`, containing `FilteredData` at every leaf.
#' If module specific, each leaf contains different instance, otherwise every leaf contains `filtered_data_singleton`.
#' @keywords internal
modules_datasets <- function(data, modules, filters, filtered_data_singleton = teal_data_to_filtered_data(data)) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  checkmate::assert_class(filters, "modules_teal_slices")
  checkmate::assert_r6(filtered_data_singleton, "FilteredData")

  if (!isTRUE(attr(filters, "module_specific"))) {
    # subset global filters
    slices <- shiny::isolate({
      Filter(function(x) x$id %in% attr(filters, "mapping")$global_filters, filters)
    })
    filtered_data_singleton$set_filter_state(slices)
    return(modules_structure(modules, filtered_data_singleton))
  }

  if (inherits(modules, "teal_module")) {
    # 1. get datanames
    datanames <-
      if (is.null(modules$datanames) || identical(modules$datanames, "all")) {
        include_parent_datanames(
          teal_data_datanames(data),
          teal.data::join_keys(data)
        )
      } else {
        include_parent_datanames(
          modules$datanames,
          teal.data::join_keys(data)
        )
      }
    # 2. subset filters (global + dedicated)
    slices <- shiny::isolate({
      Filter(x = filters, f = function(x) {
        x$dataname %in% datanames &&
          (x$id %in% attr(filters, "mapping")$global_filters ||
            x$id %in% unique(unlist(attr(filters, "mapping")[modules$label]))) # nolint: indentation_linter.
      })
    })
    # 2a. subset include/exclude varnames
    slices$include_varnames <- attr(slices, "include_varnames")[names(attr(slices, "include_varnames")) %in% datanames]
    slices$exclude_varnames <- attr(slices, "exclude_varnames")[names(attr(slices, "exclude_varnames")) %in% datanames]

    # 3. instantiate FilteredData
    filtered_data <- teal_data_to_filtered_data(data, datanames)
    # 4. set state
    filtered_data$set_filter_state(slices)
    # 5. return
    return(filtered_data)
  } else if (inherits(modules, "teal_modules")) {
    ans <- lapply(
      modules$children,
      modules_datasets,
      data = data,
      filters = filters,
      filtered_data_singleton = filtered_data_singleton
    )
    names(ans) <- vapply(modules$children, `[[`, character(1), "label")

    return(ans)
  }

  stop("something is not right")
}

# Returns nested list of same shape as `modules` with `value` at every leaf.
modules_structure <- function(modules, value = TRUE) {
  if (inherits(modules, "teal_module")) {
    return(value)
  } else {
    stats::setNames(
      lapply(modules$children, modules_structure, value),
      vapply(modules$children, `[[`, character(1), "label")
    )
  }
}

#' Wrapper on `teal.data::datanames`
#'
#' Special function used in internals of `teal` to return names of datasets even if `datanames`
#' has not been set.
#' @param data (`teal_data`)
#' @return `character`
#' @keywords internal
teal_data_datanames <- function(data) {
  checkmate::assert_class(data, "teal_data")
  if (length(teal.data::datanames(data))) {
    teal.data::datanames(data)
  } else {
    ls(teal.code::get_env(data), all.names = TRUE)
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
    as.list(data@env)
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

#' Create `renv` `.lock` file
#'
#' @description
#' Function allows to create `renv::snapshot()` during the `teal` initialization.
#'
#' @details
#'
#' The `.lockfile` creation is based on [renv::snapshot()]. It allows multiple ways of `.lockfile` creation.
#'
#' - **Pre-computed `.lockfile`**: User is able to provide his own pre-computed `.lockfile` by setting the path to the
#' `.lockfile` through `options("teal.renv.lockfile")`. Then this function is not used.
#' - **Working directory `.lockfile`**: If `options("teal.renv.lockfile")` is empty, by default `teal` will
#' create an `implicit` type of the `.lockfile`, that uses `renv::dependencies()` to detect all R packages in the
#' current project working directory.
#' - **`DESCRIPTION` based lockfile**: You can always include a `DESCRIPTION` file in your working directory and enable
#' `.lockfile` creation based on this file. To do this, run `renv::settings$snapshot.type("explicit")`. Naming of `type`
#' is the same as in `renv::snapshot()`. For the `"explicit"` type refer to `renv::settings$package.dependency.fields()`
#' to see what `DESCRIPTION` fields are included in the `.lockfile`.
#' - **Custom files based `.lockfile`**: If you want to specify custom files as a base for the `.lockfile`, then run
#' `renv::settings$snapshot.type("custom")` and set `renv.snapshot.filter` option.
#'
#' @note
#' This function computes the `.lockfile` as a `future::future` promise, while
#' running the evaluation of the process on a separate worker. `future::plan()` and `shiny::ExtendedTask()` are used to
#' setup parallel asynchronous computations.
#'
#' @section `.lockfile` usage:
#' Once you have a `.lockfile` file, you can restore R project with `ren::init();renv::restore()`.
#'
#'
#' @seealso [renv::snapshot()], [renv::restore()], [renv::init()].
#'
#' @return (`character(1)`) the path to the `lockfile` created in a `tempdir()`.
#'
#' @keywords internal
create_renv_lockfile <- function() {
  future::future({
    temp_dir <- tempdir()
    lockfile_path <- file.path(temp_dir, "renv.lock")

    renv_status <- capture.output(
      renv::snapshot(
        lockfile = lockfile_path,
        prompt = FALSE,
        force = TRUE
        # type = is taken from renv::settings$snapshot.type()
      )
    )
    if (any(grepl("Lockfile written", renv_status))) {
      logger::log_info("lockfile created successfully.", namespace = "teal")
    } else {
      logger::log_info("lockfile created with issues.", namespace = "teal")
    }

    lockfile_path
  })
}
