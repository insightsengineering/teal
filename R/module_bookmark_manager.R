#' App state management.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Capture and restore the global (app) input state.
#'
#' @details
#' This module introduces bookmarks into `teal` apps: the `shiny` bookmarking mechanism becomes enabled
#' and server-side bookmarks can be created.
#'
#' The bookmark manager presents a button with the bookmark icon and is placed in the tab-bar.
#' When clicked, the button creates a bookmark and opens a modal which displays the bookmark URL.
#'
#' `teal` does not guarantee that all modules (`teal_module` objects) are bookmarkable.
#' Those that are, have a `teal_bookmarkable` attribute set to `TRUE`. If any modules are not bookmarkable,
#' the bookmark manager modal displays a warning and the bookmark button displays a flag.
#' In order to communicate that a external module is bookmarkable, the module developer
#' should set the `teal_bookmarkable` attribute to `TRUE`.
#'
#' @section Server logic:
#' A bookmark is a URL that contains the app address with a `/?_state_id_=<bookmark_dir>` suffix.
#' `<bookmark_dir>` is a directory created on the server, where the state of the application is saved.
#' Accessing the bookmark URL opens a new session of the app that starts in the previously saved state.
#'
#' @section Note:
#' To enable bookmarking use either:
#' - `shiny` app by using `shinyApp(..., enableBookmarking = "server")` (not supported in `shinytest2`)
#' - set `options(shiny.bookmarkStore = "server")` before running the app
#'
#'
#' @inheritParams module_teal
#'
#' @return Invisible `NULL`.
#'
#' @aliases bookmark bookmark_manager bookmark_manager_module
#'
#' @name module_bookmark_manager
#' @rdname module_bookmark_manager
#'
#' @keywords internal
#'
NULL

#' @rdname module_bookmark_manager
ui_bookmark_panel <- function(id, modules) {
  ns <- NS(id)

  bookmark_option <- get_bookmarking_option()
  is_unbookmarkable <- need_bookmarking(modules)
  shinyOptions(bookmarkStore = bookmark_option)

  if (!all(is_unbookmarkable) && identical(bookmark_option, "server")) {
    .expand_button(
      id = ns("do_bookmark"),
      label = "Bookmark",
      icon = "bookmark-fill"
    )
  }
}

#' @rdname module_bookmark_manager
srv_bookmark_panel <- function(id, modules) {
  checkmate::assert_character(id)
  checkmate::assert_class(modules, "teal_modules")
  moduleServer(id, function(input, output, session) {
    logger::log_debug("bookmark_manager_srv initializing")
    ns <- session$ns
    bookmark_option <- get_bookmarking_option()
    is_unbookmarkable <- need_bookmarking(modules)

    # Set up bookmarking callbacks ----
    # Register bookmark exclusions: do_bookmark button to avoid re-bookmarking
    setBookmarkExclude(c("do_bookmark"))
    # This bookmark can only be used on the app session.
    app_session <- .subset2(session, "parent")
    app_session$onBookmarked(function(url) {
      logger::log_debug("bookmark_manager_srv@onBookmarked: bookmark button clicked, registering bookmark")
      modal_content <- if (bookmark_option != "server") {
        msg <- sprintf(
          "Bookmarking has been set to \"%s\".\n%s\n%s",
          bookmark_option,
          "Only server-side bookmarking is supported.",
          "Please contact your app developer."
        )
        tags$div(
          tags$p(msg, class = "text-warning")
        )
      } else {
        tags$div(
          tags$span(
            tags$pre(url)
          ),
          if (any(is_unbookmarkable)) {
            bkmb_summary <- rapply2(
              modules_bookmarkable(modules),
              function(x) {
                if (isTRUE(x)) {
                  "\u2705" # check mark
                } else if (isFALSE(x)) {
                  "\u274C" # cross mark
                } else {
                  "\u2753" # question mark
                }
              }
            )
            tags$div(
              tags$p(
                icon("fas fa-exclamation-triangle"),
                "Some modules will not be restored when using this bookmark.",
                tags$br(),
                "Check the list below to see which modules are not bookmarkable.",
                class = "text-warning"
              ),
              tags$pre(yaml::as.yaml(bkmb_summary))
            )
          }
        )
      }


      showModal(
        div(
          class = "teal bookmark-popup",
          modalDialog(
            id = ns("bookmark_modal"),
            title = "Bookmarked teal app url",
            modal_content,
            easyClose = TRUE,
            footer = NULL
          )
        )
      )
    })

    # manually trigger bookmarking because of the problems reported on windows with bookmarkButton in teal
    observeEvent(input$do_bookmark, {
      logger::log_debug("bookmark_manager_srv@1 do_bookmark module clicked.")
      session$doBookmark()
    })

    invisible(NULL)
  })
}


#' @rdname module_bookmark_manager
get_bookmarking_option <- function() {
  bookmark_option <- getShinyOption("bookmarkStore")
  if (is.null(bookmark_option) && identical(getOption("shiny.bookmarkStore"), "server")) {
    bookmark_option <- getOption("shiny.bookmarkStore")
  }
  bookmark_option
}

#' @rdname module_bookmark_manager
need_bookmarking <- function(modules) {
  unlist(rapply2(
    modules_bookmarkable(modules),
    Negate(isTRUE)
  ))
}


# utilities ----

#' Restore value from bookmark.
#'
#' Get value from bookmark or return default.
#'
#' Bookmarks can store not only inputs but also arbitrary values.
#' These values are stored by `onBookmark` callbacks and restored by `onBookmarked` callbacks,
#' and they are placed in the `values` environment in the `session$restoreContext` field.
#' Using `teal_data_module` makes it impossible to run the callbacks
#' because the app becomes ready before modules execute and callbacks are registered.
#' In those cases the stored values can still be recovered from the `session` object directly.
#'
#' Note that variable names in the `values` environment are prefixed with module name space names,
#' therefore, when using this function in modules, `value` must be run through the name space function.
#'
#' @param value (`character(1)`) name of value to restore
#' @param default fallback value
#'
#' @return
#' In an application restored from a server-side bookmark,
#' the variable specified by `value` from the `values` environment.
#' Otherwise `default`.
#'
#' @keywords internal
#'
restoreValue <- function(value, default) { # nolint: object_name.
  checkmate::assert_character("value")
  session_default <- shiny::getDefaultReactiveDomain()
  session_parent <- .subset2(session_default, "parent")
  session <- if (is.null(session_parent)) session_default else session_parent

  if (isTRUE(session$restoreContext$active) && exists(value, session$restoreContext$values, inherits = FALSE)) {
    session$restoreContext$values[[value]]
  } else {
    default
  }
}

#' Compare bookmarks.
#'
#' Test if two bookmarks store identical state.
#'
#' `input` environments are compared one variable at a time and if not identical,
#' values in both bookmarks are reported. States of `datatable`s are stripped
#' of the `time` element before comparing because the time stamp is always different.
#' The contents themselves are not printed as they are large and the contents are not informative.
#' Elements present in one bookmark and absent in the other are also reported.
#' Differences are printed as messages.
#'
#' `values` environments are compared with `all.equal`.
#'
#' @section How to use:
#' Open an application, change relevant inputs (typically, all of them), and create a bookmark.
#' Then open that bookmark and immediately create a bookmark of that.
#' If restoring bookmarks occurred properly, the two bookmarks should store the same state.
#'
#'
#' @param book1,book2 bookmark directories stored in `shiny_bookmarks/`;
#'                    default to the two most recently modified directories
#'
#' @return
#' Invisible `NULL` if bookmarks are identical or if there are no bookmarks to test.
#' `FALSE` if inconsistencies are detected.
#'
#' @keywords internal
#'
bookmarks_identical <- function(book1, book2) {
  if (!dir.exists("shiny_bookmarks")) {
    message("no bookmark directory")
    return(invisible(NULL))
  }

  ans <- TRUE

  if (missing(book1) && missing(book2)) {
    dirs <- list.dirs("shiny_bookmarks", recursive = FALSE)
    bookmarks_sorted <- basename(rev(dirs[order(file.mtime(dirs))]))
    if (length(bookmarks_sorted) < 2L) {
      message("no bookmarks to compare")
      return(invisible(NULL))
    }
    book1 <- bookmarks_sorted[2L]
    book2 <- bookmarks_sorted[1L]
  } else {
    if (!dir.exists(file.path("shiny_bookmarks", book1))) stop(book1, " not found")
    if (!dir.exists(file.path("shiny_bookmarks", book2))) stop(book2, " not found")
  }

  book1_input <- readRDS(file.path("shiny_bookmarks", book1, "input.rds"))
  book2_input <- readRDS(file.path("shiny_bookmarks", book2, "input.rds"))

  elements_common <- intersect(names(book1_input), names(book2_input))
  dt_states <- grepl("_state$", elements_common)
  if (any(dt_states)) {
    for (el in elements_common[dt_states]) {
      book1_input[[el]][["time"]] <- NULL
      book2_input[[el]][["time"]] <- NULL
    }
  }

  identicals <- mapply(identical, book1_input[elements_common], book2_input[elements_common])
  non_identicals <- names(identicals[!identicals])
  compares <- sprintf("$ %s:\t%s --- %s", non_identicals, book1_input[non_identicals], book2_input[non_identicals])
  if (length(compares) != 0L) {
    message("common elements not identical: \n", paste(compares, collapse = "\n"))
    ans <- FALSE
  }

  elements_boook1 <- setdiff(names(book1_input), names(book2_input))
  if (length(elements_boook1) != 0L) {
    dt_states <- grepl("_state$", elements_boook1)
    if (any(dt_states)) {
      for (el in elements_boook1[dt_states]) {
        if (is.list(book1_input[[el]])) book1_input[[el]] <- "--- data table state ---"
      }
    }
    excess1 <- sprintf("$ %s:\t%s", elements_boook1, book1_input[elements_boook1])
    message("elements only in book1: \n", paste(excess1, collapse = "\n"))
    ans <- FALSE
  }

  elements_boook2 <- setdiff(names(book2_input), names(book1_input))
  if (length(elements_boook2) != 0L) {
    dt_states <- grepl("_state$", elements_boook1)
    if (any(dt_states)) {
      for (el in elements_boook1[dt_states]) {
        if (is.list(book2_input[[el]])) book2_input[[el]] <- "--- data table state ---"
      }
    }
    excess2 <- sprintf("$ %s:\t%s", elements_boook2, book2_input[elements_boook2])
    message("elements only in book2: \n", paste(excess2, collapse = "\n"))
    ans <- FALSE
  }

  book1_values <- readRDS(file.path("shiny_bookmarks", book1, "values.rds"))
  book2_values <- readRDS(file.path("shiny_bookmarks", book2, "values.rds"))

  if (!isTRUE(all.equal(book1_values, book2_values))) {
    message("different values detected")
    message("choices for numeric filters MAY be different, see RangeFilterState$set_choices")
    ans <- FALSE
  }

  if (ans) message("perfect!")
  invisible(NULL)
}


# Replacement for [base::rapply] which doesn't handle NULL values - skips the evaluation
# of the function and returns NULL for given element.
rapply2 <- function(x, f) {
  if (inherits(x, "list")) {
    lapply(x, rapply2, f = f)
  } else {
    f(x)
  }
}
