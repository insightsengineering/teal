#' App state management.
#'
#' Capture and restore the global (app) input state.
#'
#' This module introduces bookmarks into `teal` apps: the `shiny` bookmarking mechanism becomes enabled
#' and server-side bookmarks can be created.
#'
#' The bookmark manager is accessed with the bookmark icon in the [`wunder_bar`].
#' The manager's header contains a title and a bookmark icon. Clicking the icon creates a bookmark.
#' As bookmarks are added, they will show up as rows in a table, each being a link that, when clicked,
#' will open the bookmarked application in a new window.
#'
#' @section Server logic:
#' A bookmark is a URL that contains the app address with a `/?_state_id_=<bookmark_dir>` suffix.
#' `<bookmark_dir>` is a directory created on the server, where the state of the application is saved.
#' Accessing the bookmark URL opens a new session of the app that starts in the previously saved state.
#'
#' Bookmarks are stored in a `reactiveVal` as a named list.
#' For every bookmark created a piece of HTML is created that contains a link,
#' whose text is the name of the bookmark and whose href is the bookmark URL.
#'
#' @section Bookmark mechanics:
#' When a bookmark is added, the user is prompted to name it.
#' New bookmark names are validated so that thy are unique. Leading and trailing white space is trimmed.
#'
#' Once a bookmark name has been accepted, the app state is saved: values of all inputs,
#' which are kept in the `input` slot of the `session` object, are dumped into the `input.rds` file
#' in the `<bookmark_dir>` directory on the server.
#' This is out of the box behavior that permeates the entire app, no adjustments to modules are necessary.
#' An additional `onBookmark` callback creates a snapshot of the current filter state
#' (the module has access to the filter state of the application through `slices_global` and `mapping_matrix`).
#' Then that snapshot, the previous snapshot history (which is passed to this module as argument),
#' and the previous bookmark history are dumped into the `values.rds` file in `<bookmark_dir>`.
#'
#' Finally, an `onBookmarked` callback adds the newly created bookmark to the bookmark history.
#' Notably, this occurs _after_ creating the bookmark is concluded so the bookmark history that was stored
#' does not include the newly added bookmark.
#'
#' When starting the app from a bookmark, `shiny` recognizes that the app is being restored,
#' locates the bookmark directory and loads both `.rds` file.
#' Values stored in `input.rds` are automatically set to their corresponding inputs.
#' The filter state that the app had upon bookmarking, which was saved as a separate snapshot, is restored.
#' This is done in the same manner as in the `snapshot_manager` module and thus requires access to `datasets_flat`,
#' which is passed to this module as argument.
#' Finally, snapshot history and bookmark history are loaded from `values.rds` and set to appropriate `reactiveVal`s.
#'
#' @section Note:
#' All `teal` apps are inherently bookmarkable. Normal `shiny` apps require that `enableBookmarking` be set to "server",
#' either by setting an argument in a `shinyApp` call or by calling a special function. In `teal` bookmarks are enabled
#' by automatically setting an option when the package is loaded.
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @inheritParams module_snapshot_manager
#' @param snapshot_history (named `list`) of unlisted `teal_slices` objects, as returned by the `snapshot_manager`.
#'
#' @return `reactiveVal` containing a named list of bookmark URLs.
#'
#' @name module_bookmark_manager
#' @aliases bookmark bookmark_manager bookmark_manager_module
#'

#' @rdname module_bookmark_manager
#' @keywords internal
#'
bookmark_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "manager_content",
    div(
      class = "manager_table_row",
      span(tags$b("Bookmark manager")),
      actionLink(ns("bookmark_add"), NULL, icon = suppressMessages(icon("solid fa-bookmark")), title = "add bookmark"),
      NULL
    ),
    uiOutput(ns("bookmark_list"))
  )
}

#' @rdname module_bookmark_manager
#' @keywords internal
#'
bookmark_manager_srv <- function(id, slices_global, mapping_matrix, datasets, snapshot_history) {
  checkmate::assert_character(id)
  checkmate::assert_true(is.reactive(slices_global))
  checkmate::assert_class(isolate(slices_global()), "teal_slices")
  checkmate::assert_true(is.reactive(mapping_matrix))
  checkmate::assert_data_frame(isolate(mapping_matrix()), null.ok = TRUE)
  checkmate::assert_list(datasets, types = "FilteredData", any.missing = FALSE, names = "named")
  checkmate::assert_true(is.reactive(snapshot_history))
  checkmate::assert_list(isolate(snapshot_history()), names = "unique")

  moduleServer(id, function(input, output, session) {
    logger::log_trace("bookmark_manager_srv initializing")

    # Set up bookmarking callbacks.
    # Register bookmark exclusions: all buttons and the `textInput` for bookmark name.
    setBookmarkExclude(c("bookmark_add", "bookmark_accept", "bookmark_name"))
    # Add bookmark history to bookmark.
    session$onBookmark(function(state) {
      logger::log_trace("bookmark_manager_srv@onBookmark: storing bookmark history")
      state$values$bookmark_history <- bookmark_history() # isolate this?
    })
    # This bookmark can only be used on the app session.
    app_session <- .subset2(shiny::getDefaultReactiveDomain(), "parent")
    app_session$onBookmarked(function(url) {
      logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark button clicked, registering bookmark")
      bookmark_name <- trimws(input$bookmark_name)
      if (identical(bookmark_name, "")) {
        logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark name rejected")
        showNotification(
          "Please name the bookmark.",
          type = "message"
        )
        updateTextInput(inputId = "bookmark_name", value = "", placeholder = "Meaningful, unique name")
        unlink(strsplit(url, "_state_id_=")[[1L]][[2L]], recursive = TRUE, force = TRUE, expand = FALSE)
      } else if (is.element(make.names(bookmark_name), make.names(names(bookmark_history())))) {
        logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark name rejected")
        showNotification(
          "This name is in conflict with other bookmark names. Please choose a different one.",
          type = "message"
        )
        updateTextInput(inputId = "bookmark_name", value = "", placeholder = "Meaningful, unique name")
        unlink(strsplit(url, "_state_id_=")[[1L]][[2L]], recursive = TRUE, force = TRUE, expand = FALSE)
      } else {
        # Add bookmark URL to bookmark history (with name).
        logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark name accepted, adding to history")
        bookmark_update <- c(bookmark_history(), list(url))
        names(bookmark_update)[length(bookmark_update)] <- bookmark_name
        bookmark_history(bookmark_update)

        removeModal()
      }
    })

    ns <- session$ns

    # Store input states.
    bookmark_history <- reactiveVal({
      # Restore directly from bookmarked state, if applicable.
      restoreValue(ns("bookmark_history"), list())
    })

    # Bookmark current input state - name bookmark.
    observeEvent(input$bookmark_add, {
      logger::log_trace("bookmark_manager_srv: bookmark_add button clicked")
      showModal(
        modalDialog(
          textInput(ns("bookmark_name"), "Name the bookmark", width = "100%", placeholder = "Meaningful, unique name"),
          footer = tagList(
            actionButton(ns("bookmark_accept"), label = "Accept", icon = icon("thumbs-up")),
            modalButton(label = "Cancel", icon = icon("thumbs-down"))
          ),
          size = "s"
        )
      )
    })

    # Initiate bookmarking with normal action button b/c `bookmarkButton` may not work on Windows.
    observeEvent(input$bookmark_accept, {
      app_session$doBookmark()
    })

    # Create UI elements and server logic for the bookmark table.
    # Divs are tracked for a slight speed margin.
    divs <- reactiveValues()

    observeEvent(bookmark_history(), {
      logger::log_trace("bookmark_manager_srv: bookmark history changed, updating bookmark list")
      lapply(names(bookmark_history()), function(s) {
        id_rowme <- sprintf("rowme_%s", make.names(s))

        # Create a row for the bookmark table.
        if (!is.element(id_rowme, names(divs))) {
          divs[[id_rowme]] <- div(
            class = "manager_table_row",
            a(h5(s), title = "go to bookmark", href = bookmark_history()[[s]], target = "blank")
          )
        }
      })
    })

    # Create table to display list of bookmarks and their actions.
    output$bookmark_list <- renderUI({
      rows <- rev(reactiveValuesToList(divs))
      if (length(rows) == 0L) {
        div(
          class = "manager_placeholder",
          "Bookmarks will appear here."
        )
      } else {
        rows
      }
    })

    bookmark_history
  })
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
    ans <- FALSE
  }

  if (ans) message("perfect!")
  invisible(NULL)
}
