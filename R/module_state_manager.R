#' App state management.
#'
#' Capture and restore the global (app) input state.
#'
#' This is a work in progress.
#'
#' @param id (`character(1)`) `shiny` module id
#'
#' @return Nothing is returned.
#'
#' @name state_manager_module
#' @aliases grab grab_manager state_manager
#'
#' @author Aleksander Chlebowski
#'
#' @seealso [`app_state_grab`], [`app_state_store`], [`app_state_restore`]
#'
#' @rdname state_manager_module
#' @keywords internal
#'
state_manager_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "snapshot_manager_content",
    div(
      class = "snapshot_table_row",
      span(tags$b("State manager")),
      actionLink(ns("grab_add"), label = NULL, icon = icon("camera"), title = "grab input state"),
      actionLink(ns("grab_reset"), label = NULL, icon = icon("undo"), title = "reset initial state"),
      NULL
    ),
    uiOutput(ns("grab_list"))
  )
}

#' @rdname state_manager_module
#' @keywords internal
#'
state_manager_srv <- function(id) {
  checkmate::assert_character(id)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store initial input states.
    grab_history <- reactiveVal({
      list(
        "Initial input state" = app_state_grab()
      )
    })

    # Grab current input state - name grab.
    observeEvent(input$grab_add, {
      showModal(
        modalDialog(
          textInput(ns("grab_name"), "Name the grab", width = "100%", placeholder = "Meaningful, unique name"),
          footer = tagList(
            actionButton(ns("grab_name_accept"), "Accept", icon = icon("thumbs-up")),
            modalButton(label = "Cancel", icon = icon("thumbs-down"))
          ),
          size = "s"
        )
      )
    })
    # Grab current input state - store grab.
    observeEvent(input$grab_name_accept, {
      grab_name <- trimws(input$grab_name)
      if (identical(grab_name, "")) {
        showNotification(
          "Please name the grab.",
          type = "message"
        )
        updateTextInput(inputId = "grab_name", value = "", placeholder = "Meaningful, unique name")
      } else if (is.element(make.names(grab_name), make.names(names(grab_history())))) {
        showNotification(
          "This name is in conflict with other grab names. Please choose a different one.",
          type = "message"
        )
        updateTextInput(inputId = "grab_name", value = , placeholder = "Meaningful, unique name")
      } else {
        grab <- app_state_grab()
        grab_update <- c(grab_history(), list(grab))
        names(grab_update)[length(grab_update)] <- grab_name
        grab_history(grab_update)
        removeModal()
        # Reopen filter manager modal by clicking button in the main application.
        shinyjs::click(id = "teal-main_ui-filter_manager-show", asis = TRUE)
      }
    })

    # Restore initial input state.
    observeEvent(input$grab_reset, {
      s <- "Initial input state"
      ### Begin restore procedure. ###
      grab <- grab_history()[[s]]
      app_state_restore(grab)
      if (!is.null(setdiff_teal_grab(grab, app_state_grab()))) {
        shinyjs::click("grab_reset")
      }
      removeModal()
      ### End restore procedure. ###
    })

    # Create UI elements and server logic for the grab table.
    # Observers must be tracked to avoid duplication and excess reactivity.
    # Remaining elements are tracked likewise for consistency and a slight speed margin.
    observers <- reactiveValues()
    handlers <- reactiveValues()
    divs <- reactiveValues()

    observeEvent(grab_history(), {
      lapply(names(grab_history())[-1L], function(s) {
        id_pickme <- sprintf("pickme_%s", make.names(s))
        id_saveme <- sprintf("saveme_%s", make.names(s))
        id_rowme <- sprintf("rowme_%s", make.names(s))

        # Observer for restoring grab.
        if (!is.element(id_pickme, names(observers))) {
          observers[[id_pickme]] <- observeEvent(input[[id_pickme]], {
            ### Begin restore procedure. ###
            grab <- grab_history()[[s]]
            app_state_restore(grab)
            if (!is.null(setdiff_teal_grab(grab, app_state_grab()))) {
              shinyjs::click(id_pickme)
            }
            removeModal()
            ### End restore procedure. ###
          })
        }
        # Create handler for downloading grab.
        if (!is.element(id_saveme, names(handlers))) {
          output[[id_saveme]] <- downloadHandler(
            filename = function() {
              sprintf("teal_inputs_%s_%s.json", s, Sys.Date())
            },
            content = function(file) {
              app_state_store(grab = grab_history()[[s]], file = file)
            }
          )
          handlers[[id_saveme]] <- id_saveme
        }
        # Create a row for the grab table.
        if (!is.element(id_rowme, names(divs))) {
          divs[[id_rowme]] <- div(
            class = "snapshot_table_row",
            span(h5(s)),
            actionLink(inputId = ns(id_pickme), label = icon("circle-check"), title = "select"),
            downloadLink(outputId = ns(id_saveme), label = icon("save"), title = "save to file")
          )
        }
      })
    })

    # Create table to display list of grabs and their actions.
    output$grab_list <- renderUI({
      rows <- lapply(rev(reactiveValuesToList(divs)), function(d) d)
      if (length(rows) == 0L) {
        div(
          class = "snapshot_manager_placeholder",
          "Input states will appear here."
        )
      } else {
        rows
      }
    })


  })
}




# utility functions ----

#' Grab selection state (value) of all input items in the app.
#'
#' Find all inputs in the app ans store their state.
#' Buttons are omitted as their values do not (usually) matter
#' and they are virtually impossible to restore.
#'
#' @return
#' Object of class `teal_grab`, describing the state of all inputs in the app (except buttons).
#' @keywords internal
#' @seealso [`app_state_store`], [`app_state_restore`], [`state_manager_module`]
#'
app_state_grab <- function() {
  session <- get_master_session()
  input <- session$input
  as.teal_grab(shiny::reactiveValuesToList(input))
}


#' Save input grab to json file.
#'
#' @param grab `teal_grab`
#' @param file `path` to save the input states to; must be a .json file; will be overwritten
#' @return Returns `NULL` invisibly.
#' @keywords internal
#' @seealso [`app_state_grab`], [`app_state_restore`], [`state_manager_module`]
#'
app_state_store <- function(grab, file) {
  checkmate::assert_class(grab, "teal_grab")
  checkmate::assert_path_for_output(file, overwrite = TRUE, extension = "json")

  jsonlite::write_json(jsonlite::serializeJSON(grab, pretty = TRUE), file)
  invisible(NULL)
}


#' Restore state (value) of all input items in the app according to a grab or file.
#'
#' @param grab optional `teal_grab`
#' @param file optional `path` to a .json file
#' @return Returns `NULL` invisibly.
#' @keywords internal
#' @seealso [`app_state_grab`], [`app_state_store`], [`state_manager_module`]
#'
app_state_restore <- function(grab, file) {
  if ((missing(grab) && missing(file)) || (!missing(grab) && !missing(file))) {
    stop("specify either \"grab\" or \"file\"")
  }
  if (!missing(grab)) {
    checkmate::assert_class(grab, "teal_grab")
  }
  if (!missing(file)) {
    checkmate::assert_file_exists(file, access = "r")
  }

  app_state <-
    if (missing(file)) {
      grab
    } else if (missing(grab)) {
      jsonlite::unserializeJSON(jsonlite::read_json(file)[[1L]])
    }

  session <- get_master_session()
  input <- session$input

  # validate saved input state
  checkmate::assert_subset(vapply(app_state, `[[`, character(1L), "id"), choices = names(input))

  lapply(app_state, function(i) {
    if (inherits(i$value, "POSIXt")) {
      i$value <- posix_ms_to_json(i$value)
    }
    session$sendInputMessage(inputId = i$id, message = list(value = i$value))
  })

  invisible(NULL)
}


#' @export
#'
format.teal_grab <- function(x) {
  all_ids <- vapply(x, `[[`, character(1), "id")
  all_values <- vapply(x, function(xx) toString(xx[["value"]]), character(1L))

  contents <- if (length(all_ids) + length(all_values) > 0L) {
    all_values_trimmed <- lapply(all_values, function(x) {
      if (nchar(x) > 40) {
        paste(substr(x, 1, 36),  "...")
      } else {
        x
      }
    })
    longest_id <- max(nchar(all_ids))
    longest_value <- max(nchar(all_values_trimmed))
    sprintf(sprintf("%%0%ds : %%0%ds", longest_id + 2L, longest_value), all_ids, all_values_trimmed)
  } else {
    "  no inputs"
  }

  paste(
    c(
      "teal_grab:",
      contents,
      ""
    ),
    collapse = "\n"
  )
}


#' @export
#'
print.teal_grab <- function(x, ...) {
  cat(format(x, ...))
}


#' Convert named list to `teal_grab`.
#'
#' @param x `named list`
#' @return
#' Object of class `teal_grab`, which is a list of lists,
#' each of which has two elements, one named "id" and the other "value".
#' @keywords internal
#'
as.teal_grab <- function(x) { #nolint
  checkmate::assert_list(x, names = "named")

  ans <- lapply(names(x), function(i) {
    if (!inherits(x[[i]], "shinyActionButtonValue")) {
      list(id = i, value = x[[i]])
    }
  })
  ans <- Filter(Negate(is.null), ans)

  excluded_ids <- paste(c("filter_panel", "filter_manager", "snapshot_manager", "state_manager"), collapse = "|")
  included_ids <- grep(excluded_ids, vapply(ans, `[[`, character(1L), "id"), value = TRUE, invert = TRUE)
  ans <- Filter(function(x) x[["id"]] %in% included_ids, ans)

  class(ans) <- c("teal_grab", class(ans))

  ans
}


#' Compare `teal_grab` objects.
#'
#' Performs a set difference adapted for the `teal_grab` class. Returns NULL if the difference is empty.
#'
#' @param x,y `teal_grab` objects
#' @return `teal_grab` or `NULL`, if the difference is empty.
#' @keywords internal
#'
setdiff_teal_grab <- function(x, y) {
  ans <- setdiff(x, y)
  class(ans) <- c("teal_grab", class(ans))
  if (length(ans)) {
    ans
  }
}


#' @keywords internal
#'
get_master_session <- function() {
  local_session <- shiny::getDefaultReactiveDomain()
  app_session <- .subset2(local_session, "parent")
  if (is.null(app_session)) {
    local_session
  } else {
    app_session
  }
}


#' Special consideration for datetimes which are handled by `airDatepickerInput`.
#' `POSIXct` is expressed in milliseconds and converted to a JSON representation.
#' Apparently this is the only way for the input widget to accept data.
#'
#' Adapted from `shinyWidgets`.
#'
#' @section Warning:
#' Potential vulnerability if a different date time widget is used.
#'
#' @source [`shinyWidgets::updateAirDateInput`]
#' @keywords internal
#'
posix_ms_to_json <- function(x) {
  x <- if (!is.null(x)) {
    1000 * as.numeric(as.POSIXct(as.character(x), tz = Sys.timezone()))
  }
  as.character(jsonlite::toJSON(x = x, auto_unbox = FALSE))
}
