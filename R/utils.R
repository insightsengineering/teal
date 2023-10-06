#' Get Client Timezone
#'
#' Local timezone in the browser may differ from the system timezone from the server.
#'   This script can be run to register a shiny input which contains information about
#'   the timezone in the browser.
#'
#' @param ns (`function`) namespace function passed from the `session` object in the
#'   Shiny server. For Shiny modules this will allow for proper name spacing of the
#'   registered input.
#'
#' @return (`Shiny`) input variable accessible with `input$tz` which is a (`character`)
#'  string containing the timezone of the browser/client.
#' @keywords internal
get_client_timezone <- function(ns) {
  script <- sprintf(
    "Shiny.setInputValue(`%s`, Intl.DateTimeFormat().resolvedOptions().timeZone)",
    ns("timezone")
  )
  shinyjs::runjs(script) # function does not return anything
  return(invisible(NULL))
}

#' Resolve the expected bootstrap theme
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

include_parent_datanames <- function(dataname, join_keys) {
  parents <- character(0)
  for (i in dataname) {
    while (length(i) > 0) {
      parent_i <- join_keys$get_parent(i)
      parents <- c(parent_i, parents)
      i <- parent_i
    }
  }

  return(unique(c(parents, dataname)))
}

#' Custom modalDialog
#'
#' Alternative to modalDialog. To create custom modalDialogs that can be shown/hidden
#' using JS using the modal `id` without disturbing other modalDialogs
#'
#' @param id (`character(1)`) `shiny` module id for the modalDialog.\cr
#'           Note that this id will be used to show/hide using `shinyjs::show` and `shinyjs::hide`
#' @param modal_args (`list`) list of arguments for the `shiny::modalDialog` function to customize the modal
#' @param ... (`shiny.tag`) shiny UI elements that will be displayed in the modal UI
#'
#' @return (`shiny.tag`) returns `HTML` for Shiny module UI with modifications that allow for custom show/hide
#' @keywords internal
custom_modal_dialog <- function(id, modal_args, ...) {
  stopifnot(
    rlang::is_scalar_character(id),
    is.list(modal_args)
  )
  modal_args <- append(rlang::dots_list(...), modal_args)
  tagList(
    htmltools::tagQuery(rlang::exec(modalDialog, !!!modal_args))$
      removeAttrs("id")$
      addAttrs(id = id, `aria-hidden` = "true", class = "custom-modal", `data-backdrop` = "false")$
      children("div")$
      children("div")$
      children("div")$
      sibling(".modal-footer")$
      find("button")$
      removeAttrs(c("data-dismiss", "data-bs-dismiss"))$
      addAttrs(onclick = paste0("$('#", id, "').modal('hide');"))$
      allTags()
  )
}
