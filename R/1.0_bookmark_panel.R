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
#' The bookmark manager presents a button with the bookmark icon and is placed in the [`wunder_bar`].
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
#' @inheritParams module_wunder_bar
#'
#' @return Invisible `NULL`.
#'
#' @aliases bookmark bookmark_manager bookmark_manager_module
#'
#' @name module_bookmark_manager
#' @keywords internal
#'
ui_bookmark_panel <- function(id, modules) {
  ns <- NS(id)

  bookmark_option <- get_bookmarking_option()
  is_unbookmarkable <- need_bookmarking(modules)

  # Render bookmark warnings count
  if (!all(is_unbookmarkable) && identical(bookmark_option, "server")) {
    tags$button(
      id = ns("do_bookmark"),
      class = "btn action-button wunder_bar_button bookmark_manager_button",
      title = "Add bookmark",
      tags$span(
        suppressMessages(icon("solid fa-bookmark")),
        if (any(is_unbookmarkable)) {
          tags$span(
            sum(is_unbookmarkable),
            class = "badge-warning badge-count text-white bg-danger"
          )
        }
      )
    )
  }
}

#' @rdname module_bookmark_manager
#' @keywords internal
#'
srv_bookmark_panel <- function(id, modules) {
  checkmate::assert_character(id)
  checkmate::assert_class(modules, "teal_modules")
  moduleServer(id, function(input, output, session) {
    logger::log_trace("bookmark_manager_srv initializing")
    ns <- session$ns
    bookmark_option <- get_bookmarking_option()
    is_unbookmarkable <- need_bookmarking(modules)

    # Set up bookmarking callbacks ----
    # Register bookmark exclusions: do_bookmark button to avoid re-bookmarking
    setBookmarkExclude(c("do_bookmark"))
    # This bookmark can only be used on the app session.
    app_session <- .subset2(shiny::getDefaultReactiveDomain(), "parent")
    app_session$onBookmarked(function(url) {
      logger::log_trace("bookmark_manager_srv@onBookmarked: bookmark button clicked, registering bookmark")
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
        modalDialog(
          id = ns("bookmark_modal"),
          title = "Bookmarked teal app url",
          modal_content,
          easyClose = TRUE
        )
      )
    })

    # manually trigger bookmarking because of the problems reported on windows with bookmarkButton in teal
    observeEvent(input$do_bookmark, {
      logger::log_trace("bookmark_manager_srv@1 do_bookmark module clicked.")
      session$doBookmark()
    })

    invisible(NULL)
  })
}


#' @rdname module_bookmark_manager
#' @keywords internal
get_bookmarking_option <- function() {
  bookmark_option <- getShinyOption("bookmarkStore")
  if (is.null(bookmark_option) && identical(getOption("shiny.bookmarkStore"), "server")) {
    bookmark_option <- getOption("shiny.bookmarkStore")
    # option alone doesn't activate bookmarking - we need to set shinyOptions
    shinyOptions(bookmarkStore = bookmark_option)
  }
  bookmark_option
}

#' @rdname module_bookmark_manager
#' @keywords internal
need_bookmarking <- function(modules) {
  unlist(rapply2(
    modules_bookmarkable(modules),
    Negate(isTRUE)
  ))
}
