# This is the main function from teal to be used by the end-users. Although it delegates
# directly to `module_teal_with_splash.R`, we keep it in a separate file because its doc is quite large
# and it is very end-user oriented. It may also perform more argument checking with more informative
# error messages.


#' Create the Server and UI Function For the Shiny App
#'
#' @description `r lifecycle::badge("maturing")`
#' End-users: This is the most important function for you to start a
#' teal app that is composed out of teal modules.
#'
#' **Notes for developers**:
#' This is a wrapper function around the `module_teal.R` functions. Unless you are
#' an end-user, don't use this function, but instead this module.
#'
#' @param data (`RelationalData` or `list` or `data.frame`) R6 object where \code{\link{cdisc_data}}
#' or \code{\link{teal_data}} returns such a one or a list of data frames and datasets as returned
#' by \code{\link{cdisc_dataset}} or \code{\link{dataset}} or a list of data frames and the previous
#' objects or a single data frame.\cr
#' NOTE: when a \code{list} or \code{data.frame} alone is provided, teal does not guarantee
#' reproducibility of the code when names of the list elements does not match the original object
#' names. To ensure reproducibility please use \code{\link{teal_data}} or \code{\link{cdisc_data}}
#' with `check = TRUE` enabled.
#' @param modules nested list with one list per module with the
#'   following named list elements:
#'   \tabular{ll}{
#'   \cr name \tab string with name shown in menu for the analysis item
#'   \cr server \tab required, shiny server module function, see
#'   `\link[shiny]{callModule}` for more information
#'   \cr ui \tab required, shiny ui module function, see
#'   `\link[shiny]{callModule}` for more information. Note, due to the app's initialization by teal,
#'   this `ui` function is actually part of the `server` function of the shiny app which means code which must
#'   be included in the shiny `ui` function (such as using `htmltools::htmlDependency`) should not be placed here
#'   but should instead be placed in the `header` argument to `teal::init` as that is included in the shiny `ui`
#'   function.
#'   \cr data \tab required, vector with datasets names that are passed
#'   on (filtered) to the server function
#'   \cr options \tab optional, other arguments passed on to the server
#'   function
#'   }
#' @param title (`NULL` or `character`) The browser window title (defaults to the host URL of the page).
#' @param filter (`list`) You can define filters that show when
#'   the app starts.
#'   The general pattern is:
#'   `list(iris = list(Species = ..., Sepal.Length = ...), mtcars = ...)`.
#'   An example is:
#'   `list(iris = list(Species = c("setosa", "versicolor")))`.
#'   More generally, the filters for the variable, e.g. `Species` can be
#'   specified as follows:
#'   `list(Species = c("setosa", "versicolor"))`,
#'   `list(Species = list(c("setosa", "versicolor"), keep_na = TRUE))`,
#'   or equivalently with:
#'   `list(Species = c("setosa", "versicolor", NA))`,
#'   or for the default filter (not very restrictive):
#'   `list(Species = default_filter())`
#'
#'   Instead of `choices` above, use the following names:
#'   - `numerical`: `range`
#'   - `factor`: `choices`
#'   - `logical`: `logical`
#'   A general example is:
#'   `list(
#'     iris = list(Sepal.Length = default_filter(), Species = c("M", NA)),
#'     mtcars = list(mpg = default_filter())
#'   )`
#'   Ignored if the app is restored from a bookmarked state.
#' @param header (`character` or `shiny.tag`) the header of the app. Note shiny code placed here (and in the footer
#' argument) will be placed in the app's `ui` function so code which needs to be placed in the `ui` function
#' (such as loading css via `htmltools::htmlDependency`) should be included here.
#' @param footer (`character` or `shiny.tag`) the footer of the app
#' @param id (`character`) module id to embed it, if provided,
#'   the server function must be called with `callModule`;
#'   See the vignette for an example. However, \code{\link{ui_teal_with_splash}}
#'   is then preferred to this function.
#'
#' @return named list with server and ui function
#'
#' @export
#'
#' @include FilteredData.R
#' @include modules.R
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' options(teal_logging = FALSE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#'   ),
#'   modules = root_modules(
#'     module(
#'       "data source",
#'       server = function(input, output, session, datasets) {},
#'       ui = function(id, ...) div(p("information about data source")),
#'       filters = "all"
#'     ),
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'           hist(datasets$get_data("ADSL", filtered = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {
#'         ns <- NS(id)
#'         plotOutput(ns("hist"))
#'       },
#'       filters = "ADSL"
#'     )
#'   ),
#'   title = "App title",
#'   filter = list(ADSL = list(AGE = default_filter())),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017 - 2020")
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' # or: to also work with bookmarking
#' bookmarkableShinyApp(app$ui, app$server)
#' }
#'
#' # See the vignette for an example how to embed this app as a module
#' # into a larger application
init <- function(data,
                 modules,
                 title = NULL,
                 filter = list(),
                 header = tags$p("Add Title Here"),
                 footer = tags$p("Add Footer Here"),
                 id = character(0)) {
  if (!is(data, "RelationalData")) {
    data <- to_relational_data(data = data, data_call = substitute(data))
  }

  stopifnot(
    is(data, "RelationalData"),
    is(modules, "list") || is(modules, "teal_modules"),
    is.null(title) || is_character_single(title),
    is_fully_named_list(filter),
    all(names(filter) %in% get_dataname(data)),
    is_character_vector(id, min_length = 0, max_length = 1)
  )

  log_system_info()

  if(is(modules, "list"))  modules <- do.call(root_modules, modules)

  # Note regarding case `id = character(0)`:
  # rather than using `callModule` and creating a submodule of this module, we directly modify
  # the `ui` and `server` with `id = character(0)` and calling the server function directly
  # rather than through `callModule`
  res <- list(
    ui = ui_teal_with_splash(id = id, data = data, title = title, header = header, footer = footer),
    server = function(input, output, session) {
      # copy object so that load won't be shared between the session
      data <- data$clone(deep = TRUE)
      srv_teal_with_splash(
        input, output, session,
        data = data, modules = modules, filter = filter
      )
    }
  )
  return(res)
}


#' Make a Shiny UI function bookmarkable
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is a customization of `shinyApp`.
#'
#' To be bookmarkable, the Shiny UI function must have an
#' argument `request`. This function ensures this.
#'
#' When `ui` is a function, it passes the following to `shinyApp`
#' ```
#' app <- teal::init(....)
#' ui <- app$ui
#' ui_new <- function(request) {
#'   ui() # or just `ui` when ui is already evaluated, e.g. `shiny.tag`
#' }
#' ```
#'
#' If no bookmarking is needed for teal apps, then you can also call
#' `shinyApp(ui = app$ui, server = app$server)`, where `app` is returned
#' by `init()`.
#'
#' **For Developers: **
#' The reason you cannot
#' call `shinyApp(ui = app$ui, server = app$server)` without parentheses is
#' that `app$ui` has an `id` argument with a default value which makes it
#' possible to be added into modules. `shinyApp` thinks that this is the request
#' argument which is needed for bookmarking. This avoids it.
#'
#' We guarantee that anything that can be run with `shinyApp` can be replaced
#' by a call to this function without any changes.
#'
#' @param ui `function or shiny.tag` Shiny UI; either a
#'   `shiny.tag` or a function with no argument or
#'   one argument (`request`)
#' @param server `function` Shiny server function
#' @param ... additional arguments to `shinyApp`
#' @return `shinyApp` value
#' @export
bookmarkableShinyApp <- function(ui, server, ...) { #nolint
  # ui must be a function of request to be bookmarkable
  ui_new <- function(request) {
    # we use similar logic to `shiny:::uiHttpHandler`
    if (is.function(ui)) {
      # evaluating ui with default arguments
      ui()
    } else {
      stopifnot(is_html_like(ui))
      ui
    }
  }
  return(shinyApp(ui = ui_new, server = server, ...))
}
