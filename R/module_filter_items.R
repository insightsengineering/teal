#' Creates UI to show currently selected filters that can be applied to
#' a single dataset, i.e. all columns selected for filtering are shown.
#'
#' It reacts to changes in the filter variable names as provided by
#' the `datasets` class. To filter a new variable, it is added to the
#' filter state in the `datasets` class. This then triggers this module
#' to insert a corresponding UI based on the filter state and filter info.
#' Once the variable is removed, it is removed from the UI as well.
#'
#' Note: Any changes to the filter state and filter info while this element is
#' visible in the UI are not reflected in the UI. To achieve this, you have to
#' remove the element from the filtered states in `datasets` and then add it
#' again. While doing this, it is good practice to make the UI element for this
#' variable invisible (e.g. with `shinyjs::hide`) as otherwise there will be two
#' triggerings of the input reactives of the servers, first the one corresponding
#' to user changes to the inputs while the UI was updated, then the correct new
#' input values (as sent with the new UI).
#'
#' @param id module id
#' @param dataname `character` dataname to show filters for
#'
#' @examples
#' # Example with ADSL and ADAE dataset
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#' ADAE <- radlb(cached = TRUE)
#' attr(ADAE, "keys") <- get_cdisc_keys("ADAE")
#'
#' datasets <- teal:::FilteredData$new()
#' isolate({
#'   datasets$set_data("ADSL", ADSL)
#'   datasets$set_filter_state("ADSL", list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE),
#'     SEX = list(choices = "M", keep_na = TRUE)
#'   ))
#'   datasets$set_data("ADAE", ADAE)
#'   datasets$set_filter_state("ADAE", list(
#'     CHG = list(range = c(20, 35), keep_na = FALSE)
#'   ))
#' })
#'
#' app <- shinyApp(ui = function() {
#'   tagList(
#'     teal:::include_teal_css_js(),
#'     teal:::ui_filter_items("filter_ADSL", "ADSL"),
#'     teal:::ui_filter_items("filter_ADAE", "ADAE")
#'   )
#' }, server = function(input, output, session) {
#'   callModule(teal:::srv_filter_items, "filter_ADSL", datasets, "ADSL")
#'   callModule(teal:::srv_filter_items, "filter_ADAE", datasets, "ADAE")
#' })
#' \dontrun{
#' runApp(app)
#' }
ui_filter_items <- function(id, dataname) {
  stopifnot(
    is_character_single(dataname)
  )

  ns <- NS(id)
  div(
    id = ns("whole_ui"), # to hide it entirely
    fluidRow(
      column(8, tags$span(dataname, class = "filter_panel_dataname")),
      column(4, actionLink(
        ns("remove_filters"), "", icon("times-circle", lib = "font-awesome"),
        class = "remove pull-right"
      ))
    ),
    div(
      # id needed to insert and remove UI to filter single variable as needed
      # it is currently also used by the above module to entirely hide this panel
      id = ns("filters"),
      class = "listWithHandle list-group" # to make every element in here draggable
    )
  )
}

#' Server function to display filters for a dataset
#'
#' @inheritParams srv_shiny_module_arguments
#' @inheritParams ui_filter_items
#' @param drop_levels `reactive` the reactive logical flag indicating whether
#'   to drop unused levels of variables
#' @return `reactive` returning a named list, names being the shown variables
#'   and values being the observers for the modules associated to that variable
#'   (by calling a Shiny submodule for each variable). This is useful for dynamic
#'   UI generation, so observers can be properly canceled. Note that currently, not
#'   all observers defined in this module are returned (the observer to remove all
#'   filters at once is not returned)
#'
srv_filter_items <- function(input, output, session, datasets, dataname, drop_levels) {
  stopifnot(
    is(datasets, "FilteredData"),
    is_character_single(dataname)
  )

  # Shiny does not implement the Model View Controller interface. The input elements in Shiny
  # are simultaneously controllers and views. Furthermore, when the model (input value) first
  # starts to exist, it is not removed when the UI is removed, instead it must be set manually
  # to NULL (possible through Javascript).
  # When Shiny renders an input element, e.g. created with `selectInput()`, you have to provide
  # initial values even if an element with the same id already exists on the page. This then updates
  # the model and triggers on the server.
  # This makes dynamic generation of the UI challenging because race conditions occur when the user
  # updates the input element while the server is computing and then sending a response to recreate
  # the input element, but using the old user selection.
  # There are two ways to proceed:
  # 1. The server always takes priority. If the user changes the input while the server is computing,
  #    the server may discard the user changes and instead enforce its outdated model onto the input
  #    element. Infinite loops easily happen when the server computes, the client sends an updated
  #    input, the server updates the UI, receives the new client input and computes. At the same time,
  #    however, the client sends the outdated values that it just received from the server to the
  #    server again and the cycle completes indefinitely (if latency does not come to the rescue after
  #    many iterations). At any time, the server and the client have outsynced models, the server is
  #    behind.
  #    A way to avoid this is to add timestamps to the input ids to effectively discard user inputs
  #    in the meantime.
  # 2. The server only updates the element when it is hidden or inexistent. This avoids the race
  #    condition. To still be able to update the input element from the server, we should call
  #    `updateSelectInput()` and isolate this call to avoid infinite loops. To dynamically add/remove
  #    UI to an already existing UI (e.g. filter items with already some filter variables),
  #    `insertUI` and `removeUI` should be used.
  # We prefer the 2nd option and implement it here.

  # We here choose to create a UI that listens to changes in the names of the filter variables and
  # calls `insertUI` for any variables that were added to filter and `removeUI` for variables
  # that should no longer be filtered. For this, we keep a list of the currently shown filters in the UI.
  #
  # When we insert a UI with an associated server function, we must delete the observers
  # registered in the server function when we remove it again as they will otherwise keep
  # listening. This also becomes problematic when the UI is added again. Not only will the observers
  # execute twice, but also will the input elements not be reset, e.g. a previously clicked button
  # will keep its value (equal to the number of clicks) as buttons don't reset their counts (it seems).

  # named list of variables that are shown with each item containing the associated observers
  # that must be destroyed once the UI is removed
  shown_vars_observers <- NULL
  # variables to filter according to datasets state
  filtered_vars <- reactive(get_filter_vars(datasets, dataname = dataname))
  filter_id_for_var <- function(varname) paste0("filter_", digest::digest(varname))

  observeEvent(
    filtered_vars(), {
      # this block has an effect whenever the shown variable filters differ from the datasets state
      .log("regenerating ui filters for data", dataname)
      # add variables not shown currently
      added_varnames <- setdiff(filtered_vars(), names(shown_vars_observers))
      lapply(added_varnames, function(varname) {
        filter_id <- session$ns(filter_id_for_var(varname))
        insertUI(
          selector = paste0("#", session$ns("filters")),
          where = "beforeEnd",
          # add span with id to be removable
          ui = span(
            id = filter_id,
            class = "list-group-item", # to make it draggable
            ui_single_filter_item(
              id = filter_id,
              filter_info = datasets$get_filter_info(dataname, varname),
              filter_state = datasets$get_filter_state(dataname, varname),
              prelabel = varname
            )
          )
        )
        shown_vars_observers <<- c(
          shown_vars_observers,
          setNames(
            list(
              callModule(srv_single_filter_item,
                filter_id_for_var(varname),
                datasets,
                dataname,
                varname,
                drop_levels)
            ),
            varname
          )
        )
      })
      # remove variables that should not be shown anymore
      removed_varnames <- setdiff(names(shown_vars_observers), filtered_vars())
      lapply(removed_varnames, function(varname) {
        removeUI(selector = paste0("#", session$ns(filter_id_for_var(varname))))
        lapply(shown_vars_observers[[varname]]$observers, function(obs) obs$destroy())
        shown_vars_observers[[varname]] <<- NULL
      })
      stopifnot(setequal(filtered_vars(), names(shown_vars_observers)))
    },
    # we also need to find out when there are no filtered variables to hide / show the UI
    ignoreNULL = FALSE
  )

  observeEvent(input$remove_filters, {
    .log("removing all filters for data", dataname)
    lapply(get_filter_vars(datasets, dataname = dataname), function(varname) {
      set_single_filter_state(datasets, dataname = dataname, varname = varname, state = NULL)
    })
  })

  # return observers so you can cancel them in a similar fashion as in this module when integrating this module
  # into another dynamic module
  return(shown_vars_observers)
}
