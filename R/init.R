#' Create the Server and Ui Function for the Shiny App
#'
#' Creates the server and ui part for a teal shiny app
#'
#' @param data named list with datasets. Dataset names are case sensitive. The
#'   `ADSL` data is mandatory.
#' @param modules nested list with one list per module with the
#'   following named list elements: \tabular{ll}{ name \tab string with name
#'   shown in menu for the analysis item \cr server \tab required, shiny server
#'   module function, see \code{\link[shiny]{callModule}} for more
#'   information\cr ui \tab required, shiny ui module function, see
#'   \code{\link[shiny]{callModule}} for more information\cr data \tab required,
#'   vector with datasets names that are passed on (filtered) to the server
#'   function\cr options \tab optional, other arguments passed on to the server
#'   function }
#' @param filter (\code{list}) You can pre-define filters for
#'   datasets inside this argument. Therefore you need to handover an
#'   \code{init} list. Please provide a named list inside \code{init}
#'   that contains the names of the datasets. E.g. for filtering
#'   the dataset \code{ADSL} use \code{list(init = list(ADSL = ...))}.
#'   For each datasets you need to provide a vector with column names that are
#'   relevant for the item. You can specify an ADSL filtering for the
#'   columns \code{SEX} and \code{BAGE} by:
#'
#'   \code{filter = list(init = list(ADSL = c("SEX", "BAGE")))}
#' @param header object of class `shiny.tag` to be used as the header of the app
#' @param footer object of class `shiny.tag` to be used as the footer of the app
#'
#' @return named list with server and ui function
#'
#' @export
#'
#' @importFrom shinyjs useShinyjs hidden hide show
#' @importFrom methods is
#'
#' @include FilteredData.R
#' @include modules.R
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' options(teal_logging = FALSE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "
#'       ADSL <- radsl(seed = 1)
#'     "
#'   ),
#'   modules = root_modules(
#'     module(
#'       "data source",
#'       server = function(input, output, session, datasets) {},
#'       ui = function(id, ...) div(p("information about data source")),
#'       filters = 'all'
#'     ),
#'     module(
#'       "ADSL AGE histogram",
#'       server = function(input, output, session, datasets) {
#'         output$hist <- renderPlot(
#'            hist(datasets$get_data("ADSL", filtered = TRUE, reactive = TRUE)$AGE)
#'         )
#'       },
#'       ui = function(id, ...) {ns <- NS(id); plotOutput(ns('hist'))},
#'       filters = "ADSL"
#'     )
#'   ),
#'   filter = list(init = list(ADSL = c("AGE"))),
#'   header = tags$h1("Sample App"),
#'   footer = tags$p("Copyright 2017")
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
init <- function(data,
                 modules,
                 filter = NULL,
                 header = tags$p("title here"),
                 footer = tags$p("footer here")) {

  if (modules_depth(modules) > 2) {
    stop("teal currently only supports module nesting of depth two.")
  }

  if (!is(data, "cdisc_data")) {
    warning("Please use cdisc_data() instead of list() for 'data' argument. It will be depreciated soon.")

    data_names <- names(data)
    stopifnot(length(data_names) == length(data))

    data_substitute <- substitute(data)
    data <- eval(as.call(append(
      quote(cdisc_data),
      lapply(
        seq_along(data),
        function(idx) {
          call(
            "cdisc_dataset",
            dataname = data_names[[idx]],
            data = data_substitute[[idx + 1]]
          )
        }
      )
    )))

  }

  check_module_names(modules)

  # initialize FilteredData object
  datasets <- FilteredData$new(vapply(data, `[[`, character(1), "dataname", USE.NAMES = FALSE))
  for (idx in seq_along(data)) {
    datasets$set_data(data[[idx]][["dataname"]], data[[idx]][["data"]])
    datasets$set_data_attr(data[[idx]][["dataname"]], "keys", data[[idx]][["keys"]])
    datasets$set_data_attr(data[[idx]][["dataname"]], "labels", data[[idx]][["labels"]])
  }

  # including attributes of data object
  datasets$set_attrs(data)

  # set default init filters
  if (!is.null(filter) && !is.null(filter$init)) {
    Map(function(vars, dataset) {
      lapply(vars, function(var) datasets$set_default_filter_state(dataset, var))
    }, filter$init, names(filter$init))
  }

  # ui function
  ui <- shinyUI(
      fluidPage(
        shinyjs::useShinyjs(),
        include_css_files(package = "teal"),
        include_js_files(package = "teal", except = "init.js"),
        shinyjs::hidden(icon("cog")), # add hidden icon to load font-awesome css for icons
        tags$header(header),
        tags$hr(style = "margin: 7px 0;"),
        local({
          tp <- create_ui(modules, datasets, idprefix = "teal_modules", is_root = TRUE)

          # separate the nested tabs
          tp$children <- list(
            tp$children[[1]],
            tags$hr(style = "margin: 7px 0;"),
            fluidRow(
              column(
                9,
                tp$children[[2]]
              ),
              column(
                3,
                shinyjs::hidden(
                  div(
                    id = "teal_filter-panel",
                    div(
                      id = "teal_filter_active_vars",
                      class = "well",
                      tags$label(
                        "Active Filter Variables",
                        class = "text-primary",
                        style = "margin-bottom: 15px;"
                      ),
                      tagList(
                        lapply(datasets$datanames(), function(dataname) {
                          ui_filter_info(paste0("teal_filters_info_", dataname), dataname)
                        })
                      ),
                      tagList(
                        lapply(datasets$datanames(), function(dataname) {
                          ui_filter_items(paste0("teal_filters_", dataname), dataname)
                        })
                      )
                    ),
                    div(
                      id = "teal_filter_add_vars",
                      class = "well",
                      tags$label(
                        "Add Filter Variables",
                        class = "text-primary",
                        style = "margin-bottom: 15px;"
                      ),
                      tagList(
                        lapply(datasets$datanames(), function(dataname) {
                          ui_add_filter_variable(paste0("teal_add_", dataname, "_filters"), dataname)
                        })
                      )
                    )
                  )
                )
              )
            )
          )
          tp
        }),
        tags$hr(),
        tags$footer(footer)
      )
  )


  server <- function(input, output, session) {

    run_js_file(file = "init.js", package = "teal")

    # evaluate the server functions
    call_modules(modules, datasets, idprefix = "teal_modules")

    # -- filters
    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_filter_items, paste0("teal_filters_", dataname), datasets, dataname)
    })
    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_filter_info, paste0("teal_filters_info_", dataname), datasets, dataname)
    })

    adsl_vars <- names(datasets$get_data("ADSL"))
    lapply(datasets$datanames(), function(dataname) {
      callModule(srv_add_filter_variable, paste0("teal_add_", dataname, "_filters"), datasets, dataname,
                 omit_vars = if (dataname == "ADSL") character(0) else adsl_vars)
    })

    ## hide-show filters based on module filter property
    recurse <- function(x, idprefix) {
      id <- label_to_id(x$label, idprefix)

      if (is(x, "teal_module")) {
        setNames(list(x$filters), id)
      } else if (is(x, "teal_modules")) {
        unlist(
          mapply(function(i) recurse(i, idprefix = id), x$modules, USE.NAMES = TRUE, SIMPLIFY = FALSE),
          recursive = FALSE
        )
      }
    }

    # named vector with ids and datasets
    filters_tab_lookup <- recurse(modules, "teal_modules")

    recurse_modules <- function(x, idprefix) {
      id <- label_to_id(x$label, idprefix)

      if (is(x, "teal_modules")) {
        c(id, lapply(x$modules, recurse_modules, idprefix = id))
      } else {
        NULL
      }
    }
    id_modules <- unlist(recurse_modules(modules, "teal_modules"))

    ## now show or hide the filter panels based on active tab
    observe({
      # define reactivity dependence
      main_tab <- input[["teal_modules_root"]]
      secondary_tabs <- sapply(id_modules, function(id) input[[id]], USE.NAMES = TRUE)

      # figure out which is the active tab/module
      main_tab_id <- label_to_id(main_tab, "teal_modules_root")

      active_module_id <- if (main_tab_id %in% id_modules) {
        label_to_id(secondary_tabs[[main_tab_id]], main_tab_id)
      } else {
        main_tab_id
      }

      filters <- filters_tab_lookup[[active_module_id]]
      .log("Active filter for tab", active_module_id, "is", if_null(filters, "[empty]"))

      if (is.null(filters)) {
        shinyjs::hide("teal_filter-panel")
      } else {

        shinyjs::show("teal_filter-panel")

        if ("all" %in% filters) {
          lapply(datasets$datanames(), function(dataname) {
            shinyjs::show(paste0("teal_filter_", dataname))
          })
        } else {
          lapply(
            datasets$datanames(),
            function(dataname) {
              id <- paste0("teal_filter_", dataname)
              if (dataname == "ADSL" || dataname %in% filters) {
                shinyjs::show(id)
              } else {
                shinyjs::hide(id)
              }
            }
          )
        }
      }

    })

  }

  list(server = server, ui = ui, datasets = datasets)
}
