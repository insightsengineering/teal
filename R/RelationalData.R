## RelationalData ====
#' @title Manage multiple `RelationalDataConnector`, `DatasetConnector` and `Dataset` objects.
#' @description
#' Class manages `RelationalDataConnector`, `DatasetConnector` and
#' `Dataset` objects and aggregate them in one collection.
#' Class also decides whether to launch app before initialize teal application.
#'
#' @param ... (`RelationalDataConnector`, `Dataset`, `DatasetConnector`)\cr
#'   objects
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with dataset column relationships used for joining.
#'   If empty then no joins between pairs of objects
#'
#' @examples
#' library(random.cdisc.data)
#' x1 <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#' x2 <- rcd_data(
#'   rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
#' )
#' x3 <- cdisc_dataset(
#'   dataname = "ADAE",
#'   x = radae(cached = TRUE),
#'   code = "library(random.cdisc.data)\nADAE <- radae(cached = TRUE)"
#' )
#'
#' x4 <- rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
#' tc <- teal:::RelationalData$new(x1, x2, x3, x4)
#' tc$get_datanames()
#' \dontrun{
#' tc$get_datasets()
#' tc$get_dataset("ADAE")
#' }
#' tc$get_items()
#' tc$get_code()
#' tc$get_code("ADAE")
#' \dontrun{
#' tc$launch()
#' tc$get_datasets()
#' tc$get_dataset("ADAE")
#' tc$check()
#' }
#'
#' library(random.cdisc.data)
#' x <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = radsl(cached = TRUE),
#'   code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)"
#' )
#'
#' x2 <- rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
#' tc <- teal:::RelationalData$new(x, x2)
#' \dontrun{
#' tc$get_datasets()
#' get_raw_data(tc)
#' tc$launch()
#' get_raw_data(tc)
#' }
#' @importFrom R6 R6Class
#' @importFrom shinyjs hide
#' @importFrom methods is
RelationalData <- R6::R6Class( # nolint
  classname = "RelationalData",
  inherit = DataAbstract,
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `RelationalData` class
    initialize = function(..., join_keys) {
      dot_args <- list(...)
      allowed_classes <- c("RelationalDataConnector", "Dataset", "DatasetConnector")

      is_teal_data <- is_any_class_list(dot_args, allowed_classes)
      if (!all(is_teal_data)) {
        stop("All elements should be of Dataset(Connector) or RelationalDataConnector class")
      }

      if (missing(join_keys)) {
        join_keys <- teal::join_keys()
      }
      if (is(join_keys, "JoinKeySet")) {
        join_keys <- teal::join_keys(join_keys)
      }
      stopifnot(is(join_keys, "JoinKeys"))

      datanames <- ulapply(dot_args, get_dataname)
      private$check_names(datanames)

      private$datasets <- dot_args

      private$pull_code <- CodeClass$new()
      private$mutate_code <- CodeClass$new()

      if (length(self$get_connectors()) > 0) {
        private$set_ui()
        private$set_server()
      }

      private$join_keys <- join_keys
      # fill in primary keys as a join keys to self
      for (i in datanames) {
        if (is_empty(self$get_join_keys()$get(i, i))) {
          self$mutate_join_keys(i, i, get_keys(self$get_items(i)))
        }
      }

      return(invisible(self))
    },

    # ___ getters ====
    #' @description
    #'
    #' Derive the names of all datasets
    #' @return (`character` vector) with names
    get_datanames = function() {
      datasets_names <- ulapply(private$datasets, get_dataname)

      return(datasets_names)
    },
    #' @description
    #' Get `JoinKeys` object with keys used for joining.
    #' @return (`JoinKeys`)
    get_join_keys = function() {
      private$join_keys
    },
    #' Get data connectors.
    #'
    #' @return (`list`) with all `DatasetConnector` or `RelationalDataConnector` objects.
    get_connectors = function() {
      return(Filter(
        function(x) {
          is(x, "DatasetConnector") || is(x, "RelationalDataConnector")
        },
        private$datasets
      ))
    },
    #' @description
    #'
    #' @description
    #' Get all datasets and all dataset connectors
    #'
    #'   name of dataset connector to be returned. If \code{NULL}, all connectors are returned.
    #' @param dataname (\code{character} value)\cr
    #'
    #' @return \code{list} with all datasets and all connectors
    get_items = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))

      get_sets <- function(x) {
        if (is(object = x, class2 = "RelationalDataConnector")) {
          x$get_items()
        } else {
          x
        }
      }

      sets <- ulapply(private$datasets, get_sets)
      names(sets) <- vapply(sets, get_dataname, character(1))

      if (is_character_single(dataname)) {
        if (!(dataname %in% self$get_datanames())) {
          stop(paste("dataset", dataname, "not found"))
        }
        return(sets[[dataname]])
      } else {
        return(sets)
      }
    },

    # ___ shiny ====

    #' @description
    #'
    #' Get a shiny-module UI to render the necessary app to
    #' derive \code{RelationalDataConnector} object's data
    #'
    #' @param id (\code{character}) item ID for the shiny module
    #' @return the \code{shiny} \code{ui} function
    get_ui = function(id) {
      if (is.null(private$ui)) {
        div(id = id, "Data Loaded")
      } else {
        private$ui(id)
      }
    },
    #' @description
    #'
    #' Get a shiny-module server to render the necessary app to
    #' derive \code{RelationalDataConnector} object's data
    #'
    #' @return \code{shiny} \code{server} module.
    get_server = function() {
      if (is.null(private$server)) {
        return(function(input, output, session) {
          reactive(self)
        })
      } else {
        private$server
      }
    },
    #' @description
    #'
    #' Launch an app that allows to run the user-interfaces of all
    #' \code{DataConnector} and \code{DatasetConnector} modules
    #'
    #' This piece is mainly used for debugging.
    launch = function() {
      # if no data connectors can append any dataset connectors
      # and not load an app
      if (self$is_pulled()) {
        stop("All the datasets have already been pulled.")
      }

      # otherwise load RelationDataConnector and
      # DatasetConnector with shiny app
      shinyApp(
        ui = fluidPage(
          fluidRow(
            column(
              width = 8,
              offset = 2,
              private$ui(id = "main_app"),
              shinyjs::hidden(
                tags$div(
                  id = "data_loaded",
                  div(
                    h3("Data successfully loaded."),
                    p("You can close this window and get back to R console.")
                  )
                )
              ),
              useShinyjs(),
              include_teal_css_js(),
              br()
            )
          )
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          dat <- callModule(private$server, id = "main_app")

          observeEvent(dat(), {
            if (self$is_pulled()) {
              shinyjs::show("data_loaded")
              stopApp()
            }
          })
        }
      )
    },

    # ___ mutate ====
    #' @description
    #' Change join_keys for a given pair of dataset names
    #' @param dataset_1,dataset_2 (`character`) datasets for which join_keys are to be returned
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate_join_keys = function(dataset_1, dataset_2, val) {
      self$get_join_keys()$mutate(dataset_1, dataset_2, val)
      return(invisible(self))
    },

    # ___ check ====
    #' @description
    #' Check there is consistency between the datasets and join_keys
    #' @return raise and error or invisible `TRUE`
    check_metadata = function() {

      for (dataset in self$get_datasets()) {

        dataname <- get_dataname(dataset)
        dataset_colnames <- dataset$get_colnames()

        # expected columns in this dataset from JoinKeys specification
        join_key_cols <- unique(ulapply(self$get_join_keys()$get(dataname), names))
        if (!is.null(join_key_cols) && !all(join_key_cols %in% dataset_colnames)) {
          stop(
            paste(
              "The join key specification requires dataset",
              dataname,
              "to contain the following columns:",
              paste(join_key_cols, collapse = ", ")
            )
          )
        }

        # check if primary keys in dataset
        primary_key_cols <- get_keys(dataset)
        if (!is.null(primary_key_cols) && !all(primary_key_cols %in% dataset_colnames)) {
          stop(
            paste(
              "The primary keys specification requires dataset",
              dataname,
              "to contain the following columns:",
              paste(primary_key_cols, collapse = ", ")
            )
          )
        }
      }
      return(invisible(TRUE))
    }
  ),

  ## __Private Fields ====
  private = list(
    ui = NULL,
    server = NULL,
    join_keys = NULL, # JoinKeys after initialization
    ## __Private Methods ====
    set_ui = function() {
      private$ui <- function(id) {
        ns <- NS(id)

        # connectors ui(s) + submit button
        fluidPage(
          shinyjs::hidden(
            column(
              id = ns("delayed_data"),
              width = 8,
              offset = 2,
              div(
                tagList(
                  lapply(
                    private$datasets,
                    function(x) {
                      div(
                        if (!is(x, c("DatasetConnector", "RelationalDataConnector"))) {
                          div(h4("Data(set) for: ", lapply(x$get_datanames(), code)), p(icon("check"), "Loaded"))
                        } else {
                          if_null(
                            x$get_ui(id = ns(paste0(x$get_datanames(), collapse = "_"))),
                            div(
                              h4("Dataset Connector for: ", lapply(x$get_datanames(), code)),
                              p(icon("check"), "Ready to Load")
                            )
                          )
                        },
                        br()
                      )
                    }
                  ),
                  actionButton(inputId = ns("submit"), label = "Submit all")
                ),
                `data-proxy-click` = ns("submit")
              )
            )
          )
        )
      }
    },
    set_server = function() {
      private$server <- function(input, output, session) {
        shinyjs::show("delayed_data")
        rv <- reactiveVal(NULL)
        observeEvent(input$submit, {
          # load data from all connectors
          for (dc in self$get_connectors()) {
            if (is(dc, class2 = "RelationalDataConnector")) {
              callModule(dc$get_server(),
                         id = paste0(dc$get_datanames(), collapse = "_"),
                         connection = dc$get_connection(),
                         connectors = dc$get_items()
              )

            } else if (is(dc, class2 = "DatasetConnector")) {
              callModule(dc$get_server(),
                         id = dc$get_dataname()
              )
            }
            if (dc$is_failed()) {
              break
            }
          }


          if (self$is_pulled()) {
            shinyjs::hide("delayed_data")
            rv(self)
          }
        })
        return(rv)
      }
    }
  )
)

## Functions ====
is_any_class_list <- function(x, class) {
  vapply(
    x,
    function(xx) {
      any(
        vapply(
          class,
          function(class_name) {
            is(object = xx, class2 = class_name)
          },
          logical(1)
        )
      )
    },
    logical(1)
  )
}
