## RelationalDataList ====
#' @title Manage multiple \code{RelationalDataConnector} and \code{RelationalDatasetConnector}
#' @description
#' Class manages  \code{RelationalDataConnector} and \code{RelationalDatasetConnector} objects
#' and aggregate them in one application. Class also decides whether to launch app
#' before initialize teal application.
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_data( # RelationalDataConnector
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#' x2 <- rcd_data( # RelationalDataConnector
#'   rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
#' )
#' x3 <- cdisc_dataset(
#'   dataname = "ADAE", # RelationalDataset
#'   data = radae(cached = TRUE),
#'   code = "library(random.cdisc.data)\nADAE <- radae(cached = TRUE)"
#' )
#'
#' x4 <- rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
#' tc <- teal:::RelationalDataList$new(x, x2, x3, x4)
#' tc$get_datanames()
#' tc$get_datasets()
#' tc$get_dataset("ADAE")
#' tc$get_all_datasets()
#' tc$get_code()
#' tc$get_code("ADAE")
#' \dontrun{
#' tc$launch()
#' tc$get_raw_data()
#' tc$get_datasets()
#' tc$get_dataset("ADAE")
#' tc$check()
#' }
#'
#' library(random.cdisc.data)
#' x <- cdisc_dataset(
#'   dataname = "ADSL", # RelationalDataset
#'   data = radsl(cached = TRUE),
#'   code = "library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)"
#' )
#'
#' x2 <- rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE)
#' tc <- teal:::RelationalDataList$new(x, x2)
#' tc$get_datasets()
#' get_raw_data(tc)
#' \dontrun{
#' tc$launch()
#' get_raw_data(tc)
#' }
#' @importFrom R6 R6Class
#' @importFrom shinyjs hide
#' @importFrom methods is
RelationalDataList <- R6::R6Class( # nolint
  classname = "RelationalDataList",
  inherit = RelationalData,
  ## __Public Methods ====
  public = list(
    #' @param ... (\code{RelationalDataConnector}, \code{RelationalDataset},
    #'  \code{RelationalDatasetConnector}) object
    #'
    initialize = function(...) {
      dot_args <- list(...)
      possible_classes <- c("RelationalData", "RelationalDataConnector",
                            "RelationalDataset", "RelationalDatasetConnector")

      is_teal_data <- is_any_class_list(dot_args, possible_classes)
      if (!all(is_teal_data)) {
        stop("All data elements should be RelationalData(set) or RelationalData(set)Connection")
      }

      # Cannot allow RelationalDataList to be inside dot_args
      # previous check does not capture this as RelationalDataList inherits
      # from RelationalData
      if (any(is_any_class_list(dot_args, "RelationalDataList"))) {
        stop("Data elements cannot be RelationalDataList")
      }

      datanames <- ulapply(dot_args, get_dataname)
      if (any(duplicated(datanames))) {
        stop("Found duplicated dataset names.")
      }

      private$datasets <- dot_args

      private$code <- CodeClass$new()

      if (length(self$get_connectors()) > 0) {
        private$set_ui()
        private$set_server()
      }

      return(invisible(self))
    },
    #' @description
    #'
    #' Derive the names of all datasets
    #' @return \code{character} vector with names
    get_datanames = function() {
      datasets_names <- lapply(
        private$datasets, get_dataname
      )

      return(unlist(datasets_names))
    },
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
    #' Get dataset connectors.
    #'
    #' @return \code{list} with all \code{RelationalDatasetConnector} objects.
    get_dataset_connectors = function() {
      res <- Filter(
        function(x) {
          is(object = x, class2 = "RelationalDatasetConnector")
        },
        private$datasets
      )

      if (!is.null(res)) {
        names(res) <- vapply(res, get_dataname, character(1))
      }
      return(res)
    },
    #' Get data connectors.
    #'
    #' @return \code{list} with all \code{RelationalDataConnector} objects.
    get_data_connectors = function() {
      res <- Filter(
        function(x) {
          is(object = x, class2 = "RelationalDataConnector")
        },
        private$datasets
      )
      return(res)
    },
    #' Get data connectors.
    #'
    #' @return \code{list} with all \code{RelationalDataConnector} objects.
    get_connectors = function() {
      return(Filter(
        function(x) {
          is(object = x, class2 = "RelationalDatasetConnector") || is(object = x, class2 = "RelationalDataConnector")
        },
        private$datasets
      ))
    },
    #' @description
    #' Get \code{list} of \code{RelationalDataset} objects.
    #' @return \code{list} of \code{RelationalDataset}.
    get_datasets = function() {
      datasets <- ulapply(private$datasets, function(x) if (x$is_pulled()) get_datasets(x) else NULL)
      res <- Filter(Negate(is.null), datasets)

      if (!is.null(res)) {
        names(res) <- vapply(res, get_dataname, character(1))
      }
      return(res)
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
    get_all_datasets = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))
      get_sets <- function(x) {
        if (is(object = x, class2 = "RelationalDataConnector")) {
          x$get_dataset_connectors()
        } else if (is(object = x, class2 = "RelationalData")) {
          get_datasets(x)
        } else {
          x
        }
      }
      sets_list <- lapply(private$datasets, get_sets)
      sets <- unlist(sets_list)
      names(sets) <- vapply(sets, get_dataname, character(1))
      if (is.null(dataname)) {
        return(sets)
      } else {
        sets[[dataname]]
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
      # RelationalDatasetConnector with shiny app
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
              `if`(private$.check && !self$check(), stop("Reproducibility check failed."))
              stopApp()
            }
          })
        }
      )
    }
  ),

  ## __Private Fields ====
  private = list(
    ui = NULL,
    server = NULL,

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
              tagList(
                lapply(
                  private$datasets,
                  function(x) {
                    div(
                      if (!is(x, c("RelationalDatasetConnector", "RelationalDataConnector"))) {
                        div(
                          h4("Data(set) for: ", lapply(x$get_datanames(), code)),
                          p(icon("check"), "Loaded")
                          )
                        } else {
                          if_null(
                            x$get_ui(
                              id = ns(paste0(x$get_datanames(), collapse = "_"))
                              ),
                            div(
                              h4("Dataset Connector for: ", lapply(x$get_datanames(), code))
                              )
                            )
                          },
                      br()
                      )
                    }
                  ),
                actionButton(inputId = ns("submit"), label = "submit all")
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
                         connectors = dc$get_dataset_connectors()
              )

            } else if (is(dc, class2 = "RelationalDatasetConnector")) {
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
    FUN = function(xx) {
      any(
        vapply(
          class,
          function(class_name) {
            is(object = xx, class2 = class_name)
          },
          FUN.VALUE = logical(1)
        )
      )
    },
    FUN.VALUE = logical(1)
  )
}
