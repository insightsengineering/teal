## RelationalDataList ====
#' @title Manage multiple \code{RelationalDataConnector} and \code{RelationalDatasetConnector}
#' @description
#' Class manages  \code{RelationalDataConnector} and \code{RelationalDatasetConnector} objects
#' and aggregate them in one application. Class also decides whether to launch app
#' before initialize teal application.
#'
#' @examples
#' library(random.cdisc.data)
#' x <- rcd_cdisc_data( # RelationalDataConnector
#'   rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE),
#'   rcd_cdisc_dataset_connector("ADLB", radlb, cached = TRUE)
#' )
#' x2 <- rcd_cdisc_data( # RelationalDataConnector
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
#' tc$get_cdisc_data()
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
#' tc$get_cdisc_data()
#' \dontrun{
#' tc$launch()
#' get_raw_data(tc)
#' }
#' @importFrom R6 R6Class
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

      #Cannot allow RelationalDataList to be inside dot_args
      #previous check does not capture this as RelationalDataList inherits
      #from RelationalData
      if (any(is_any_class_list(dot_args, "RelationalDataList"))) {
        stop("Data elements cannot be RelationalDataList")
      }

      datanames <- unlist(lapply(dot_args, get_dataname))
      if (any(duplicated(datanames))) {
        stop("Found duplicated dataset names.")
      }

      private$datasets <- dot_args

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
    #' Derive the code for all datasets
    #' @param dataname (\code{character}) dataname or \code{NULL} for all datasets
    #' @param deparse (\code{logical}) whether to return the deparsed form of a call
    #' @return \code{vector} of \code{character} containing code
    get_code = function(dataname = NULL, deparse = TRUE) {
      stopifnot(is_logical_single(deparse))

      datasets_code <- private$get_code_datasets(dataname = dataname, deparse = deparse)
      mutate_code <- private$get_mutate_code(deparse = deparse)
      all_code <- c(datasets_code, mutate_code)

      if (isTRUE(deparse)) {
        all_code <- paste0(all_code, collapse = "\n")
      }

      return(all_code)
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
      names(res) <- vapply(res, get_dataname, character(1))
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
    #' Get \code{RelationalDataset} object.
    #' @param dataname (\code{character} value)\cr
    #'   name of dataset to be returned. If \code{NULL}, all datasets are returned.
    #'
    #' @return \code{RelationalDataset}.
    get_dataset = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))
      self$get_datasets()[[dataname]]
    },
    #' @description
    #' Get \code{list} of \code{RelationalDataset} objects.
    #' @return \code{list} of \code{RelationalDataset}.
    get_datasets = function() {
      datasets <- unlist(lapply(private$datasets, function(x) if (x$is_pulled()) get_datasets(x) else NULL))
      res <- Filter(Negate(is.null), datasets)
      names(res) <- vapply(res, get_dataname, character(1))
      return(res)
    },
    #' @description
    #' Get \code{cdisc_data} object from multiple \code{RelationalDataset} objects.
    #'
    #' @return \code{cdisc_data} object.
    get_cdisc_data = function() {
      if (is.null(private$cdisc_code)) {
        do.call("cdisc_data", self$get_datasets())
      } else {
        do.call("cdisc_data", c(self$get_datasets(), code = private$cdisc_code))
      }
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

      # otherwise load RelationDataConnector and
      # RelationalDatasetConnector with shiny app
      shinyApp(
        ui = fluidPage(
          fluidRow(
            column(
              width = 8,
              offset = 2,
              private$ui(id = "main_app"),
              useShinyjs(),
              br()
            )
          )
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          dat <- callModule(private$server, id = "main_app")
        }
      )
    }
  ),

  ## __Private Fields ====
  private = list(
    ui = NULL,
    server = NULL,

    ## __Private Methods ====
    get_code_datasets = function(dataname = NULL, deparse = TRUE) {
      if (is.null(private$datasets)) {
        if (isTRUE(deparse)) {
          character(0)
        } else {
          NULL
        }
      } else if (!is.null(dataname) && !(dataname %in% self$get_datanames())) {
        if (isTRUE(deparse)) {
          character(0)
        } else {
          NULL
        }
      } else if (is.null(private$code) && !is.null(dataname)) {
        for (i in private$datasets) {
          if (dataname %in% get_dataname(i)) {
            return(if_cond(get_code(i, dataname = dataname, deparse = deparse), character(0), is_empty_string))
          }
        }
      } else {
        if (isTRUE(deparse)) {
          Filter(
            Negate(is_empty_string),
            vapply(private$datasets, get_code, character(1), deparse = TRUE)
          )
        } else {
          unname(unlist(lapply(private$datasets, get_code, deparse = FALSE)))
        }
      }
    },
    set_ui = function() {
      private$ui <- function(id) {
        ns <- NS(id)

        # connectors ui(s) + submit button
        fluidPage(
          column(
            width = 8,
            offset = 2,
            tagList(
              lapply(
                self$get_data_connectors(),
                function(x) {
                  div(
                    x$get_ui(
                      id = ns(paste0(x$get_datanames(), collapse = "_"))
                    ),
                    br()
                  )
                }
              ),
              lapply(
                self$get_dataset_connectors(),
                function(x) {
                  div(
                    x$get_ui(
                      id = ns(x$get_dataname())
                    ),
                    br()
                  )
                }
              ),
              actionButton(inputId = ns("submit"), label = "submit all")
            )
          )
        )
      }
    },
    set_server = function() {
      private$server <- function(input, output, session) {
        rv <- reactiveVal(NULL)

        observeEvent(input$submit, {

          # load data from all connectors
          lapply(
            self$get_connectors(),
            function(dc) {
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
            }
          )

          shinyjs::hide("submit")

          rv(self)
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
