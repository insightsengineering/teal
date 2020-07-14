# RelationalDataList ------
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
#'
#' tc <- teal:::RelationalDataList$new(x, x2)
#' \dontrun{
#' tc$launch()
#' tc$get_cdisc_data()
#' }
#' @importFrom R6 R6Class
RelationalDataList <- R6::R6Class( #nolint
  classname = "RelationalDataList",
  inherit = RelationalData,
  # ..public ------
  public = list(
    #' @param ... (\code{RelationalDataConnector}, \code{RelationalDataset},
    #'  \code{RelationalDatasetConnector}) object
    #'
    initialize = function(...) {
      dot_args <- list(...)
      delayed_classes <- c("RelationalDataConnector", "RelationalDataset", "RelationalDatasetConnector")

      is_teal_data <- is_any_class_list(dot_args, delayed_classes)
      if (!all(is_teal_data)) {
        stop("All data elements should be RelationalData(set) or RelationalData(set)Connection")
      }

      datanames <- unlist(lapply(dot_args, get_dataname))
      if (any(duplicated(datanames))) {
        stop("Please do not include duplicated names.")
      }

      dot_args <- lapply(c(TRUE, FALSE), function(condition) {
        Filter(
          function(x) {
            is(object = x, class2 = "RelationalDataset") == condition
          },
          dot_args
        )
      })

      private$datasets <- dot_args[[1]]
      names(private$datasets) <- vapply(private$datasets, get_dataname, character(1))

      private$connectors <- dot_args[[2]]




      if (length(self$get_data_connectors()) > 0) {
        private$set_ui()
        private$set_server()
      }

      return(invisible(self))
    },
    #' @description
    #'   Check if the all datasets and connectors raw data is reproducible from the \code{get_code()} code.
    #' @return
    #'   \code{TRUE} if all the items generated from evaluating the
    #'   \code{get_code()} code are identical to the raw data, else \code{FALSE}.
    check = function() {
      if (!self$is_pulled()) {
        stop("Cannot check the raw data until it is pulled.")
      }

      if (is.null(private$code)) {
        all(vapply(private$datasets, function(x) x$check(), logical(1))) &&
          all(vapply(private$connectors, function(x) x$check(), logical(1)))
      } else {
        all(vapply(private$datasets, function(x) super$check_dataset_all_code(x), logical(1))) &&
          all(vapply(private$connectors, function(x) super$check_dataset_all_code(x), logical(1)))
      }
    },
    #' @description
    #'
    #' Derive the names of all datasets
    #' @return \code{character} vector with names
    get_datanames = function() {
      c(
        vapply(private$datasets, get_dataname, character(1)),
        unlist(lapply(private$connectors, get_dataname))
      )
    },
    #' @description
    #' Derive the code for all datasets
    #' @param dataname (\code{character}) dataname or \code{NULL} for all datasets
    #' @param deparse (\code{logical}) whether to return the deparsed form of a call
    #' @return \code{vector} of \code{character} containing code
    get_code = function(dataname = NULL, deparse = TRUE) {
      stopifnot(is_logical_single(deparse))

      datasets_code <- private$get_code_datasets(dataname = dataname, deparse = deparse)
      connectors_code <- private$get_code_connectors(dataname = dataname, deparse = deparse)
      mutate_code <- private$get_mutate_code(deparse = deparse)

      all_code <- c(datasets_code, connectors_code, mutate_code)

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
      return(Filter(
        function(x) {
          is(object = x, class2 = "RelationalDatasetConnector")
        },
        private$connectors))
    },
    #' Get data connectors.
    #'
    #' @return \code{list} with all \code{RelationalDataConnector} objects.
    get_data_connectors = function() {
      return(Filter(
        function(x) {
          is(object = x, class2 = "RelationalDataConnector")
        },
        private$connectors))
    },
    #' @description
    #' Get all datasets and all dataset connectors
    #'
    #' @param dataname (\code{character} value)\cr
    #'   name of dataset connector to be returned. If \code{NULL}, all connectors are returned.
    #'
    #' @return \code{list} with all datasets and all connectors
    get_all_datasets = function(dataname = NULL) {
      stopifnot(is.null(dataname) || is_character_single(dataname))

      all_datasets <- c(
        private$datasets,
        unlist(lapply(
          private$connectors,
          function(x) {
            if (is(x, "RelationalDatasetConnector")) {
              x
            } else if (is(x, "RelationalDataConnector")) {
              x$get_all_datasets()
            }
          }
        ))
      )

      all_datanames <- vapply(all_datasets, get_dataname, character(1))
      names(all_datasets) <- all_datanames

      if (is.null(dataname)) {
        all_datasets
      } else {
        all_datasets[[dataname]]
      }
    },

    #' @description
    #'
    #' Get a shiny-module server to render the necessary app to
    #' derive \code{RelationalDataConnector} object's data
    #'
    get_server = function() {
      if (is.null(private$server)) {
        return(function(input, output, session) {
          private$append_dataset_connectors()
          reactive(self$get_cdisc_data())
        })
      } else {
        private$server
      }
    },
    #' @description
    #' Check if all datasets and connectors have already been pulled.
    #'
    #' @return \code{TRUE} if all items have been already pulled, else \code{FALSE}
    is_pulled = function() {
      all(vapply(private$datasets, is_pulled, logical(1))) &&
        all(vapply(private$connectors, is_pulled, logical(1)))
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
      if (length(self$get_data_connectors()) == 0) {
        private$append_dataset_connectors()
      } else {
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
    }
  ),
  # ..private ------
  private = list(
    # .... fields: ------
    connectors = NULL,
    ui = NULL,
    # .... methods: ------
    server = NULL,
    get_code_connectors = function(dataname = NULL, deparse = TRUE) {
      if (is.null(private$connectors)) {
        NULL
      } else if (!is.null(dataname) && is.null(private$code)) {
        get_code(self$get_all_datasets(dataname))
      } else {
        if (isTRUE(deparse)) {
          vapply(private$connectors, get_code, character(1), deparse = TRUE)
        } else {
          unname(unlist(lapply(private$connectors, get_code, deparse = FALSE)))
        }
      }
    },
    append_datasets = function(datasets, remove_connectors = TRUE) {
      stopifnot(is_logical_single(remove_connectors))
      stopifnot(is(datasets, "RelationalDataset") ||
                  (is.list(datasets) && all(vapply(datasets, is, logical(1), "RelationalDataset"))))

      # Make sure there is not duplicated dataname added
      # by appending a new dataset
      if (is.list(datasets)) {
        stopifnot(length(intersect(
          vapply(datasets, get_dataname, character(1)),
          vapply(self$get_datasets(), get_dataname, character(1))
        )) == 0)
      } else {
        stopifnot(!datasets$get_dataname() %in% self$get_datanames())
      }
      # save datasets in object
      private$datasets <- c(private$datasets, datasets)

      if (remove_connectors) {
        # To not duplicate data, remove the connectors
        # at the origin
        private$connectors <- NULL
      }

      invisible(NULL)
    },
    append_dataset_connectors = function() {
      # append datasets only if all connectors are DatasetConnectors
      stopifnot(length(self$get_data_connectors()) == 0)
      if (length(private$connectors) > 0) {
        # save datasets in object
        private$append_datasets(
          datasets = lapply(private$connectors, function(x) {
            if (!is.null(x)) {
              load_dataset(x)
              return(get_dataset(x))
            }
          })
        )
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
        res <- reactiveVal(NULL)
        observeEvent(input$submit, {
          # load data from all connectors
          lapply(
            private$connectors,
            function(dc) {
              if (is(dc, class2 = "RelationalDataConnector")) {
                callModule(dc$get_server(),
                           id = paste0(dc$get_datanames(), collapse = "_"),
                           connection = dc$get_connection(),
                           connectors = dc$get_dataset_connectors())

              } else if (is(dc, class2 = "RelationalDatasetConnector")) {
                callModule(dc$get_server(),
                           id = dc$get_dataname())
              }
            }
          )

          # pull out datasets from connectors
          datasets <- unlist(lapply(private$connectors, function(dc) {
            if (is(dc, "RelationalDataConnector")) {
              get_datasets(dc)
            } else {
              get_dataset(dc)
            }
          })
          )
          names(datasets) <- vapply(datasets, get_dataname, character(1))

          # move datasets from connectors to private$datasets
          private$append_datasets(datasets = datasets)
          shinyjs::hide("submit")

          res(self$get_cdisc_data())
        })
        return(res)
      }
    }
  )
)


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
