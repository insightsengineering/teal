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
      connectors <- list(...)
      delayed_classes <- c("RelationalDataConnector", "RelationalDataset", "RelationalDatasetConnector")

      is_teal_data <- is_any_class_list(connectors, delayed_classes)
      if (!all(is_teal_data)) {
        stop("All data elements should be RelationalData(set) or RelationalData(set)Connection")
      }

      # sort elements by class name
      connectors <- sapply(
        delayed_classes,
        function(class_name) {
          Filter(
            function(x) {
              is(object = x, class2 = class_name)
            },
            connectors
          )
        },
        USE.NAMES = TRUE,
        simplify = FALSE
      )
      private$data_connectors <- connectors$RelationalDataConnector

      private$datasets <- connectors$RelationalDataset
      names(private$datasets) <- vapply(connectors$RelationalDataset,
                                        get_dataname,
                                        character(1))

      private$dataset_connectors <- connectors$RelationalDatasetConnector
      names(private$dataset_connectors) <- vapply(connectors$RelationalDatasetConnector,
                                                  get_dataname,
                                                  character(1))

      names_all <- c(names(private$datasets),
                     names(private$dataset_connectors),
                     names(private$data_connectors))

      if (any(duplicated(names_all))) {
        stop("Please do not include duplicated names.")
      }

      # Retrieve the IDs of the submit buttons
      # from all the connectors handed over
      # id of buttons is data_connector_<dataname>
      # (or <dataname_dataname> if multiple)
      if (length(private$data_connectors) > 0) {
        private$ui_submit_ids <- lapply(
          private$data_connectors,
          function(dc) {
            paste0(
              "data_connector_",
              paste0(
                vapply(dc$get_dataset_connectors(), get_dataname, character(1)),
                collapse = "_"
              ),
              "-",
              dc$get_submit_id()
            )
          }
        )
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
      c(
        vapply(private$datasets, get_dataname, character(1)),
        vapply(private$dataset_connectors, get_dataname, character(1)),
        `if`(
          length(private$data_connectors) > 0,
          unlist(lapply(private$data_connectors, function(x) x$get_datanames())),
          NULL
        )
      )
    },
    #' @description
    #'
    #' Derive the code for all datasets
    #' @return \code{list} of \code{character} containing code
    get_code = function() {
      c(
        vapply(private$datasets, get_code, character(1)),
        vapply(private$dataset_connectors, get_code, character(1)),
        `if`(
          length(private$data_connectors) > 0,
          unlist(lapply(private$data_connectors, function(x) x$get_code())),
          NULL
        )
      )
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
      return(private$dataset_connectors)
    },
    #' Get data connectors.
    #'
    #' @return \code{list} with all \code{RelationalDataConnector} objects.
    get_data_connectors = function() {
      return(private$data_connectors)
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
    #'
    #' Append datasets to this class
    #'
    #' @param datasets (\code{RelationalDataset}) use a already rendered dataset
    #'   and add it
    append = function(datasets) {
      if (is(datasets, "RelationalDataset") ||
          (is.list(datasets) && all(vapply(datasets, is, logical(1), "RelationalDataset")))
      ) {
        private$append_datasets(datasets)
      } else {
        stop("Only RelationalDataset objects can be appended to RelationalData.")
      }
    },
    #' @description
    #'
    #' Launch an app that allows to run the user-interfaces of all
    #' \code{DataConnector} and \code{DatasetConnector} modules
    #'
    #' This piece is mainly used for debugging.
    launch = function() {
      # load RelationDataConnector with shiny app
      if (length(private$data_connectors) > 0) {
        shinyApp(
          ui = fluidPage(private$ui(id = "main_app"),
                         br(),
                         useShinyjs(),
                         uiOutput("result")),
          server = function(input, output, session) {
            session$onSessionEnded(stopApp)

            dat <- callModule(private$server, id = "main_app")
              output$result <- renderUI({
                if (is(dat(), "cdisc_data")) {
                  private$data_connectors <- NULL
                  return(h3("Data successfully loaded!"))
                }
            })
          }
        )
      }
    }
  ),
  # ..private ------
  private = list(
    # .... fields: ------
    data_connectors = NULL,
    dataset_connectors = NULL,
    ui = NULL,
    ui_submit_ids = NULL,
    # .... methods: ------
    server = NULL,
    append_datasets = function(datasets, from = NULL) {
      stopifnot(is_character_single(from) || is.null(from))
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

      if (!is.null(from)) {
        # To not duplicate data, remove the connectors
        # at the origin
        private[[from]] <- NULL
      }

      invisible(NULL)
    },
    append_dataset_connectors = function() {
      # load RelationDatasetConnector objects
      if (length(private$dataset_connectors) > 0) {
        # save datasets in object
        private$append_datasets(
          datasets = lapply(private$dataset_connectors, function(x) {
            if (!is.null(x)) {
              load_dataset(x)
              return(get_dataset(x))
            }
          }),
          from = "dataset_connectors"
        )
      }
    },
    set_ui = function() {
      private$ui <- function(id) {
        ns <- NS(id)
        # hide "submit" buttons in favour of one "submit_all"
        tagList(
          tags$head(
            tags$style(
              paste0(paste0("#", ns(private$ui_submit_ids), " {display:none}"), collapse = "\n")
            )
          ),

          do.call(
            what = "tagList",
            args = lapply(
              private$data_connectors,
              function(x) {
                datanames <- vapply(x$get_dataset_connectors(), get_dataname, character(1))
                div(
                  h3(paste(c("Inputs for:", datanames), collapse = " ")),
                  x$get_ui(
                    ns(paste0("data_connector_", paste0(datanames, collapse = "_")))
                  )
                )

              }
            )
          ),

          actionButton(inputId = ns("submit"), label = "submit all")
        )
      }
    },
    set_server = function() {
      private$server <- function(input, output, session) {
        private$append_dataset_connectors()

        # each RelationalDataConnector modules is called here
        data_connector_modules <- lapply(
          private$data_connectors,
          function(dc) {
            id <- paste0(
              "data_connector_",
              paste0(
                vapply(dc$get_dataset_connectors(), get_dataname, character(1)),
                collapse = "_"
              )
            )
            callModule(dc$get_server(), id, return_cdisc_data = FALSE)
          }
        )

        # teal::init uses reactive(cdisc_data)
        res <- reactive({
          datasets <- unlist(lapply(data_connector_modules, function(x) x()))
          if (all(vapply(datasets, is, logical(1), "RelationalDataset")) &&
              length(datasets) > 0 &&
              !any(vapply(datasets, is.null, logical(1)))) {

            # save datasets in object
            private$append_datasets(
              datasets = datasets,
              from = "data_connectors"
            )

            self$get_cdisc_data()
          } else {
            NULL
          }
        })

        # click hidden 'submit' buttons when clicking 'submit_all'
        observeEvent(input$submit, {
          sapply(
            private$ui_submit_ids,
            function(id)  {
              shinyjs::click(id)
            }
          )
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
