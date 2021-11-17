## RelationalData ====
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title Manage multiple `RelationalDataConnector`, `DatasetConnector` and `Dataset` objects.
#'
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
#' @param check (\code{logical}) reproducibility check - whether evaluated preprocessing code gives the same objects
#'   as provided in arguments. Check is run only if flag is true and preprocessing code is not empty.
#'
#' @examples
#' library(scda)
#' adsl_cf <- callable_function(function() synthetic_cdisc_data("latest")$adsl)
#' adlb_cf <- callable_function(function() synthetic_cdisc_data("latest")$adlb)
#' adrs_cf <- callable_function(function() synthetic_cdisc_data("latest")$adrs)
#' adtte_cf <- callable_function(function() synthetic_cdisc_data("latest")$adtte)
#' x1 <- cdisc_dataset_connector("ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
#' x2 <- cdisc_dataset_connector("ADRS", adrs_cf, keys = get_cdisc_keys("ADRS"))
#' x3 <- cdisc_dataset(
#'   dataname = "ADAE",
#'   x = synthetic_cdisc_data("latest")$adae,
#'   code = "library(scda)\nADAE <- synthetic_cdisc_data(\"latest\")$adae"
#' )
#' x4 <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"))
#' tc <- teal:::RelationalData$new(x1, x2, x3, x4)
#' tc$get_datanames()
#' \dontrun{
#' tc$launch()
#' get_datasets(tc) # equivalent to tc$get_datasets()
#' tc$get_dataset("ADAE")
#' tc$check()
#' }
#'
#' x <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = synthetic_cdisc_data("latest")$adsl,
#'   code = "library(scda)\nADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#' )
#'
#' x2 <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"))
#' tc <- teal:::RelationalData$new(x, x2)
#' \dontrun{
#' # This errors as we have not pulled the data
#' # tc$get_datasets()
#' # pull the data and then we can get the datasets
#' tc$launch()
#' tc$get_datasets()
#' get_raw_data(tc)
#' }
#'
RelationalData <- R6::R6Class( # nolint
  classname = "RelationalData",
  inherit = DataAbstract,
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `RelationalData` class
    initialize = function(..., check = FALSE, join_keys) {
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

      self$set_check(check)

      private$pull_code <- CodeClass$new()
      private$mutate_code <- CodeClass$new()

      for (dataset_1 in names(join_keys$get())) {
        for (dataset_2 in names(join_keys$get()[[dataset_1]])) {
          self$mutate_join_keys(dataset_1, dataset_2, join_keys$get()[[dataset_1]][[dataset_2]])
        }
      }
      for (dat_name in names(self$get_items())) {
        if (is_empty(join_keys$get(dat_name, dat_name))) {
          self$mutate_join_keys(dat_name, dat_name, get_keys(self$get_items(dat_name)))
        }
      }


      self$id <- sample.int(1e11, 1, useHash = TRUE)

      logger::log_trace(
        "RelationalData$initialize initialized data: { paste(self$get_datanames(), collapse = ' ') }"
      )
      return(invisible(self))
    },
    #' @description
    #' Creates a copy of the object with keeping valid references
    #' between `Dataset` and `DatasetConnector` objects
    #' @param deep (`logical(1)`)\cr
    #'  argument passed to `clone` method. If `TRUE` deep copy is made
    #' @return self invisible
    copy = function(deep = FALSE) {
      new_self <- self$clone(deep = deep)
      new_self$reassign_datasets_vars()
      logger::log_trace("RelationalData$copy copied self.")
      invisible(new_self)
    },
    #' Prints this RelationalData.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      check_ellipsis(...)

      cat(sprintf(
        "A %s object containing %d Dataset/DatasetConnector object(s) as element(s):\n",
        class(self)[1],
        length(private$datasets)
      ))

      for (i in seq_along(private$datasets)) {
        cat(sprintf("--> Element %d:\n", i))
        print(private$datasets[[i]])
      }

      invisible(self)
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
      res <- join_keys()

      for (dat_obj in self$get_items()) {
        list_keys <- dat_obj$get_join_keys()$get()[[1]]
        for (dat_name in names(list_keys)) {
          res$mutate(dat_obj$get_dataname(), dat_name, list_keys[[dat_name]])
        }
      }
      return(res)
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
        return(
          function(id) {
            moduleServer(
              id = id,
              module = function(input, output, session) {
                reactive(self)
              }
            )
          }
        )
      } else {
        function(id) {
          moduleServer(
            id = id,
            module = private$server
          )
        }
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
              self$get_ui(id = "main_app"),
              shinyjs::hidden(
                tags$div(
                  id = "data_loaded",
                  div(
                    h3("Data successfully loaded."),
                    p("You can close this window and get back to R console.")
                  )
                )
              ),
              shinyjs::useShinyjs(),
              include_teal_css_js(),
              br()
            )
          )
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          dat <- self$get_server()(id = "main_app")

          observeEvent(dat(), {

            if (self$is_pulled()) {
              shinyjs::show("data_loaded")
              stopApp()
            }
          })
          NULL
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
      stopifnot(is_character_single(dataset_1))
      stopifnot(is_character_single(dataset_2))

      if (!dataset_1 %in% names(self$get_items())) {
        stop(sprintf("%s is not a name to any dataset stored in object.", dataset_1))
      }
      if (!dataset_2 %in% names(self$get_items())) {
        stop(sprintf("%s is not a name to any dataset stored in object.", dataset_2))
      }

      data_obj_1 <- self$get_items()[[dataset_1]]
      data_obj_2 <- self$get_items()[[dataset_2]]

      data_obj_1$mutate_join_keys(dataset_2, val)
      data_obj_2$mutate_join_keys(dataset_1, val)

      logger::log_trace("RelationalData$mutate_join_keys modified the join keys.")
      return(invisible(self))
    },

    # ___ check ====
    #' @description
    #' Check there is consistency between the datasets and join_keys
    #' @return raise and error or invisible `TRUE`
    check_metadata = function() {
      if (isFALSE(self$is_pulled())) {
        # all the checks below required data to be already pulled
        return(invisible(TRUE))
      }
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
        dataset$check_keys()
      }
      logger::log_trace("CDISCData$check_metadata metadata check passed")

      return(invisible(TRUE))
    }
  ),

  ## __Private Fields ====
  private = list(
    ui = function(id) {
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
                      if (is(x, class2 = "RelationalDataConnector")) {
                        if_null(
                          x$get_ui(id = ns(x$id)),
                          div(
                            h4("Dataset Connector for: ", lapply(x$get_datanames(), code)),
                            p(icon("check"), "Ready to Load")
                          )
                        )

                      } else if (is(x, class2 = "DatasetConnector")) {
                        if_null(
                          x$get_ui(id = ns(paste0(x$get_datanames(), collapse = "_"))),
                          div(
                            h4("Dataset Connector for: ", code(x$get_dataname())),
                            p(icon("check"), "Ready to Load")
                          )
                        )
                      } else {
                        div(h4("Data(set) for: ", lapply(x$get_datanames(), code)), p(icon("check"), "Loaded"))
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
    },
    server = function(input, output, session) {
      logger::log_trace(
        "RelationalData$server initializing..."
      )

      shinyjs::show("delayed_data")
      for (dc in self$get_connectors()) {
        if (is(dc, class2 = "RelationalDataConnector")) {
          dc$get_preopen_server()(id = dc$id)
        }
      }
      rv <- reactiveVal(NULL)
      observeEvent(input$submit, {
        logger::log_trace("RelationalData$server@1 submit button clicked.")
        # load data from all connectors
        for (dc in self$get_connectors()) {
          if (is(dc, class2 = "RelationalDataConnector")) {
            dc$get_server()(
              id = dc$id,
              connection = dc$get_connection(),
              connectors = dc$get_items()
            )

          } else if (is(dc, class2 = "DatasetConnector")) {
            dc$get_server()(id = dc$get_dataname())
          }
          if (dc$is_failed()) {
            break
          }
        }

        if (self$is_pulled()) {
          logger::log_trace("RelationalData$server@1 data is pulled.")
          withProgress(value = 1, message = "Checking data reproducibility", {
            # We check first and then mutate.
            #  mutate_code is reproducible by default we assume that we don't
            #  have to check the result of the re-evaluation of the code
            self$check_reproducibility()
          })

          withProgress(value = 1, message = "Executing processing code", {
            self$execute_mutate()
            self$check_metadata()
          })
          logger::log_info("Data ready to pass to the application.")
          shinyjs::hide("delayed_data")
          rv(self)
        }
      })
      return(rv)
    },
    join_keys = NULL # JoinKeys after initialization
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
