#' @name CDISCFilteredData
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title Class to encapsulate relational filtered datasets with its parents.
#'
#' @details
#' The `CDISCFilteredData` class implements logic to filter a relational
#' dataset by inheriting from `FilteredData`.
#' A dataset can have up to one parent dataset. Rows are identified by the foreign
#' key and only those rows that appear in the parent dataset are kept in the filtered
#' dataset.
#'
#' The relational model is attached to the data. When `set_data` is called with the
#' data, its `keys` attribute has the following elements:
#'  - `primary`: columns based on which unique rows are counted
#'  - `parent`: parent dataset based on which the dataset is filtered (via `inner_join`)
#'  - `foreign`: keys to join on when filtering with parent dataset
#'
#' The teal UI works with objects of class `FilteredData` which may mix CDISC and other
#' data (e.g. `iris`).
#'
#' It is not too difficult to make this class generalize to more than one parent dataset,
#' provided each dataset then contains information about its parents together with the
#' shared keys with each of them.
#'
#' @seealso `FilteredData` class
#'
#' @examples
#' library(scda)
#' library(shiny)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#'
#' adsl <- cdisc_dataset("ADSL", ADSL)
#' adtte <- cdisc_dataset("ADTTE", ADTTE)
#' data <- cdisc_data(adsl, adtte)
#' datasets <- teal:::CDISCFilteredData$new()
#'
#' # to avoid using isolate(), you can provide a default isolate context by calling
#' # options(shiny.suppressMissingContextError = TRUE) #nolint
#' # don't forget to deactivate this option at the end
#' # options(shiny.suppressMissingContextError = FALSE) #nolint
#'
#' # setting the data
#' isolate({
#'   datasets$set_dataset(adsl,  data$get_join_keys()$get(dataset_1 = "ADSL"))
#'   datasets$set_dataset(adtte, data$get_join_keys()$get(dataset_1 = "ADTTE"))
#'   datasets$set_join_keys(join_keys())
#'  })
#'
#'
#' isolate({
#'   datasets$datanames()
#'   datasets$get_data_info("ADSL", filtered = FALSE)
#'
#'   # filters dataset to obtain information
#'   datasets$get_data_info("ADSL", filtered = TRUE)
#'
#'   print(datasets$get_call("ADSL"))
#'   print(datasets$get_call("ADTTE"))
#'
#'   df <- datasets$get_data("ADSL", filtered = FALSE)
#'   print(df)
#'  })
#'
#'
#' filter_state_adtte <- teal:::init_filter_state(
#'   ADTTE$PARAMCD,
#'   varname = "PARAMCD",
#'   input_dataname = as.name("ADTTE"),
#'   use_dataname = TRUE
#' )
#' filter_state_adtte$set_selected("OS")
#'
#' states <- datasets$get_filtered_datasets("ADTTE")$get_filter_states(1)
#' states$queue_push(filter_state_adtte, queue_index = 1L, element_id = "PARAMCD")
#'
#' isolate(datasets$get_call("ADTTE"))
#'
#'
#' filter_state_adsl <- teal:::init_filter_state(
#'   ADSL$SEX,
#'   varname = "SEX",
#'   input_dataname = as.name("SEX"),
#'   use_dataname = TRUE
#' )
#' filter_state_adsl$set_selected("F")
#'
#' states <- datasets$get_filtered_datasets("ADSL")$get_filter_states("filter")
#' states$queue_push(filter_state_adsl, queue_index = 1L, element_id = "SEX")
#'
#' isolate(datasets$get_call("ADSL"))
#' isolate(datasets$get_call("ADTTE"))
CDISCFilteredData <- R6::R6Class( # nolint
  "CDISCFilteredData",
  inherit = FilteredData,
  ## CDISCFilteredData ====
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initialize a `CDISCFilteredData` object
    initialize = function() {
      super$initialize()
      return(invisible(self))
    },

    #' @description
    #' Get datanames
    #'
    #' The datanames are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of datanames
    datanames = function() {
      datanames <- super$datanames()
      # get_keys checks dataname is in datanames, not by calling `self$datanames()`,
      # but `names(private$unfiltered_datasets)` to avoid an infinite recursion
      child_parent <- sapply(datanames, function(i) self$get_parentname(i), USE.NAMES = TRUE, simplify = FALSE)
      ordered_datanames <- topological_sort(child_parent)
      return(as.character(intersect(as.character(ordered_datanames), datanames)))
    },

    #' @description
    #'
    #' Returns the filter `call` to filter a single dataset including the `inner_join`
    #' with its parent dataset. It assumes that the filtered datasets it depends
    #' on are available.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`call` or `list` of calls ) to filter dataset
    #'
    get_call = function(dataname) {
      filtered_dataname <- private$filtered_dataname(dataname)
      parent_dataname <- self$get_parentname(dataname)

      if (is_empty(parent_dataname)) {
        super$get_call(dataname)
      } else {
        self$get_filtered_datasets(dataname)$get_call()
      }
    },

    #' @description
    #' Get info about dataname, i.e. number of rows, subjects.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @param filtered (`logical`) whether to obtain this info for the
    #'   filtered dataset
    #' @return a named vector
    get_data_info = function(dataname, filtered) {
      private$check_data_varname_exists(dataname)
      stopifnot(is_logical_single(filtered))

      nrows <- self$get_filtered_datasets(dataname)$get_data_info(filtered = filtered)
      nsubjects <- self$get_filtered_datasets(dataname)$get_subjects_info(filtered = filtered)

      list(
        Obs = nrows,
        Subjects = nsubjects
      )
    },


    #' @description
    #' Get names of datasets available for filtering
    #'
    #' @param dataname (`character` vector) names of the dataset
    #' @return (`character` vector) of dataset names
    get_filterable_datanames = function(dataname) {

      parents <- character(0)
      for (i in dataname) {
        while (!is_empty(i)) {
          parent_i <- self$get_parentname(i)
          parents <- c(parents, parent_i)
          i <- parent_i
        }
      }

      return(unique(c(parents, dataname)))
    },

    #' @description
    #' Get parent dataset name
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) name of parent dataset
    get_parentname = function(dataname) {
      self$get_data_attr(dataname, "parent")
    },

    #' @description
    #' Add dataset
    #'
    #' Add dataset and preserve all attributes attached to this object.
    #' Technically `set_dataset` created `FilteredDataset` which keeps
    #' `dataset` for filtering purpose.
    #'
    #' @param dataset (`Dataset`)\cr
    #'   object containing data and attributes.
    #' @param join_key_set (`JoinKeySet`)\cr
    #'   keys to merge this `dataset` to the other datasets
    #' @return (`self`) object of this class
    set_dataset = function(dataset, join_key_set = NULL) {
      super$set_dataset(dataset, join_key_set)

      dataname <- get_dataname(dataset)
      parent_dataname <- self$get_parentname(dataname)

      if (!is_empty(parent_dataname)) {
        parent_dataset <- self$get_filtered_datasets(parent_dataname)
        fdataset <- self$get_filtered_datasets(dataname)
        fdataset$add_to_eval_env(
          parent_dataset$get_filtered_dataname(),
          list(parent_dataset$get_data_reactive())
        )
      }
    }

  ),

  ## __Private Methods---------------------
  private = list(
    # datanames in the order in which they must be evaluated (in case of dependencies)
    # this is a reactive and kept as a field for caching
    ordered_datanames = NULL,

    join_keys = NULL,

    validate = function() {
      stopifnot(
        setequal(private$ordered_datanames, names(private$dataset_filters)),
      )
      stopifnot(is.null(join_keys))
      super$validate()
    }
  )
)

#' Topological graph sort
#'
#' Graph is a list which for each node contains a vector of child nodes
#' in the returned list, parents appear before their children.
#'
#' Implementation of Kahn algorithm with a modification to maintain the order of input elements.
#'
#' @param graph (named `list`) list with node vector elements
#'
#' @examples
#' teal:::topological_sort(list(A = c(), B = c("A"), C = c("B"), D = c("A")))
#' teal:::topological_sort(list(D = c("A"), A = c(), B = c("A"), C = c("B")))
#' teal:::topological_sort(list(D = c("A"), B = c("A"), C = c("B"), A = c()))
topological_sort <- function(graph) {
  # compute in-degrees
  in_degrees <- list()
  for (node in names(graph)) {
    in_degrees[[node]] <- 0
    for (to_edge in graph[[node]]) {
      in_degrees[[to_edge]] <- 0
    }
  }

  for (node in graph) {
    for (to_edge in node) {
      in_degrees[[to_edge]] <- in_degrees[[to_edge]] + 1
    }
  }

  # sort
  visited <- 0
  sorted <- list()
  zero_in <- list()
  for (node in names(in_degrees)) {
    if (in_degrees[[node]] == 0) zero_in <- append(zero_in, node)
  }
  zero_in <- rev(zero_in)

  while (length(zero_in) != 0) {
    visited <- visited + 1
    sorted <- c(zero_in[[1]], sorted)
    for (edge_to in graph[[zero_in[[1]]]]) {
      in_degrees[[edge_to]] <- in_degrees[[edge_to]] - 1
      if (in_degrees[[edge_to]] == 0) {
        zero_in <- append(zero_in, edge_to, 1)
      }
    }
    zero_in[[1]] <- NULL
  }

  if (visited != length(in_degrees)) {
    stop("Graph is not a directed acyclic graph. Cycles involving nodes: ",
      paste0(setdiff(names(in_degrees), sorted), collapse = " "))
  } else {
    return(sorted)
  }
}

is_dag <- function(graph) {
  is(try(topological_sort(graph), silent = TRUE), "try-error")
}
