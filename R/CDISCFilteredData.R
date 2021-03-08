#' @name CDISCFilteredData
#' @docType class
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
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
#' datasets <- teal:::CDISCFilteredData$new()
#'
#' # to avoid using isolate(), you can provide a default isolate context by calling
#' # options(shiny.suppressMissingContextError = TRUE) #nolint
#' # don't forget to deactivate this option at the end
#' # options(shiny.suppressMissingContextError = FALSE) #nolint
#'
#' isolate({
#'   datasets$datanames()
#'   datasets
#'
#'   datasets$set_data("ADSL", ADSL)
#'   datasets$set_join_keys(join_keys())
#'
#'   datasets$datanames()
#'   datasets
#'
#'   datasets$datanames()
#'   datasets$get_data_info("ADSL", filtered = FALSE)
#'   # filters dataset to obtain information
#'   datasets$get_data_info("ADSL", filtered = TRUE)
#'   datasets$get_filter_info("ADSL")
#'   df <- datasets$get_data("ADSL", filtered = FALSE)
#'   # df
#'
#'   datasets$get_filter_type("ADSL", "SEX")
#'   datasets$set_filter_state("ADSL", state = list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE),
#'     SEX = list(choices = c("M", "F"), keep_na = FALSE)
#'   ))
#'   datasets
#'   datasets$get_filter_type("ADSL", "SEX")
#'   datasets$get_filter_info("ADSL")[["SEX"]]$type
#'
#'   # will fail because of invalid range
#'   # datasets$set_filter_state("ADSL", list(
#'   #   AGE = list(range = c(3, 7), keep_na = FALSE)
#'   # ))
#'   datasets$set_filter_state("ADSL", list(
#'     AGE = list(range = c(33, 44), keep_na = FALSE)
#'   ))
#'   # wrapper functions
#'   teal:::set_single_filter_state(datasets, "ADSL", "AGE", c(35, 42))
#'   teal:::set_single_filter_state(
#'     datasets, "ADSL", "AGE",
#'     state = list(range = c(33, 44), keep_na = FALSE)
#'   )
#'   datasets$set_filter_state(
#'     "ADSL",
#'      state = list(
#'        SEX = list(choices = c("M", "F"), keep_na = FALSE)
#'     )
#'   )
#'   datasets$get_filter_type("ADSL", "SEX")
#'
#'   datasets$get_filter_state("ADSL")
#'
#'   datasets$print_filter_info("ADSL")
#'
#'   # add ADAE to show dependency
#'   ADAE <- radae(cached = TRUE)
#'   ADAE_indep <- ADAE
#'   attr(ADAE, "keys") <- get_cdisc_keys("ADAE")
#'   attr(ADAE_indep, "keys") <- get_cdisc_keys("ADAE")
#'   attr(ADAE, "parent") <- "ADSL"
#'
#'   datasets$set_data("ADAE", ADAE)
#'   datasets$set_data("ADAEIndep", ADAE_indep)
#'
#'   datasets$set_join_keys(join_keys(join_key("ADSL", "ADAE", c("STUDYID", "USUBJID"))))
#'
#'   datasets$get_data_info("ADSL", filtered = FALSE)
#'   datasets$get_data_info("ADSL", filtered = TRUE)
#'   datasets$get_data_info("ADAE", filtered = FALSE)
#'   datasets$get_data_info("ADAE", filtered = TRUE)
#'   datasets$get_data_info("ADAEIndep", filtered = FALSE)
#'   datasets$get_data_info("ADAEIndep", filtered = TRUE)
#'
#'   # see generated code
#'   datasets$get_filter_expr("ADSL")
#'   datasets$get_filter_expr("ADAEIndep")
#' })
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
      # Also initializes the datanames reactive
      private$ordered_datanames <- private$reactive_ordered_datanames()
      return(invisible(self))
    },

    #' @description
    #' Get datanames
    #'
    #' The datanames are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of datanames
    datanames = function() {
      return(as.character(private$ordered_datanames()))
    },

    #' @description
    #' Get dataset names of a given dataname for the filtering.
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
    #' Get variable names of a given dataname for the filtering.
    #'
    #' This method incorporates parent-child relations, i.e.
    #' parent variable names are removed from child variable names.
    #' @param dataname (`character`) name of the dataset
    #' @return (`character` vector) of variable names
    get_filterable_varnames = function(dataname) {
      varnames <- self$get_varnames(dataname)
      parent_dataname <- self$get_parentname(dataname)
      parent_varnames <- if_not_empty(parent_dataname, self$get_varnames(parent_dataname))
      setdiff(varnames, parent_varnames)
    },

    #' @description
    #'
    #' Returns the filter expression to filter a single dataset including the `inner_join`
    #' with its parent dataset. It assumes that the filtered datasets it depends
    #' on are available.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`expression`) to filter dataset
    #'
    get_filter_expr = function(dataname) {
      private$check_data_varname_exists(dataname)

      filtered_dataname <- private$filtered_dataname(dataname)
      parent_dataname <- self$get_parentname(dataname)

      if (is_empty(parent_dataname)) {
        # use bquote to return same types whether or not parent is present
        bquote({
          .(as.call(list(
            as.name("<-"),
            as.name(filtered_dataname),
            private$get_pure_filter_call(dataname)
          )))
        })
      } else {
        filtered_dataname_alone <- paste0(filtered_dataname, "_ALONE")
        filter_call <- as.call(list(
          as.name("<-"),
          as.name(filtered_dataname_alone),
          private$get_pure_filter_call(dataname)
        ))

        # join with parent dataset
        keys <- self$get_join_keys(parent_dataname, dataname)
        parent_keys <- names(keys)
        dataset_keys <- unname(keys)
        check_in_subset(parent_keys, self$get_varnames(parent_dataname), pre_msg = "Join keys not in columns: ")
        check_in_subset(dataset_keys, self$get_varnames(dataname), pre_msg = "Join keys not in columns: ")

        merge_call <- call(
          "<-", as.name(filtered_dataname),
          call_with_colon(
            "dplyr::inner_join",
            x = as.name(filtered_dataname_alone),
            y = if (is_empty(parent_keys)) {
              as.name(private$filtered_dataname(parent_dataname))
            } else {
              call(
                "[",
                as.name(private$filtered_dataname(parent_dataname)),
                quote(expr = ), # nolint
                parent_keys,
                drop = FALSE
              )
            },
            unlist_args = if (is_empty(parent_keys) || is_empty(dataset_keys)) {
              list()
            } else if (identical(parent_keys, dataset_keys)) {
              list(by = parent_keys)
            } else {
              list(by = setNames(parent_keys, nm = dataset_keys))
            }
          )
        )

        return(bquote({
          .(filter_call)
          .(merge_call)
        }))
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

      data <- self$get_data(dataname, filtered = filtered)

      nrows <- nrow(data)

      # here goes strong assumption that parent keys distinguish subject id
      parent_name <- self$get_parentname(dataname)
      subject_keys <- if (is_empty(parent_name)) {
        self$get_primary_keys(dataname)
      } else {
        self$get_join_keys(parent_name, dataname)
      }
      no_subjects <- if (is_empty(subject_keys)) {
        dplyr::n_distinct(data)
      } else {
        dplyr::n_distinct(data[subject_keys])
      }

      c(
        Obs = nrows,
        Subjects = no_subjects
      )
    },

    #' @description
    #' Get parent dataset name
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) name of parent dataset
    get_parentname = function(dataname) {
      self$get_data_attr(dataname, "parent")
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
        setequal(private$ordered_datanames, names(private$unfiltered_datasets)),
      )
      stopifnot(is.null(join_keys))
      super$validate()
    },

    # Method to compute the order in which datasets should be evaluated
    # It needs to return a reactive because it's inputs are reactive
    # (in particular: private$unfiltered_datasets is a reactiveValues)
    reactive_ordered_datanames = function() {
      reactive({
        datanames <- names(private$unfiltered_datasets)
        # get_keys checks dataname is in datanames, not by calling `self$datanames()`,
        # but `names(private$unfiltered_datasets)` to avoid an infinite recursion
        child_parent <- sapply(datanames, function(i) self$get_parentname(i), USE.NAMES = TRUE, simplify = FALSE)
        ordered_datanames <- topological_sort(child_parent)
        .log(paste0("Ordered datanames computed: ", toString(ordered_datanames)))
        ordered_datanames
      })
    },

    # Reactive to compute the filtered dataset
    # @details
    # This returns a reactive that computes the filtered dataset.
    # Through reactivity, it is only lazily evaluated and also re-evaluates
    # whenever the unfiltered datasets it depends on change.
    #
    # @param dataname `character` name of the dataset
    # @return `reactive` expression for the provided `dataname` that returns
    #   the filtered dataset when evaluated
    reactive_filtered_dataset = function(dataname) {
      private$check_data_varname_exists(dataname)

      reactive({
        .log("################################################# Refiltering dataset ", dataname)

        # We could use a chunks object here, but don't do so for simplicity as we don't need its capabilities.
        # We take the parent of the global env, i.e. the last attached package, to avoid picking up
        # any variables defined in the global environment. This is okay as it takes whatever is the current
        # `globalenv` when this reactive is evaluated, i.e. when the relevant packages are already loaded.
        env <- new.env(parent = parent.env(globalenv()))

        # the filtered parent dataset must be available as well as the unfiltered dataset
        env[[dataname]] <- self$get_data(dataname, filtered = FALSE)
        parent_dataname <- self$get_data_attr(dataname, "parent")
        if (!is_empty(parent_dataname)) {
          env[[private$filtered_dataname(parent_dataname)]] <- self$get_data(parent_dataname, filtered = TRUE)
        }

        eval(self$get_filter_expr(dataname), envir = env)
        return(env[[private$filtered_dataname(dataname)]])
      })
    }
  )
)

#' Topological graph sort
#'
#' graph is a list which for each node contains a vector of child nodes
#' in the returned list, parents appear before their children.
#'
#' @param graph (named `list`) list with node vector elements
#'
#' @examples
#' correct_orders <- list(
#'   list("A", "B", "D", "C"),
#'   list("A", "B", "C", "D"),
#'   list("A", "B", "C", "D")
#' )
#' stopifnot(identical(
#'   teal:::topological_sort(list(A = c(), B = c("A"), C = c("B"), D = c("A"))),
#'   correct_orders[[1]]
#' ))
#' stopifnot(identical(
#'   teal:::topological_sort(list(D = c("A"), A = c(), B = c("A"), C = c("B"))),
#'   correct_orders[[2]]
#' ))
#' stopifnot(identical(
#'   teal:::topological_sort(list(D = c("A"), B = c("A"), C = c("B"), A = c())),
#'   correct_orders[[3]]
#' ))
topological_sort <- function(graph) {
  # implementation of Kahn's topological sort
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

  while (length(zero_in) != 0) {
    visited <- visited + 1
    sorted <- c(zero_in[[1]], sorted)
    for (edge_to in graph[[zero_in[[1]]]]) {
      in_degrees[[edge_to]] <- in_degrees[[edge_to]] - 1
      if (in_degrees[[edge_to]] == 0) {
        zero_in <- append(zero_in, edge_to)
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
