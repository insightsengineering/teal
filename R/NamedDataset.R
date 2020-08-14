## NamedDataset ====
#' @title  R6 Class representing a dataset including code
#' @description
#' Any \code{data.frame} or \code{rtable} object can be
#' stored inside this object. Additionally there needs
#' to be the code that was used to generate this object.
#' additionally it is possible to label the data set
#' by handing over a \code{label} argument to the
#' \code{new} method
#'
#' @examples
#' named_data <- teal:::NamedDataset$new(
#'   x = data.frame(x = c(2, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   dataname = "XY",
#'   code = "XY <- data.frame(x = c(2, 2),
#'     y = c('a', 'b'), stringsAsFactors = FALSE)"
#' )
#' named_data$ncol
#' named_data$get_code()
#' named_data$get_dataname()
#' @importFrom R6 R6Class
#' @importFrom rlang with_options
NamedDataset <- R6::R6Class( # nolint
  "NamedDataset",
  inherit = RawDataset,

  ## __Public Methods ====
  public = list(
    #' @description
    #' initialize a \code{NamedDataset} class object
    #' @param x (\code{data.frame})
    #'
    #' @param dataname (\code{character}) A given name for the dataset
    #'   it may not contain spaces
    #'
    #' @param code (\code{character}) A character string defining the code
    #'   needed to produce the data set in \code{x}
    #'
    #' @param label (\code{character}) Label to describe the dataset
    #'
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{NamedDataset}, \code{NamedDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #'
    #' @import utils.nest
    #' @return new \code{NamedDataset} object
    initialize = function(x, dataname, code = character(0), label = character(0), vars = list()) {
      # Run RawDataset initialization
      super$initialize(x)

      self$set_dataname(dataname)
      private$code <- CodeClass$new()
      self$set_vars(vars)
      self$set_code(code)
      self$set_dataset_label(label)
      return(invisible(self))
    },
    #' @description
    #' Derive the \code{name} which was formerly called \code{dataname}
    #' @return \code{character} name of the dataset
    get_dataname = function() {
      private$.dataname
    },
    #' @description
    #' Set the name for the dataset
    #' @param dataname (\code{character}) the new name
    #' @return \code{self} invisibly for chaining
    set_dataname = function(dataname) {
      stopifnot(is_character_single(dataname))
      stopifnot(!grepl("\\s", dataname))
      private$.dataname <- dataname
      return(invisible(self))
    },
    #' @description
    #' Derive the dataname
    #' @return \code{character} name of the dataset
    get_datanames = function() {
      private$.dataname
    },
    #' @description
    #' Derive the \code{label} which was former called \code{datalabel}
    #' @return \code{character} label of the dataset
    get_dataset_label = function() {
      private$label
    },
    #' @description
    #' Set the label for the dataset
    #' @param label (\code{character}) the new label
    #' @return \code{self} invisibly for chaining
    set_dataset_label = function(label) {
      if (is.null(label)) {
        label <- character(0)
      }
      stopifnot(is_character_vector(label, min_length = 0, max_length = 1))
      private$label <- label
      return(invisible(self))
    },
    #' @description
    #' Get code to get data
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #'
    #' @return optionally deparsed \code{call} object
    get_code = function(deparse = TRUE) {
      stopifnot(is_logical_single(deparse))

      return(self$get_code_class()$get_code(deparse = deparse))
    },
    #' @description
    #' Get internal \code{CodeClass} object
    #'
    #' @return \code{CodeClass}
    #'
    get_code_class = function() {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$vars))
      res$append(private$code)

      return(res)
    },

    #' @description
    #' Mutate dataset by code
    #'
    #' Either code or script must be provided, but not both.
    #'
    #' @param code (\code{character}) Code to mutate the dataset. Must contain the
    #'  \code{dataset$dataname}
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    #'
    #' @return self invisibly for chaining
    mutate = function(code, vars = list()) {
      self$set_vars(vars)
      self$set_code(code)

      code_container <- CodeClass$new()
      code_container$set_code(code)

      execution_environment <- private$execute_code(
        code = code_container,
        # environment needs also this var to mutate self
        vars = c(private$vars, setNames(list(self), self$get_dataname()))
      )
      new_set <- execution_environment[[self$get_dataname()]]
      super$initialize(new_set)

      return(invisible(self))
    },
    #' @description
    #' Adds variables which code depends on
    #'
    #' @param vars (list)\cr
    #'   In case when this object code depends on the \code{raw_data} from the other
    #'   \code{RelationalDataset}, \code{RelationalDatasetConnector} object(s) or other constant value,
    #'   this/these object(s) should be included
    set_vars = function(vars) {
      stopifnot(is_fully_named_list(vars))

      if (length(vars) > 0) {
        # include only new (by name) variable
        private$vars <- c(private$vars, vars)
      }

      return(invisible(NULL))
    },
    #' Sets reproducible code
    #'
    #'
    #' @param code (\code{character}) Code to mutate the dataset. Must contain the
    #'  \code{dataset$dataname}
    set_code = function(code) {
      stopifnot(is_character_vector(code, 0, 1))

      if (length(code) > 0 && code != "") {
        private$code$set_code(code = code,
                              dataname = self$get_datanames(),
                              deps = names(private$vars))
      }

      return(invisible(NULL))
    },
    #' @description
    #'   Check to determine if the raw data is reproducible from the \code{get_code()} code.
    #' @return
    #'   \code{TRUE} if the dataset generated from evaluating the
    #'   \code{get_code()} code is identical to the raw data, else \code{FALSE}.
    check = function() {
      if (!is_character_single(self$get_code()) || !grepl("\\w+", self$get_code())) {
        stop(
          sprintf(
            "Cannot check preprocessing code of '%s' - code is empty.",
            self$get_dataname()
          )
        )
      }

      execution_environment <- private$execute_code(code = self$get_code_class(),
                                                    vars = private$vars)
      new_set <- execution_environment[[self$get_dataname()]]

      res_check <- tryCatch({
        identical(self$get_raw_data(), new_set)
      }, error = function(e) {
        FALSE
      })

      return(res_check)
    }
  ),
  private = list(
    ## __Private Fields ====
    .dataname = character(0),
    code = NULL, # CodeClass after initialization
    vars = list(),
    label = character(0),
    ## __Private Methods ====
    # Evaluate script code to modify data or to reproduce data
    #
    # Evaluate script code to modify data or to reproduce data
    # @inheritParams as_relational
    # @param vars (named \code{list}) additional pre-requisite vars to execute code
    # @return (\code{environment}) which stores modified \code{x}
    execute_code = function(code, vars = list()) {
      stopifnot(is(code, "CodeClass"))
      stopifnot(is_fully_named_list(vars))
      execution_environment <- new.env(parent = parent.env(globalenv()))

      for (vars_idx in seq_along(vars)) {
        var_name <- names(vars)[[vars_idx]]
        var_value <- vars[[vars_idx]]
        if (is(var_value, "RawDatasetConnector") || is(var_value, "RawDataset")) {
          var_value <- get_raw_data(var_value)
        }
        assign(envir = execution_environment, x = var_name, value = var_value)
      }

      code$eval(envir = execution_environment)

      if (!is.data.frame(execution_environment[[self$get_dataname()]])) {
        out_msg <- sprintf(
          "\n%s\n\n - Code from %s need to return a data.frame.",
          self$get_code(),
          self$get_dataname()
        )

        rlang::with_options(
          .expr = stop(out_msg, call. = FALSE),
          warning.length = max(min(8170, nchar(out_msg) + 30), 100)
        )
      }
      return(execution_environment)
    }
  ),
  ## __Active Methods ====
  active = list(
    #' @field dataname for backwards compatibility
    dataname = function() {
      private$.dataname
    }
  )
)

## Constructors ====

#' Constructor for \link{NamedDataset} class
#'
#' @inheritParams raw_dataset
#' @param dataname (\code{character})\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param code (\code{character})\cr
#'   A character string defining the code needed to produce the data set in \code{x}
#'
#' @param label (\code{character})\cr
#'   Label to describe the dataset
#'
#' @param vars (named \code{list})) \cr
#'   In case when this object code depends on the \code{raw_data} from the other
#'   \code{RelationalDataset} object(s) or other constant value,
#'   this/these object(s) should be included as named element of the list.
#'
#' @return \link{NamedDataset} object
#' @export
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL_dataset <- named_dataset(dataname = "ADSL", x = ADSL)
#'
#' ADSL_dataset$get_dataname()
#'
#' ADSL_dataset <- named_dataset(dataname = "ADSL",
#'   x = ADSL,
#'   label = "AdAM subject-level dataset",
#'   code = "ADSL <- radsl(cached = TRUE)"
#' )
#'
#' ADSL_dataset$get_dataset_label()
#' ADSL_dataset$get_code()
#'
named_dataset <- function(dataname, x, code = character(0), label = character(0), vars = list()) {
  NamedDataset$new(x, dataname, code, label, vars = vars)
}
