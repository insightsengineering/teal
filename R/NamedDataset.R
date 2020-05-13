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
#' named_data <- NamedDataset$new(
#'   x = data.frame(x = c(2, 2), y = c("a", "b"), stringsAsFactors = FALSE),
#'   name = "XY",
#'   code = "XY <- data.frame(x = c(2, 2),
#'     y = c('a', 'b'), stringsAsFactors = FALSE)"
#' )
#' named_data$ncol
#' named_data$get_code()
#' named_data$get_name()
#'
#' @export
NamedDataset <- R6::R6Class( # nolint
  "NamedDataset",
  inherit = RawDataset,
  ## NamedDataset ====
  ## __Public Methods ====
  public = list(
    #' @param x (\code{data.frame})
    #' @param name (\code{character}) A given name for the dataset
    #'   it may not contain spaces
    #' @param code (\code{character}) A character string defining the code
    #'   needed to produce the data set in \code{x}
    #' @param label (\code{character}) Label to describe the dataset
    #' @import utils.nest
    initialize = function(x, name, code = character(0), label = character(0)) {
      # Run RawDataset initialization
      super$initialize(x)

      self$set_name(name)
      self$set_code(code)
      self$set_label(label)
    },
    #' @description
    #' Derive the \code{name} which was former called \code{dataname}
    get_name = function() {
      private$.name
    },
    #' @description
    #' Set the name for the dataset
    #' @param name (\code{character}) the new name
    set_name = function(name) {
      stopifnot(!grepl("\\s", name))
      stopifnot(utils.nest::is_character_single(name))
      private$.name <- name
      invisible(NULL)
    },
    #' @description
    #' Derive the \code{label} which was former called \code{datalabel}
    get_label = function() {
      private$.label
    },
    #' @description
    #' Set the label for the dataset
    #' @param label (\code{character}) the new label
    set_label = function(label) {
      if (is.null(label)) {
        label <- character(0)
      }
      stopifnot(is_character_vector(label, min_length = 0, max_length = 1))
      private$.label <- label
      invisible(NULL)
    },
    #' @description
    #' Derive the code stored inside the object
    get_code = function() {
      private$.code
    },
    #' @description
    #' Set the code for the dataset
    #' @param code (\code{character}) the new code
    set_code = function(code) {
      stopifnot(is_character_vector(code, min_length = 0, max_length = 1))
      private$.code <- code
      invisible(NULL)
    },
    #' @description
    #' Derive the dataset_label
    get_dataset_label = function() {
      private$.label
    }
  ),
  ## __Private Methods ====
  private = list(
    .name = character(0),
    .code = NULL,
    .label = character(0)
  ),
  active = list(
    #' @field dataset_label for backwards compatibility
    dataset_label = function() {
      private$.label
    },
    #' @field dataname for backwards compatibility
    dataname = function() {
      private$.name
    }
  )
)
