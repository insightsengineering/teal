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
NamedDataset <- R6::R6Class( # nolint
  "NamedDataset",
  inherit = RawDataset,
  ## NamedDataset ====
  ## __Public Methods ====
  public = list(
    #' @description
    #' initialize a \code{NamedDataset} class object
    #' @param x (\code{data.frame})
    #' @param dataname (\code{character}) A given name for the dataset
    #'   it may not contain spaces
    #' @param code (\code{character}) A character string defining the code
    #'   needed to produce the data set in \code{x}
    #' @param label (\code{character}) Label to describe the dataset
    #' @import utils.nest
    initialize = function(x, dataname, code = character(0), label = character(0)) {
      # Run RawDataset initialization
      super$initialize(x)

      self$set_dataname(dataname)
      self$set_code(code)
      self$set_label(label)
      return(invisible(self))
    },
    #' @description
    #' Derive the \code{name} which was former called \code{dataname}
    get_dataname = function() {
      private$.dataname
    },
    #' @description
    #' Set the name for the dataset
    #' @param dataname (\code{character}) the new name
    set_dataname = function(dataname) {
      stopifnot(utils.nest::is_character_single(dataname))
      stopifnot(!grepl("\\s", dataname))
      private$.dataname <- dataname
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
    #' Get code to get data
    #'
    #' @param deparse (\code{logical}) whether return deparsed form of a call
    #'
    #' @return optionally deparsed \code{call} object
    get_code = function(deparse = TRUE) {
      stopifnot(is_logical_single(deparse))
      code <- if (deparse) {
        paste(
          vapply(
            private$.code,
            function(x) {
              paste(
                deparse(x, width.cutoff = 80L),
                collapse = "\n"
              )
            },
            FUN.VALUE = character(1)
          ),
          collapse = "\n"
        )

      } else {
        private$.code
      }
      return(code)
    },
    #' @description
    #' Set the code for the dataset
    #' @param code (\code{character}) the new code
    set_code = function(code) {
      stopifnot(is_character_vector(code, min_length = 0, max_length = 1))

      if (length(code) > 0) {
        private$.code <- as.list(as.call(parse(text = code)))
      }

      invisible(TRUE)
    },
    #' @description
    #' Derive the dataset_label
    get_dataset_label = function() {
      private$.label
    }
  ),
  ## __Private Methods ====
  private = list(
    .dataname = character(0),
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
      private$.dataname
    }
  )
)

#' Constructor for \link{NamedDataset} class
#'
#' @param dataname (\code{character}) A given name for the dataset
#'   it may not contain spaces
#' @param x (\code{data.frame})
#' @param code (\code{character}) A character string defining the code
#'   needed to produce the data set in \code{x}
#' @param label (\code{character}) Label to describe the dataset
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
named_dataset <- function(dataname, x, code = character(0), label = character(0)) {
  NamedDataset$new(x, dataname, code, label)
}
