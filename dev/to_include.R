Dataset <- R6::R6Class( # nolint
  "Dataset",
  ## Dataset ====
  ## __Public Methods ====
  public = list(
    raw_data = NULL,
    dataname = NULL,
    keys = NULL,
    source = NULL,
    initialize = function(raw_data, dataname, keys, source = NULL) {
      stopifnot(is.data.frame(raw_data))
      stopifnot(is.character(dataname) && length(dataname) == 1)
      stopifnot(is.character(keys))
      stopifnot(is.null(source) || (is.character(source) && length(source) == 1))
      self$raw_data <- raw_data
      self$dataname <- dataname
      self$keys <- keys
      self$source <- source
    },
    to_teal_data = function() {
      structure(
        self$raw_data,
        dataname = self$dataname,
        keys = self$keys,
        source = self$source
      )
    }
  )
)

# x <- data_for_teal(
#   modified_data,
#   c("USUBJID", "STUDYID"),
#   "radsl(N = 600)  %>% dplyr::mutate(A = 1)"
# )
# from_teal_data(x)
# todo: dataname should be part of teal_data
from_teal_data <- function(teal_data) {
  # "names", "class", "row.names" are from data frame
  stopifnot(identical(c("names", "class", "row.names", "keys", "source", "dataname") %in% attributes(teal_data)))

  raw_data <- teal_data
  attributes(raw_data) <- NULL
  Dataset$new(
    raw_data = raw_data,
    dataname = attr(teal_data, "dataname"),
    keys = attr(teal_data, "keys"),
    source = attr(teal_data, "source")
  )
}
