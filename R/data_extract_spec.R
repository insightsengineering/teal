#' Data Extract input for teal modules
#'
#' The Data Extract input can be used to filter and select columns from a data
#' set. This function enables such an input in teal.
#' Please use the constructor function \link{data_extract_spec} to set it up.
#'
#' @export
#' @name data_extract_spec
#'
#' @section Module Development:
#' \describe{
#' From this function's output a \code{teal.devel::data_extract_input} can
#' be constructed. This input can be read by a \code{teal.devel::data_extract_module} module.
#' }
#'
#' @importFrom methods is
#' @importFrom utils.nest is.class.list
#' @param dataname (\code{character}) The name of the \code{teal} dataset to
#'   be extracted. This dataset has to be handed over to the \code{data} argument of the
#'   \code{\link[teal]{init}} function.
#' @param filter (\code{filter_spec}-S3-class) Setup of the filtering of
#'  key columns inside the dataset. This setup can be created using the \code{\link{filter_spec}}
#'  function.
#' @param columns (\code{column_spec}-S3 class) Columns to be selected from the input dataset
#'  mentioned in \code{dataname.} The setup can be created using \code{\link{columns_spec}} function.
#'
#' @section Examples:
#' \describe{
#' \enumerate{
#'   \item{Dataset with multiple filters and column selection}{
#'     \preformatted{
#'adtte_filters <- filter_spec(
#' vars = c("PARAMCD", "CNSR"),
#' sep = "-",
#' choices = c("OS-1" = "OS-1", "OS-0" = "OS-0", "PFS-1" = "PFS-1"),
#' selected = "OS-1",
#' multiple = FALSE,
#' label = "Choose endpoint and Censor"
#' )
#'
#' data_extract_spec(
#'   dataname = "ADTTE",
#'   filter = adtte_filters,
#'   columns = columns_spec(
#'     choices = c("AVAL", "BMRKR1", "AGE"),
#'         selected = c("AVAL", "BMRKR1"),
#'         multiple = TRUE,
#'         fixed = FALSE,
#'         label = "Column"
#'     )
#' )
#'     }
#'     \if{html}{
#'       \figure{data_extract_spec_1.png}{options: alt="Dataset with multiple filters and column selection"}
#'     }
#'     \if{html}{
#'       \figure{data_extract_spec_12.png}{options: alt="Dataset with multiple filters and column selection"}
#'     }
#'     \if{html}{
#'       \figure{data_extract_spec_11.png}{options: alt="Dataset with multiple filters and column selection"}
#'     }
#'   }
#'   \item{Data extract without filtering}{
#'   \preformatted{
#'
#' data_extract_spec(
#'   dataname = "ADSL",
#'   filter = NULL,
#'   columns = columns_spec(
#'     choices = c("AGE", "SEX", "USUBJID"),
#'         selected = c("SEX"),
#'         multiple = FALSE,
#'         fixed = FALSE
#'     )
#' )
#'   }
#'   }
#'   \if{html}{
#'       \figure{data_extract_spec_2.png}{options: alt="Data extract without filtering"}
#'     }
#' }
#'}
#'
#' @references \link{columns_spec} \link{filter_spec}
data_extract_spec <- function(dataname, columns, filter = NULL) {
  stopifnot(is.character(dataname), length(dataname) == 1)
  stopifnot(is(columns, "column_spec"), length(columns) >= 1)
  stopifnot(is.null(filter) || (is(filter, "filter_spec") & length(filter) >= 1) ||
    is.class.list("filter_spec")(filter))

  res <- list(dataname = dataname, columns = columns, filter = filter)
  class(res) <- "data_extract_spec"

  res
}
