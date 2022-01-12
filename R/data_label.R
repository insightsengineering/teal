#' Get dataset label attribute
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param data \code{data.frame} from which attribute is extracted
#'
#' @return (\code{character}) label or \code{NULL} if it's missing
#'
#' @export
#'
#' @examples
#' library(scda)
#' data_label(synthetic_cdisc_data("latest")$adsl)
data_label <- function(data) {
  attr(data, "label")
}

#' Set dataset label attribute
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param x \code{data.frame} for which attribute is set
#' @param value (\code{character}) label
#'
#' @return modified \code{x} object
#'
#' @export
#'
#' @examples
#' library(scda)
#' x <- synthetic_cdisc_data("latest")$adsl
#' data_label(x) <- "My custom label"
#' data_label(x)
`data_label<-` <- function(x, value) { # nolint
  stopifnot(is.data.frame(x))
  checkmate::assert_string(value)

  attr(x, "label") <- value
  x
}

#' Function that returns the default keys for a `CDISC` dataset by name
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param dataname name of the `CDISC` dataset
#'
#' @return \code{keys} object
#'
#' @export
#'
#' @examples
#' get_cdisc_keys("ADSL")
get_cdisc_keys <- function(dataname) {
  checkmate::assert_string(dataname)

  if (!(dataname %in% names(default_cdisc_keys))) {
    stop(sprintf(
      "There is no dataset called: %s \n  List of supported cdisc_datasets:\n   %s",
      dataname, paste(names(default_cdisc_keys), collapse = ", ")
    ))
  } else {
    cdisc_keys <- default_cdisc_keys[[dataname]]$primary

    return(cdisc_keys)
  }
}

#' Function that extract labels from CDISC dataset
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param data (\code{data.frame}) table to extract the labels from
#' @inheritParams rtables::var_labels
#'
#' @return labels
#'
#' @export
#'
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' get_labels(ADSL)
get_labels <- function(data, fill = TRUE) {
  stopifnot(is.data.frame(data))
  checkmate::assert_flag(fill)

  cdisc_labels <- list(
    "dataset_label" = data_label(data),
    "column_labels" = rtables::var_labels(data, fill = fill)
  )
  return(cdisc_labels)
}

#' Function that extract column labels from CDISC dataset
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param data (\code{data.frame}) any CDISC data set
#' @param columns optional, (\code{character}) column names to extract the labels from. If (\code{NULL}) then all
#'   columns are being used.
#' @inheritParams rtables::var_labels
#'
#' @return labels of the columns
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' get_variable_labels(ADSL)
#' get_variable_labels(ADSL, c("AGE", "RACE", "BMRKR1"))
#' get_variable_labels(ADSL, c("AGE", "RACE", "BMRKR1", "xyz"))
#'
#' ADSL$NEW_COL <- 1
#' get_variable_labels(ADSL, c("AGE", "RACE", "BMRKR1", "NEW_COL"))
#' get_variable_labels(ADSL, c("AGE", "RACE", "BMRKR1", "NEW_COL"), fill = FALSE)
get_variable_labels <- function(data, columns = NULL, fill = TRUE) {
  stopifnot(is.data.frame(data))
  checkmate::assert_character(columns, min.len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_flag(fill)

  columns <- if_null(columns, colnames(data))
  labels <- as.list(get_labels(data, fill = fill)$column_labels)
  # convert NULL into NA_character for not-existing column
  res <- vapply(columns, function(x) if_null(labels[[x]], NA_character_), character(1))

  return(res)
}
