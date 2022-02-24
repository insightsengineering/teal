#' Get dataset label attribute
#'
#' @description `r lifecycle::badge("stable")`
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
#' @description `r lifecycle::badge("stable")`
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
#' @description `r lifecycle::badge("stable")`
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

#' Extracts dataset and variable labels from a dataset.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param data (`data.frame`) table to extract the labels from
#' @param fill (`logical(1)`) if `TRUE`, the function will return variable names for columns with non-existent labels;
#'   otherwise will return `NA` for them
#'
#' @return `list` with two keys: `dataset_labels` and `column_labels`
#'
#' @export
#'
#' @examples
#' library(scda)
#' iris_with_labels <- iris
#' attributes(iris_with_labels)$label <- "Custom iris dataset with labels"
#' attributes(iris_with_labels["Sepal.Length"])$label <- stats::setNames(object = "Sepal Length", "Sepal.Length")
#' get_labels(iris_with_labels, fill = TRUE)
#' get_labels(iris_with_labels, fill = FALSE)
#'
get_labels <- function(data, fill = TRUE) {
  stopifnot(is.data.frame(data))
  checkmate::assert_flag(fill)

  column_labels <- Map(function(col, colname) {
    label <- attr(col, "label")
    if (is.null(label)) {
      if (fill) {
        colname
      } else {
        NA_character_
      }
    } else {
      if (!checkmate::test_string(label, na.ok = TRUE)) {
        stop("label for variable ", colname, " is not a character string")
      }
      as.vector(label) # because label might be a named vector
    }
  }, data, colnames(data))
  column_labels <- unlist(column_labels, recursive = FALSE, use.names = TRUE)

  list("dataset_label" = data_label(data), "column_labels" = column_labels)
}

#' Extracts column labels from a dataset
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @param data (\code{data.frame}) the dataset
#' @param columns optional, (\code{character}) column names to extract the labels from. If (\code{NULL}) then all
#'   columns are being used.
#' @inheritParams get_labels
#'
#' @return labels of the columns
#'
#' @export
#'
get_variable_labels <- function(data, columns = NULL, fill = TRUE) {
  lifecycle::deprecate_soft(
    when = "0.10.2",
    what = "get_variable_labels()",
    with = "variable_labels()",
    details = "get_variable_labels() is superseded and will be removed in future releases."
  )
  variable_labels(data, columns, fill)
}

#' Extracts column labels from a dataset.
#'
#' @description `r lifecycle::badge("stable")`
#' @details The variable labels are extracted from the `label` attribute of the columns. See the example
#' to see a way to set a label in a way this function can retrieve it.
#'
#' @param data (\code{data.frame}) the dataset
#' @param columns optional, (\code{character}) column names to extract the labels from. If (\code{NULL}) then all
#'   columns are being used.
#' @inheritParams get_labels
#'
#' @return labels of the columns
#'
#' @export
#'
#' @examples
#' custom_iris <- iris
#' variable_labels(iris) <- paste("Test label no", seq(from = 1, length.out = ncol(custom_iris)))
#' variable_labels(iris, fill = TRUE)
#' variable_labels(iris, columns = colnames(custom_iris)[1:3])
#'
#' # It's possible to set the labels manually
#' attr(custom_iris[[1]], "label") <- "A new test label"
#' variable_labels(custom_iris)
#'
variable_labels <- function(data, columns = NULL, fill = TRUE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(columns, min.len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_flag(fill)
  if (is.null(columns)) {
    columns <- colnames(data)
  }
  labels <- as.list(get_labels(data, fill = fill)$column_labels)
  # convert NULL into NA_character for not-existing column
  vapply(columns, FUN.VALUE = character(1), FUN = function(x) {
    if (is.null(labels[[x]])) {
      NA_character_
    } else {
      labels[[x]]
    }
  })
}

#' Sets the column labels of a `data.frame`.
#'
#' @param x (`data.frame`) object with columns
#' @param value (`charater`) labels
#' @return `x`
#'
#' @export
#'
#' @examples
#' variable_labels(iris) <- colnames(iris)
#'
`variable_labels<-` <- function(x, value) {
  checkmate::assert_data_frame(x)
  checkmate::assert_character(value, len = ncol(x))

  for (i in seq_along(x)) {
    if (!is.na(value[i])) attr(x[[i]], "label") <- value[i]
  }

  x
}


#' Copy and Change Variable Labels of a `data.frame`
#'
#' Relabels a subset of the variables.
#'
#' @inheritParams var_labels<-
#' @param ... name-value pairs, where name corresponds to a variable name in
#'   `x` and the value to the new variable label
#'
#' @return a copy of `x` with changed labels according to `...`
#'
#' @export
#'
#' @examples
#' x <- var_relabel(iris, Sepal.Length = "Sepal Length of iris flower")
#' variable_labels(x)
#'
var_relabel <- function(x, ...) {
  stopifnot(is.data.frame(x))
  if (missing(...)) {
    return(x)
  }
  dots <- list(...)
  varnames <- names(dots)
  stopifnot(!is.null(varnames))
  map_varnames <- match(varnames, colnames(x))
  if (any(is.na(map_varnames))) {
    stop("variables: ", paste(varnames[is.na(map_varnames)],
      collapse = ", "
    ), " not found")
  }
  if (any(vapply(dots, Negate(is.character), logical(1)))) {
    stop("all variable labels must be of type character")
  }
  for (i in seq_along(map_varnames)) {
    attr(x[[map_varnames[[i]]]], "label") <- dots[[i]]
  }
  x
}
