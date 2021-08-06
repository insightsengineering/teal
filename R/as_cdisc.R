#' Convert a `Dataset(Connector)` object to a `CDISCDataset(Connector)` object
#'
#' Convert a `Dataset(Connector)` object to a `CDISCDataset(Connector)` object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @note If passed a \code{cdisc}-flavored object it returns the unmodified object.
#'
#' @param x an object of `Dataset` or `DatasetConnector` class
#' @inheritParams cdisc_dataset
#'
#' @return (`CDISCDataset` or `CDISCDatasetConnector`) object
#'
#' @export
as_cdisc <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  if (any(class(x) %in% c("CDISCDataset", "CDISCDatasetConnector"))) {
    x
  } else {
    UseMethod("as_cdisc")
  }
}

#' @rdname as_cdisc
#' @export
#' @examples
#' # Dataset --------
#'
#' library(scda)
#' as_cdisc(
#'   dataset(
#'     "ADSL",
#'     synthetic_cdisc_data("latest")$adsl,
#'     keys = get_cdisc_keys("ADSL"),
#'     code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#'   )
#' )
#' as_cdisc(
#'   dataset(
#'     "ADAE",
#'     synthetic_cdisc_data("latest")$adae,
#'     keys = get_cdisc_keys("ADAE"),
#'     code = "ADAE <- synthetic_cdisc_data(\"latest\")$adae"
#'   ),
#'   parent = "ADSL"
#' )
as_cdisc.Dataset <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  if (length(get_keys(x)) > 0 || !(get_dataname(x) %in% names(default_cdisc_keys))) {
    cdisc_dataset(
      dataname = get_dataname(x),
      x = get_raw_data(x),
      keys = get_keys(x),
      parent = parent,
      label = get_dataset_label(x),
      code = x$get_code_class()
    )
  } else {
    cdisc_dataset(
      dataname = get_dataname(x),
      x = get_raw_data(x),
      parent = parent,
      label = get_dataset_label(x),
      code = x$get_code_class()
    )
  }
}

#' @rdname as_cdisc
#' @export
#' @examples
#' # DatasetConnector --------
#'
#' library(scda)
#' pull_fun_adsl <- callable_function(
#'   function() {synthetic_cdisc_data("latest")$adsl}
#' )
#' as_cdisc(
#'   dataset_connector(
#'     "ADSL",
#'     pull_fun_adsl,
#'     keys = get_cdisc_keys("ADSL")
#'   )
#' )
#'
#' pull_fun_adae <- callable_function(
#'   function() {synthetic_cdisc_data("latest")$adae}
#' )
#' as_cdisc(
#'   dataset_connector(
#'     "ADAE",
#'     pull_fun_adae,
#'     keys = get_cdisc_keys("ADAE")
#'   ),
#'   parent = "ADSL"
#' )
#'
as_cdisc.DatasetConnector <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  ds <- tryCatch(
    expr = get_dataset(x),
    error = function(e) NULL
  )
  if (!is.null(ds)) {
    warning(
      "Pulled 'dataset' from 'x' will not be passed to CDISCDatasetConnector.
      Avoid pulling before conversion."
    )
  }

  cdisc_dataset_connector(
    dataname = get_dataname(x),
    pull_callable = x$get_pull_callable(),
    keys = get_keys(x),
    parent = parent,
    label = get_dataset_label(x),
    vars = x$.__enclos_env__$private$pull_vars
  )
}
