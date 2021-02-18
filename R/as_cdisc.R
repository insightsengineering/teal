#' Convert a `Dataset(Connector)` object to a `CDISCDataset(Connector)` object
#'
#' Convert a `Dataset(Connector)` object to a `CDISCDataset(Connector)` object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x an object of `Dataset` or `DatasetConnector` class
#' @inheritParams cdisc_dataset
#'
#' @return (`CDISCDataset` or `CDISCDatasetConnector`) object
#'
#' @export
as_cdisc <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  UseMethod("as_cdisc")
}

#' @rdname as_cdisc
#' @export
#' @examples
#' # Dataset --------
#'
#' library(random.cdisc.data)
#' as_cdisc(
#'   dataset(
#'     "ADSL",
#'     radsl(cached = TRUE),
#'     keys = get_cdisc_keys("ADSL"),
#'     code = "ADSL <- radsl(cached = TRUE)"
#'   )
#' )
#' as_cdisc(
#'   dataset(
#'     "ADAE",
#'     radae(cached = TRUE),
#'     keys = get_cdisc_keys("ADAE"),
#'     code = "ADAE <- radae(cached = TRUE)"
#'   ),
#'   parent = "ADSL"
#' )
as_cdisc.Dataset <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  return(
    cdisc_dataset(
      dataname = get_dataname(x),
      x = get_raw_data(x),
      keys = get_keys(x),
      parent = parent,
      label = get_dataset_label(x),
      code = get_code(x)
    )
  )
}

#' @rdname as_cdisc
#' @export
#' @examples
#' # DatasetConnector --------
#'
#' library(random.cdisc.data)
#' as_cdisc(
#'   rcd_dataset_connector(
#'     "ADSL",
#'     radsl,
#'     keys = get_cdisc_keys("ADSL")
#'   )
#' )
#' as_cdisc(
#'   rcd_dataset_connector(
#'     "ADAE",
#'     radae,
#'     keys = get_cdisc_keys("ADSL")
#'   ),
#'   parent = "ADSL"
#' )
as_cdisc.DatasetConnector <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  ds <- tryCatch(
    expr = get_dataset(x),
    error = function(e) NULL
  )
  if (!is.null(ds)) {
    warning(
      "Pulled 'dataset' from 'x' will not be passed to DatasetConnector.
      Avoid pulling before conversion."
    )
  }
  return(
    cdisc_dataset_connector(
      dataname = get_dataname(x),
      pull_callable = x$get_pull_callable(),
      keys = get_keys(x),
      parent = parent,
      label = get_dataset_label(x),
      vars = x$.__enclos_env__$private$pull_vars
    )
  )
}
