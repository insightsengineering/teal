#' Convert a `TealDataset(Connector)` object to a `CDISCTealDataset(Connector)` object
#'
#' Convert a `TealDataset(Connector)` object to a `CDISCTealDataset(Connector)` object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @note If passed a `CDISC`-flavored object it returns the unmodified object.
#'
#' @param x an object of `TealDataset` or `TealDatasetConnector` class
#' @inheritParams cdisc_dataset
#'
#' @return (`CDISCTealDataset` or `CDISCTealDatasetConnector`) object
#'
#' @export
as_cdisc <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  if (any(class(x) %in% c("CDISCTealDataset", "CDISCTealDatasetConnector"))) {
    x
  } else {
    UseMethod("as_cdisc")
  }
}

#' @rdname as_cdisc
#' @export
#' @examples
#' # TealDataset --------
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
as_cdisc.TealDataset <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  if (length(get_keys(x)) > 0 || !(get_dataname(x) %in% names(default_cdisc_keys))) {
    cdisc_dataset(
      dataname = get_dataname(x),
      x = get_raw_data(x),
      keys = get_keys(x),
      parent = parent,
      label = get_dataset_label(x),
      code = x$get_code_class(),
      metadata = x$get_metadata()
    )
  } else {
    cdisc_dataset(
      dataname = get_dataname(x),
      x = get_raw_data(x),
      parent = parent,
      label = get_dataset_label(x),
      code = x$get_code_class(),
      metadata = x$get_metadata()
    )
  }
}

#' @rdname as_cdisc
#' @export
#' @examples
#' # TealDatasetConnector --------
#'
#' library(scda)
#' pull_fun_adsl <- callable_function(
#'   function() {
#'     synthetic_cdisc_data("latest")$adsl
#'   }
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
#'   function() {
#'     synthetic_cdisc_data("latest")$adae
#'   }
#' )
#' as_cdisc(
#'   dataset_connector(
#'     "ADAE",
#'     pull_fun_adae,
#'     keys = get_cdisc_keys("ADAE")
#'   ),
#'   parent = "ADSL"
#' )
as_cdisc.TealDatasetConnector <- function(x, parent = `if`(identical(get_dataname(x), "ADSL"), character(0), "ADSL")) {
  ds <- tryCatch(
    expr = get_dataset(x),
    error = function(e) NULL
  )
  if (!is.null(ds)) {
    warning(
      "Pulled 'dataset' from 'x' will not be passed to CDISCTealDatasetConnector.
      Avoid pulling before conversion."
    )
  }

  cdisc_dataset_connector(
    dataname = get_dataname(x),
    pull_callable = x$get_pull_callable(),
    keys = get_keys(x),
    parent = parent,
    label = get_dataset_label(x),
    vars = x$.__enclos_env__$private$pull_vars,
    metadata = x$.__enclos_env__$private$metadata
  )
}
