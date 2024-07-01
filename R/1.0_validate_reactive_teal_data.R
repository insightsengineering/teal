validate_reactive_teal_data <- function(data) {
  if (!is.reactive(data)) {
    stop("The `teal_data_module` passed to `data` must return a reactive expression.", call. = FALSE)
  }
  reactive({
    # custom module can return error
    data_out <- tryCatch(data(), error = function(e) e)

    # there is an empty reactive cycle on init!
    if (inherits(data_out, "shiny.silent.error") && identical(data_out$message, "")) {
      return(NULL)
    }

    # to handle qenv.error
    if (inherits(data_out, "qenv.error")) {
      validate(
        need(
          FALSE,
          paste(
            "Error when executing `teal_data_module` passed to `data`:\n ",
            paste(data_out$message, collapse = "\n"),
            "\n Check your inputs or contact app developer if error persists."
          )
        )
      )
    }

    # to handle module non-qenv errors
    if (inherits(data_out, "error")) {
      validate(
        need(
          FALSE,
          paste(
            "Error when executing `teal_data_module` passed to `data`:\n ",
            paste(data_out$message, collpase = "\n"),
            "\n Check your inputs or contact app developer if error persists."
          )
        )
      )
    }

    validate(
      need(
        inherits(data_out, "teal_data"),
        paste(
          "Error: `teal_data_module` passed to `data` failed to return `teal_data` object, returned",
          toString(sQuote(class(data_out))),
          "instead.",
          "\n Check your inputs or contact app developer if error persists."
        )
      )
    )

    if (!length(teal.data::datanames(data_out))) {
      warning("`data` object has no datanames. Default datanames are set using `teal_data`'s environment.")
    }

    data_out
  })
}

validate_module_datanames <- function(data, modules) {
  is_modules_ok <- check_modules_datanames(modules, teal_data_datanames(data))
  if (isFALSE(is_modules_ok)) {
    validate(need(isTRUE(is_modules_ok), sprintf("%s. Contact app developer.", is_modules_ok)))
  }
}

validate_filter_datanames <- function(data, filter) {
  is_filter_ok <- check_filter_datanames(filter, teal_data_datanames(data))
  if (isFALSE(is_filter_ok)) {
    showNotification(
      "Some filters were not applied because of incompatibility with data. Contact app developer.",
      type = "warning",
      duration = 10
    )
    warning(is_filter_ok)
  }
}
