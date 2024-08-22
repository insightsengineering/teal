#' Execute and validate `teal_data_module`
#'
#' This is a low level module to handle `teal_data_module` execution and validation.
#' [teal_transform_module()] inherits from [teal_data_module()] so it is handled by this module too.
#' [srv_teal()] accepts various `data` objects and eventually they are all transformed to `reactive`
#' [teal_data()] which is a standard data class in whole `teal` framework.
#'
#' @section data validation:
#'
#' Executed [teal_data_module()] is validated and output is validated for consistency.
#' Output `data` is invalid if:
#' 1. [teal_data_module()] is invalid if server doesn't return `reactive`. **Immediately crashes an app!**
#' 2. `reactive` throws a `shiny.error` - happens when module creating [teal_data()] fails.
#' 3. `reactive` returns `qenv.error` - happens when [teal_data()] evaluates a failing code.
#' 4. `reactive` object doesn't return [teal_data()].
#' 5. [teal_data()] object lacks any `datanames` specified in the `modules` argument.
#'
#' `teal` (observers in `srv_teal`) always waits to render an app until `reactive` `teal_data` is
#' returned. If error 2-4 occurs, relevant error message is displayed to app user and after issue is
#' resolved app will continue to run. `teal` guarantees that errors in a data don't crash an app
#' (except error 1). This is possible thanks to `.fallback_on_failure` which returns input-data
#' when output-data fails
#'
#'
#' @param id (`character(1)`) Module id
#' @param data (`reactive teal_data`)
#' @param data_module (`teal_data_module`)
#' @param modules (`teal_modules` or `teal_module`) For `datanames` validation purpose
#' @param validate_shiny_silent_error (`logical`) If `TRUE`, then `shiny.silent.error` is validated and
#' error message is displayed.
#' Default is `FALSE` to handle empty reactive cycle on `init`.
#'
#' @return `reactive` `teal_data`
#'
#' @rdname module_teal_data
#' @name module_teal_data
#' @keywords internal
NULL

#' @rdname module_teal_data
ui_teal_data <- function(id, data_module) {
  checkmate::assert_string(id)
  checkmate::assert_class(data_module, "teal_data_module")
  ns <- NS(id)
  shiny::tagList(
    data_module$ui(id = ns("data"))
  )
}

#' @rdname module_teal_data
srv_teal_data <- function(id,
                          data,
                          data_module,
                          modules = NULL,
                          validate_shiny_silent_error = TRUE) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(data_module, "teal_data_module")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"), null.ok = TRUE)

  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_teal_data initializing.")

    data_out <- if (is_arg_used(data_module$server, "data")) {
      data_module$server(id = "data", data = data)
    } else {
      data_module$server(id = "data")
    }
    data_out

    # data_validated <- srv_validate_reactive_teal_data(
    #   id = "validate",
    #   data = data_out,
    #   modules = modules,
    #   validate_shiny_silent_error = validate_shiny_silent_error
    # )

    # .fallback_on_failure(
    #   this = data_validated,
    #   that = data,
    #   label = sprintf("Data element '%s' for module '%s'", id, modules$label)
    # )
  })
}
