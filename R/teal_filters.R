#' Filter settings for teal applications
#'
#' Filter settings for teal applications
#'
#' @inheritParams teal.slice::filter_settings
#' @param mapping (`named list`)\cr
#'  Each element of the list should contain character vector of `teal_slices` `id` (see
#'  [teal.slice::filter_conf()]). Filters referred in list elements will be set on the startup of a
#'  `teal` application.
#'  Names of the list should correspond to `teal_module` `label` set in [module()] function.
#'
#' @param module_specific (`logical(1)`)\cr
#'  - `TRUE` when filter panel should be module-specific. All modules can have different set
#'   of filters specified - see `mapping` argument.
#'  - `FALSE` when one filter panel needed to all modules. All filters will be shared
#'    by all modules.
#'
#' @examples
#' filter <- teal::teal_filters(
#'   teal.slice::filter_conf(dataname = "iris", varname = "Species", id = "species"),
#'   teal.slice::filter_conf(dataname = "iris", varname = "Sepal.Length", id = "sepal_length"),
#'   teal.slice::filter_conf(
#'     dataname = "iris", id = "long_petals", title = "Long petals", expr = "Petal.Length > 5"
#'   ),
#'   teal.slice::filter_conf(dataname = "mtcars", varname = "mpg", id = "mtcars_mpg"),
#'   mapping = list(
#'     module1 = c("species", "sepal_length"),
#'     module2 = c("mtcars_mpg"),
#'     global_filters = "long_petals"
#'   )
#' )
#'
#' app <- teal::init(
#'   modules = list(
#'     module("module1"),
#'     module("module2")
#'   ),
#'   data = list(iris, mtcars),
#'   filter = filter
#' )
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#'
#' @export
teal_filters <- function(...,
                         exclude_varnames = NULL,
                         include_varnames = NULL,
                         count_type = NULL,
                         module_add = TRUE,
                         mapping = list(),
                         module_specific = length(mapping) > 0) {
  shiny::isolate({
    checkmate::assert_list(mapping, names = "named")
    checkmate::assert_flag(module_specific)
    checkmate::assert_flag(module_add)
    modules_mapped <- setdiff(names(mapping), "global_filters")
    if (length(modules_mapped) && !module_specific) {
      stop(
        "`mapping` is specified for modules (", toString(modules_mapped), ") even though `module_specific` isn't TRUE.",
        "Please set module_specific to `TRUE` or specify filters without mapping."
      )
    }

    fs <- teal.slice::filter_settings(
      ...,
      exclude_varnames = exclude_varnames,
      include_varnames = include_varnames,
      count_type = count_type,
      module_add = module_add
    )

    all_slice_id <- vapply(fs, `[[`, character(1), "id")
    for (i in names(mapping)) {
      failed_slice_id <- setdiff(mapping[[i]], all_slice_id)
      if (length(failed_slice_id)) {
        stop(sprintf(
          "id of filters in mapping '%s' don't match any available filter.\n %s not in %s",
          i,
          toString(failed_slice_id),
          toString(all_slice_id)
        ))
      }
    }

    attr(fs, "mapping") <- mapping
    attr(fs, "module_specific") <- module_specific
    class(fs) <- c("modules_filter_settings", class(fs))
    fs
  })
}

#' Deep copy `teal_slices`
#'
#' it's important to create a new copy of `teal_slices` when
#' starting a new `shiny` session. Otherwise, object will be shared
#' by multiple users as it is created in global environment before
#' `shiny` session starts.
#' @param filter (`teal_slices`)
#' @return `teal_slices`
#' @keywords internal
deep_copy_filter <- function(filter) {
  shiny::isolate({
    filter_copy <- lapply(filter, function(slice) {
      do.call(teal.slice::filter_conf, args = reactiveValuesToList(slice))
    })
    attributes(filter_copy) <- attributes(filter)
    return(filter_copy)
  })
}
