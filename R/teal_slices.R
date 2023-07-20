#' Filter settings for teal applications
#'
#' Filter settings for teal applications
#'
#' @inheritParams teal.slice::teal_slices
#'
#' @param module_specific (`logical(1)`)\cr
#'  - `TRUE` when filter panel should be module-specific. All modules can have different set
#'   of filters specified - see `mapping` argument.
#'  - `FALSE` when one filter panel needed to all modules. All filters will be shared
#'    by all modules.
#' @param mapping (`named list`)\cr
#'  Specifies which filters will be available in which modules on app start.
#'  Elements should contain character vector of `teal_slice` `id`s (see [teal.slice::teal_slice()]).
#'  Names of the list should correspond to `teal_module` `label` set in [module()] function.
#'  If missing, all filters will be applied to all modules.
#'  If empty list, all filters will be available to all modules but will start inactive.
#'
#' @examples
#' filter <- teal_slices(
#'   teal.slice::teal_slice(dataname = "iris", varname = "Species", id = "species"),
#'   teal.slice::teal_slice(dataname = "iris", varname = "Sepal.Length", id = "sepal_length"),
#'   teal.slice::teal_slice(
#'     dataname = "iris", id = "long_petals", title = "Long petals", expr = "Petal.Length > 5"
#'   ),
#'   teal.slice::teal_slice(dataname = "mtcars", varname = "mpg", id = "mtcars_mpg"),
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
teal_slices <- function(...,
                        exclude_varnames = NULL,
                        include_varnames = NULL,
                        count_type = NULL,
                        allow_add = TRUE,
                        module_specific = FALSE,
                        mapping) {
  shiny::isolate({
    checkmate::assert_flag(allow_add)
    checkmate::assert_flag(module_specific)

    slices <- list(...)
    all_slice_id <- vapply(slices, `[[`, character(1L), "id")

    if (missing(mapping)) {
      mapping <- list(global_filters = all_slice_id)
    }
    checkmate::assert_list(mapping, names = "named")

    tss <- teal.slice::teal_slices(
      ...,
      exclude_varnames = exclude_varnames,
      include_varnames = include_varnames,
      count_type = count_type,
      allow_add = allow_add
    )

    failed_slice_id <- setdiff(unlist(mapping), all_slice_id)
    if (length(failed_slice_id)) {
      stop(sprintf(
        "id of filters in mapping '%s' don't match any available filter.\n %s not in %s",
        i,
        toString(failed_slice_id),
        toString(all_slice_id)
      ))
    }

    attr(tss, "mapping") <- mapping
    attr(tss, "module_specific") <- module_specific
    class(tss) <- c("modules_teal_slices", class(tss))
    tss
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
  checkmate::assert_class(filter, "teal_slices")
  shiny::isolate({
    filter_copy <- lapply(filter, function(slice) {
      teal.slice::as.teal_slice(as.list(slice))
    })
    attributes(filter_copy) <- attributes(filter)
    filter_copy
  })
}
