#' Evaluate expression on `teal_data_module`
#'
#' @details
#' `within` is a convenience function for evaluating inline code inside the environment of a `teal_data_module`.
#' It accepts only inline expressions (both simple and compound) and allows for injecting values into `expr` through
#' the `...` argument: as `name:value` pairs are passed to `...`, `name` in `expr` will be replaced with `value.`
#'
#' @param data (`teal_data_module`) object
#' @param expr (`expression`) to evaluate. Must be inline code. See
#' @param ... See `Details`.
#'
#' @return
#' `within` returns a `teal_data_module` object with a delayed evaluation of `expr` when the module is run.
#'
#' @examples
#' within(tdm, dataset1 <- subset(dataset1, Species == "virginica"))
#'
#' # use additional parameter for expression value substitution.
#' valid_species <- "versicolor"
#' within(tdm, dataset1 <- subset(dataset1, Species %in% species), species = valid_species)
#' @include teal_data_module.R
#' @name within
#' @rdname teal_data_module
#'
#' @export
#'
within.teal_data_module <- function(data, expr, ...) {
  expr <- substitute(expr)
  extras <- list(...)

  # Add braces for consistency.
  if (!identical(as.list(expr)[[1L]], as.symbol("{"))) {
    expr <- call("{", expr)
  }

  calls <- as.list(expr)[-1]

  # Inject extra values into expressions.
  calls <- lapply(calls, function(x) do.call(substitute, list(x, env = extras)))

  eval_code(object = data, code = as.expression(calls))
}
