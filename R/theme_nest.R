#' Returns a custom `NEST` `ggplot2` theme
#'
#' @param ... (`list`) additional arguments to the `ggplot2` theme function
#'
#' @export
#' @examples
#' plot <- ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width)) +
#'   ggplot2::geom_point() +
#'   theme_nest()
theme_nest <- function(...) {
  ggplot2::theme_bw(...)
}
