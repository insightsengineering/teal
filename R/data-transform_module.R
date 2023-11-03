#' `delayed_data` for `teal_data`
#'
#' Function creates object of class `delayed_data` which allows
#' `teal` app developer to transform freely `teal_data` object passed to `data` argument in
#' [teal::init()]. This helps in case when app developer wants to use `teal` app
#' where `data` can be influenced by app user. For example, app developer can create
#' `teal` app which allows user to connect to database and then use data from this database.
#' @param ... (`any`) arguments passed to `server` function.
#' @param ui (`function(id)`) function to create UI
#' @param server (`function(id)`) `shiny` server which returns `teal_data` object wrapped in
#' `reactive`. `server` should have `id` argument and exactly the same formals as specified in `...`.
#' @export
delayed_data <- function(ui, server, ...) {
  checkmate::assert_function(ui, args = "id")
  server_args <- list(...)
  if (length(server_args) && is.null(names(server_args))) {
    stop("All arguments passed to delayed_data() should be named")
  }

  server_formals <- names(formals(server))
  extra_args <- setdiff(names(server_args), server_formals)
  if (length(extra_args) > 0) {
    stop(
      "Unexpected arguments specified in delayed_data(): ",
      toString(extra_args),
      "\n arguments specified in `...` should be the same as in `server` function",
      call. = FALSE
    )
  }

  extra_formals <- setdiff(server_formals, c("id", names(server_args)))
  if (length(extra_formals) > 0) {
    stop(
      "Missing arguments specified in delayed_data(): ",
      toString(extra_formals),
      "\n arguments specified in `...` should be the same as in `server` function",
      call. = FALSE
    )
  }

  structure(
    list(ui = ui, server = server),
    server_args = server_args,
    class = "delayed_data"
  )
}
