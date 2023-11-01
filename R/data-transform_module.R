#' Transform module for `teal_data`
#'
#' Function creates object of class `teal_trnasform_module` which allows
#' `teal` app developer to transform freely `teal_data` object passed to `data` argument in
#' [teal::init()]. This helps in case when app developer wants to use `teal` app
#' where `data` can be influenced by app user. For example, app developer can create
#' `teal` app which allows user to connect to database and then use data from this database.
#' @param data `teal_data` object
#' @param ui (`function(id)`) function to create UI
#' @param server (`function(id, data)`) `shiny` server
#' which returns `teal_data` object wrapped in `reactive`.
#' @export
teal_transform <- function(data, ui, server) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_function(ui, args = "id")
  checkmate::assert_function(server, args = c("id", "data"))

  structure(
    list(ui = ui, server = server),
    data = data,
    class = "teal_transform_module"
  )
}
