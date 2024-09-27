#' Modify object in teal_data environment
#' @export
setGeneric(
  "decorate_teal_data",
  function(ui = function(id) NULL, server, expr) {
    standardGeneric("decorate_teal_data")
  }
)

setMethod(
  "decorate_teal_data",
  signature = list(server = "missing", expr = "missing"),
  function(ui = function(id) NULL, server, expr) {
    teal_transform_module(ui, server = function(id, data) data)
  }
)

setMethod(
  "decorate_teal_data",
  signature = list(server = "function"),
  function(ui = function(id) NULL, server, expr) {
    if (!missing(expr)) {
      stop("expr argument ignored when ui and server are specified")
    }
    teal_transform_module(ui, server)
  }
)

setMethod( # comment: this could be teal_transform_module method
  "decorate_teal_data",
  signature = list(expr = "language"),
  function(ui = function(id) NULL, server, expr) {
    decorate_teal_data(
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          reactive({
            req(data())
            data() |> eval_code(expr)
          })
        })
      }
    )
  }
)

setMethod( # comment: this could be teal_transform_module method
  "decorate_teal_data",
  signature = list(expr = "language"),
  function(ui = function(id) NULL, server, expr) {
    decorate_teal_data(
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          reactive({
            req(data())
            data() |> eval_code(expr)
          })
        })
      }
    )
  }
)
