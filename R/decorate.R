setOldClass("teal_transform_module")

#' Modify object in teal_data environment
#' @export
setGeneric(
  "decorate_teal_data",
  function(x, output_name) {
    standardGeneric("decorate_teal_data")
  }
)

setMethod(
  "decorate_teal_data",
  signature = list(x = "teal_transform_module"),
  function(x, output_name) x
)

setMethod(
  "decorate_teal_data",
  signature = list(x = "language"),
  function(x, output_name) {
    teal_transform_module(
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          reactive({
            req(data())
            eval_code(data(), code = x)
          })
        })
      }
    )
  }
)

setMethod(
  "decorate_teal_data",
  signature = list(x = "function"),
  function(x, output_name) {
    checkmate::check_string(output_name)
    formal_args <- names(formals(x)) # how the args are named is here
    expr <- do.call(
      substitute,
      list(
        expr = body(x),
        env = setNames(list(as.name(output_name)), formal_args[[1]])
      )
    )
    decorate_teal_data(x = expr)
  }
)
