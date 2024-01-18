#' Set global objects
#'
#' Class to manage global objects and bring them into scope of `shiny` modules.
#'
#' Singleton object to pass variables between modules. Instantiated in the package namespace.
#' Global objects are stored in the `session$userData` environment.
#' Every `shiny` module can register a setter functions for a global variable to be taken from storage
#' or created from scratch. Then, that function can be called anywhere in the app.
#'
#' @docType class
#' @name GlobalSetter
#' @rdname GlobalSetter
#'
#' @examples
#' GlobalSetter <- getFromNamespace("GlobalSetter", "teal")
#' setter <- GlobalSetter$new()
#' setter$add_setter("romans", function() {
#'   assign("romans", .romans, envir = .GlobalEnv)
#'   .romans
#'   })
#' setter
#' setter$set_global("romans")
#' exists("romans", envir = .GlobalEnv, inherits = TRUE)
#'
#' @keywords internal
#'
GlobalSetter <- R6::R6Class(
  classname = "GlobalSetter",

  # __Public Methods ----
  public = list(

    #' @description
    #' Set object.
    #' @param obj (`character(1)`) Name of object to set.
    #' @return
    #' Whatever the particular setter function returns, usually the global object.
    set_global = function(obj) {
      if (!shiny::isRunning()) {
        message("no shiny, no objects")
        return(NULL)
      }
      checkmate::assert_string(obj)
      checkmate::assert_choice(obj, private$objects)

      ind <- match(obj, private$objects)
      private$functions[[ind]]()
    },

    #' @description
    #' Add setter function for global object.
    #' @param obj (`character(1)`) Name of object.
    #' @param fun (`function`) Setter function.
    add_setter = function(obj, fun) {
      checkmate::assert_string(obj)
      checkmate::assert_function(fun)

      ind <- match(obj, private$objects, nomatch = length(private$objects) + 1L)

      if (is.element(obj, private$objects)) {
        if (identical(fun, private$functions[[ind]])) {
          message("this setter already exists")
          return(invisible(NULL))
        } else {
          message(sprintf("overwriting setter function for object '%s'", obj))
        }
      }

      private$objects[ind] <- obj
      private$functions[[ind]] <- fun
      invisible(NULL)
    },

    #' @noRd
    format = function(...) {
      paste(
        c(
          "Global Object Setter",
          if (length(private$objects)) sprintf("  objects: %s", toString(private$objects)) else "  empty"
        ),
        collapse = "\n"
      )
    }
  ),

  # __Private Members ----
  private = list(
    objects = character(0L), # objects for which setter functions have been defined
    functions = list() # setter functions
  ),
  cloneable = FALSE
)

GS <- GlobalSetter$new()
