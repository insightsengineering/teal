# Queue ====
#' @title  R6 Class - A First-In-First-Out Abstract Data Type
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @description
#' Implements the queue abstract data type. The last element added to this queue is
#' the last one to be returned from it.
#' @export
#'
Queue <- R6::R6Class( # nolint
  classname = "Queue",
  public = list(
    #' @description
    #' Adds another element to this queue.
    #'
    #' @param new_elements the elements to be added to this queue
    #'
    #' @return invisibly self
    push = function(new_elements) {
      if (is.R6(new_elements) || isS4(new_elements)) {
        private$set(new_elements, append = TRUE)
      } else {
        for (i in seq_along(new_elements)) {
          if (length(new_elements[i]) == 1 || is.R6(new_elements[i])) {
            # new_elements[i] does not discard names if it's a names list
            private$set(new_elements[i], append = TRUE)
          }
        }
      }

      invisible(self)
    },
    #' @description
    #' Returns the first added element to this queue and removes it
    #' from this queue.
    #'
    #' @return the first added element to this queue or `NULL` if this queue is empty
    pop = function() {
      returned_element <- self$get()[1]
      private$set(self$get()[-1])
      returned_element
    },
    #' @description
    #' Removes all elements from this queue.
    #'
    #' @return invisibly self
    empty = function() {
      private$set(c())
      invisible(self)
    },
    #' @description
    #' Returns the number of elements in this queue.
    #'
    #' @return the number of elements in this queue
    size = function() {
      length(self$get())
    },
    #' @description
    #' Returns an array of elements in this queue. The order of elements is chronological:
    #' the first elements in the returned array is the oldest element added to this queue.
    #'
    #' @param reversed (`logical`)\cr
    #' if TRUE then returns the First-In-First-Out order; otherwise returns the Last-In-First-Out order.
    #' Default: FALSE
    #'
    #' @return the array of elements in this queue
    get = function(reversed = FALSE) {
      if (reversed) {
        rev(private$array)
      } else {
        private$array
      }
    },
    #' @description
    #' Removes the eldest occurrence of elements from this queue. Relies on implicit
    #' conversions of R types to compare a removed element with elements in this queue.
    #'
    #' @param elements the elements to remove from this queue
    #'
    #' @return invisibly self
    remove = function(elements) {
      lapply(elements, function(element) {
        index_to_remove <- which(vapply(self$get(), identical, logical(1), element))[1]
        if (!is.na(index_to_remove)) private$set(self$get()[-index_to_remove])
      })
      invisible(self)
    },
    #' @description
    #' Prints this queue.
    #'
    #' @param ... the additional arguments to this method, ignored
    #'
    #' @return invisibly self
    print = function(...) {
      cat(
        sprintf(
          "%s\nSize: %i\nElements:\n%s\n",
          strsplit(format(self), "\n")[[1]][1],
          self$size(),
          paste(self$get(), collapse = " ")
        )
      )
      invisible(self)
    }
  ),
  private = list(
    array = c(),
    set = function(x, append = FALSE) {
      if (isTRUE(append)) {
        private$array <- c(private$array, x)
      } else {
        private$array <- x
      }
    }
  ),
  lock_class = TRUE
)


# ReactiveQueue ====
#'
#' @title  Reactive extension of \code{Queue} class
#'
#' @description
#' Implements the queue abstract data type. The last element added to this queue is
#' the last one to be returned from it.
#' @keywords internal
#'
ReactiveQueue <- R6::R6Class( # nolint
  classname = "ReactiveQueue",
  inherit = Queue,
  public = list(
    #' @description
    #' Initializes `ReactiveQueue` by setting empty `reactiveVal`
    initialize = function() {
      private$array <- reactiveVal()
    },

    #' @description
    #' Get queue
    #'
    #' @param reversed (`logical(1)`)\cr
    #'   whether order of elements in the queue should be reversed.
    #'   `FALSE` by default
    #' @return values stored in the queue
    get = function(reversed = FALSE) {
      res <- if (private$if_reactive_context()) {
        private$array()
      } else {
        isolate(private$array())
      }
      if (reversed) {
        rev(res)
      } else {
        res
      }
    }
  ),
  private = list(
    array = NULL, # because it hold reactiveVal
    set = function(x, append = FALSE) {
      value_to_set <- if (isTRUE(append)) {
        c(self$get(), x)
      } else {
        x
      }
      if (private$if_reactive_context()) {
        isolate(private$array(value_to_set))
      } else {
        private$array(value_to_set)
      }
    },
    if_reactive_context = function() {
      !is.null(shiny:::.getReactiveEnvironment()$.currentContext)
    }
  ),
  lock_class = TRUE
)
