## CodeClass ====
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @title Code Class
#'
#'
#' @examples
#' cc <- teal:::CodeClass$new()
#' cc$set_code(c("foo <- function() {1}", "foo2 <- function() {2}"))
#' cc$get_code()
#' cc$get_code(deparse = FALSE)
#'
#' cc$set_code(c("ADSL <- radsl()", "ADSL$var <- 1"), "ADSL")
#' cc$set_code("ADSL$a <- foo()", "ADSL")
#'
#' # dependent dataset
#' cc$set_code("ADAE <- radae(ADSL = ADSL)", "ADAE", deps = "ADSL")
#'
#' cc$set_code("baz <- function() {2}")
#' cc$set_code("ADAE$a <- baz()", "ADAE")
#'
#' cc$get_code()
#' cc$get_code("ADSL")
#' cc$get_code("ADAE")
#'
#' #########################################
#' #########################################
#' #########################################
#'
#' x1 <- teal:::CodeClass$new()
#' x1$set_code("ADSL <- radsl(cached = TRUE)", "ADSL")
#' x1$get_code()
#'
#' x2 <- teal:::CodeClass$new()
#' x2$set_code("ADAE <- radae(ADSL = ADSL)", "ADAE", "ADSL")
#' x2$get_code()
#'
#' x <- teal:::CodeClass$new()
#' x$append(x1)
#' x$append(x2)
#'
#' x$get_code()
#' x$get_code("ADSL")
#' x$get_code("ADAE")
#' x$get_code(c("ADSL", "ADAE"))
#'
#' x3 <- teal:::CodeClass$new()
#' x3$set_code("ADRS <- radae(cached = TRUE)", "ADRS")
#' x3$get_code()
#'
#' x$append(x3)
#' x$get_code("ADRS")
#'
#' # mutation simulation
#' x$set_code("ADRS$x <- foo(ADSL$x)", "ADRS", deps = "ADSL")
#' x$get_code("ADRS")
CodeClass <- R6::R6Class( # nolint
  "CodeClass",
  ## __Public Methods ====
  public = list(
    #' @description
    #' CodeClass constructor
    #' @param code (`character`) vector of code text to be set
    #' @param dataname optional, (`character`) vector of datanames to assign code to. If empty then the code
    #' is considered to be "global"
    #' @param deps optional, (`character`) vector of datanames that given code depends on
    #' @return object of class CodeClass
    initialize = function(code = character(0), dataname = character(0), deps = character(0)) {
      if (length(code) > 0) {
        self$set_code(code, dataname, deps)
      }
      logger::log_trace("CodeClass initialized.")
      return(invisible(self))
    },
    #' @description
    #' Append `CodeClass` object to a given `CodeClass` object
    #' @param x (`CodeClass`) object to be appended
    #' @return changed `CodeClass` object
    append = function(x) {
      stopifnot(is(x, "CodeClass"))
      if (length(x$code) > 0) {
        for (code_i in x$code) {
          private$set_code_single(code_i)
        }
        logger::log_trace("CodeClass$append CodeClass appended.")
      }

      return(invisible(self))
    },
    #' @description
    #' Set code in form of character
    #' @param code (`character`) vector of code text to be set
    #' @param dataname optional, (`character`) vector of datanames to assign code to. If empty then the code
    #' is considered to be "global"
    #' @param deps optional, (`character`) vector of datanames that given code depends on
    #'
    #' @return changed `CodeClass` object
    set_code = function(code, dataname = character(0), deps = character(0)) {
      checkmate::assert_character(code, min.len = 1, any.missing = FALSE)
      checkmate::assert_character(dataname, any.missing = FALSE)
      stopifnot(!(dataname %in% deps))

      code <- pretty_code_string(code)

      for (code_single in code) {
        private$set_code_single(code_single, dataname, deps)
      }
      logger::log_trace("CodeClass$set_code code set.")
      return(invisible(self))
    },
    #' @description
    #' Get the code for a given data names
    #' @param dataname optional, (`character`) vector of datanames for which the code is extracted.
    #' If `NULL` then get the code for all data names
    #' @param deparse optional, (`logical`) whether to return the deparsed form of a call
    #' @return `character` or `list` of calls
    get_code = function(dataname = NULL, deparse = TRUE) {
      checkmate::assert_character(dataname, min.len = 1, null.ok = TRUE, any.missing = FALSE)
      checkmate::assert_flag(deparse)
      if (is.null(dataname)) {
        private$get_code_all(deparse = deparse)
      } else {
        private$get_code_dataname(dataname = dataname, deparse = deparse)
      }
    },
    #' @description
    #' Evaluates internal code within given environment
    #' @param envir (`environment`) environment in which code will be evaluated
    #' @return invisibly `NULL`
    eval = function(envir = new.env(parent = parent.env(.GlobalEnv))) {
      for (x in self$get_code(deparse = FALSE)) {
        out <- tryCatch(
          base::eval(x, envir = envir),
          error = function(e) e
        )

        if (is(out, "error")) {
          error_msg <- sprintf("%s\n\nEvaluation of the code failed:\n %s", deparse1(x, collapse = "\n"), conditionMessage(out))

          rlang::with_options(
            stop(error_msg, call. = FALSE),
            warning.length = max(min(8170, nchar(error_msg) + 30), 100)
          )
        }
      }
      logger::log_trace("CodeClass$eval successfuly evaluated the code.")
      return(invisible(NULL))
    }
  ),
  private = list(
    ## __Private Fields ====
    .code = list(),
    deps = list(),
    ## __Private Methods ====
    set_code_single = function(code,
                               dataname = attr(code, "dataname"),
                               deps =  attr(code, "deps"),
                               id = attr(code, "id")) {
      if (is.null(dataname)) dataname <- character(0)
      if (is.null(deps)) deps <- character(0)
      if (is.null(id)) id <- digest::digest(c(private$.code, code))
      # Line shouldn't be added when it contains the same code and the same dataname
      # as a line already present in an object of CodeClass
      if (!id %in% unlist(lapply(private$.code, "attr", "id")) ||
        all_true(dataname, function(x) !x %in% unlist(lapply(private$.code, "attr", "dataname")))) {
        attr(code, "dataname") <- dataname
        attr(code, "deps") <- deps
        attr(code, "id") <- id

        private$.code <- base::append(private$.code, list(code))
      }
      return(invisible(NULL))
    },
    get_code_all = function(deparse) {
      private$get_code_idx(idx = seq_along(private$.code), deparse = deparse)
    },
    get_code_dataname = function(dataname, deparse) {
      # the lines of code we need for the dataname
      res <- integer(0)
      # the set of datanames we want code for code for intially just dataname
      datanames <- dataname

      # loop backwards along code
      for (idx in rev(seq_along(private$.code))) {
        code_entry <- private$.code[[idx]]

        # line of code is one we want if it is not empty and
        # has any dataname attribute in the vector datanames or dataname starts with * or is global code and
        # already have some lines of code selected
        if (
          (
            any(datanames %in% attr(code_entry, "dataname")) ||
            any(grepl("^[*]", attr(code_entry, "dataname"))) ||
            (length(res) > 0 && length(attr(code_entry, "dataname")) == 0)
          ) &&
          length(code_entry) > 0
        ) {

          # append to index of code we want
          res <- c(idx, res)

          # and update datasets we want for preceding code with additional datanames and deps
          datanames <- unique(c(datanames, attr(code_entry, "dataname"), attr(code_entry, "deps")))
        }
      }
      private$get_code_idx(idx = res, deparse = deparse)
    },
    get_code_idx = function(idx, deparse) {
      if (isFALSE(deparse)) {
        return(Filter(
          Negate(is.null),
          unname(unlist(lapply(
            private$.code[idx],
            function(x) sapply(x, function(i) text_to_call(i), simplify = FALSE)
          )))
        ))
      } else {
        return(paste0(unlist(private$.code[idx]), collapse = "\n"))
      }
    }
  ),

  ## __Active Fields ====
  active = list(
    #' @field code (`list`) Derive the code of the dataset.
    code = function() {
      private$.code
    }
  )
)


## Functions ====

# Convert named list to `CodeClass` utilizing both `TealDatasetConnector` and `TealDataset`
list_to_code_class <- function(x) {
  stopifnot(is_fully_named_list(x))

  res <- CodeClass$new()

  if (length(x) > 0) {
    for (var_idx in seq_along(x)) {
      var_value <- x[[var_idx]]
      var_name <- names(x)[[var_idx]]
      if (is(var_value, "TealDatasetConnector") || is(var_value, "TealDataset")) {
        res$append(var_value$get_code_class())
        if (var_name != var_value$get_dataname()) {
          res$set_code(
            deparse1(call("<-", as.name(var_name), as.name(var_value$get_dataname())), collapse = "\n"),
            dataname = var_value$get_dataname()
          )
        }
      } else {
        var_code <- deparse1(call("<-", as.name(var_name), var_value), collapse = "\n")
        res$set_code(var_code, var_name)
      }
    }
  }
  return(res)
}

#' Create call from string
#'
#' @param x (`character`) string containing the code.
#'
#' @return (`call`) object.
text_to_call <- function(x) {
  parsed <- parse(text = x, keep.source = FALSE)
  if (length(parsed) == 0) {
    return(NULL)
  } else {
    return(as.list(as.call(parsed))[[1]])
  }
}

#' Format a vector of code into a string
#'
#' @param code_vector (`character`) vector containing lines of
#'   code to format into a string.
#'
#' @return (`character`) string containing the formatted code.
pretty_code_string <- function(code_vector) {
  # in order to remove bad formatting: text -> code -> text
  unlist(lapply(
    code_vector,
    function(code_single) {
      if (length(parse(text = code_single, keep.source = FALSE)) == 0) {
        # if string code cannot be passed into expression (e.g. code comment) then pass on the string
        code_single
      } else {
        vapply(
          as.list(as.call(parse(text = code_single, keep.source = FALSE))),
          deparse1,
          character(1),
          collapse = "\n"
        )
      }
    }
  ))
}
