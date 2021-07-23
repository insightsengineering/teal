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
    #' @param code (\code{character}) vector of code text to be set
    #' @param dataname optional, (\code{character}) vector of datanames to assign code to. If empty then the code
    #' is considered to be "global"
    #' @param deps optional, (\code{character}) vector of datanames that given code depends on
    #' @return object of class CodeClass
    initialize = function(code = character(0), dataname = character(0), deps = character(0)) {
      if (length(code) > 0)
        self$set_code(code, dataname, deps)
      return(invisible(self))
    },
    #' @description
    #' Append \code{CodeClass} object to a given \code{CodeClass} object
    #' @param x (\code{CodeClass}) object to be appended
    #' @return changed \code{CodeClass} object
    append = function(x) {
      stopifnot(is(x, "CodeClass"))
      if (is_empty(x$code)) {
        return(invisible(self))
      } else {
        prior <- self$code
        for (code_i in x$code) {
          private$set_code_single(code_i)
        }
        if (identical(prior, self$code)) {
          warning("Code is not appended because it is identical to previously added code.", call. =  FALSE)
        }
        return(invisible(self))
      }
    },
    #' @description
    #' Set code in form of character
    #' @param code (\code{character}) vector of code text to be set
    #' @param dataname optional, (\code{character}) vector of datanames to assign code to. If empty then the code
    #' is considered to be "global"
    #' @param deps optional, (\code{character}) vector of datanames that given code depends on
    #'
    #' @return changed \code{CodeClass} object
    set_code = function(code, dataname = character(0), deps = character(0)) {
      stopifnot(
        is_character_vector(code),
        is_character_vector(dataname, min_length = 0),
        !(dataname %in% deps)
      )

      code <- pretty_code_string(code)

      for (code_single in code) {
        private$set_code_single(code_single, dataname, deps)
      }

      return(invisible(self))
    },
    #' @description
    #' Get the code for a given data names
    #' @param dataname optional, (\code{character}) vector of datanames for which the code is extracted.
    #' If \code{NULL} then get the code for all data names
    #' @param deparse optional, (\code{logical}) whether to return the deparsed form of a call
    #' @return \code{character} or \code{list} of calls
    get_code = function(dataname = NULL, deparse = TRUE) {
      stopifnot(is.null(dataname) || is_character_vector(dataname))
      stopifnot(is_logical_single(deparse))
      if (is.null(dataname)) {
        private$get_code_all(deparse = deparse)
      } else {
        private$get_code_dataname(dataname = dataname, deparse = deparse)
      }
    },
    #' @description
    #' Evaluates internal code within given environment
    #' @param envir (\code{environment}) environment in which code will be evaluated
    #' @return invisibly \code{NULL}
    eval = function(envir = new.env(parent = parent.env(.GlobalEnv))) {
     for (x in self$get_code(deparse = FALSE)) {
       out <- tryCatch(
         base::eval(x, envir = envir),
         error = function(e) e
       )

       if (is(out, "error")) {
         error_msg <- sprintf("%s\n\nEvaluation of the code failed:\n %s", pdeparse(x), conditionMessage(out))

         rlang::with_options(
           stop(error_msg, call. = FALSE),
           warning.length = max(min(8170, nchar(error_msg) + 30), 100)
         )
       }
     }
     return(invisible(NULL))
    }
  ),

  private = list(
    ## __Private Fields ====
    .code = list(),
    deps = list(),
    ## __Private Methods ====
    set_code_single = function(code,
                               dataname = if_null(attr(code, "dataname"), character(0)),
                               deps = if_null(attr(code, "deps"), character(0)),
                               id = if_null(attr(code, "id"), digest::digest(c(private$.code, code)))) {
      # Line shouldn't be added when it contains the same code and the same dataname
      # as a line already present in an object of CodeClass
      if (!id %in% ulapply(private$.code, "attr", "id") ||
          all_true(dataname, function(x) !x %in% ulapply(private$.code, "attr", "dataname"))) {
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
      #the lines of code we need for the dataname
      res <- integer(0)
      # the set of datanames we want code for code for intially just dataname
      datanames <- dataname

      # loop backwards along code
      for (idx in rev(seq_along(private$.code))) {

        code_entry <- private$.code[[idx]]

        # line of code is one we want if it is not empty and
        # has any dataname attribute in the vector datanames or dataname starts with * or is global code and
        # already have some lines of code selected
        if ((
          any(datanames %in% attr(code_entry, "dataname")) ||
          any(grepl("^[*]", attr(code_entry, "dataname"))) ||
          (!is_empty(res) && is_empty(attr(code_entry, "dataname")))) &&
          !is_empty(code_entry)) {

          #append to index of code we want
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
          unname(ulapply(
            private$.code[idx],
            function(x) sapply(x, function(i) text_to_call(i), simplify = FALSE)
          ))
        ))
      } else {
        return(paste0(unlist(private$.code[idx]), collapse = "\n"))
      }
    }
  ),

  ## __Active Fields ====
  active = list(
    #' @field code (\code{list}) Derive the code of the dataset.
    code = function() {
      private$.code
    }
  )
)


## Functions ====

# Convert named list to \code{CodeClass} utilizing both \code{DatasetConnector} and \code{Dataset}
list_to_code_class <- function(x) {
  stopifnot(is_fully_named_list(x))

  res <- CodeClass$new()

  if (!is_empty(x)) {
    for (var_idx in seq_along(x)) {
      var_value <- x[[var_idx]]
      var_name <- names(x)[[var_idx]]
      if (is(var_value, "DatasetConnector") || is(var_value, "Dataset")) {
        res$append(var_value$get_code_class())
        res$set_code(pdeparse(call("<-", as.name(var_name), as.name(var_value$get_dataname()))))
      } else {
        var_code <- pdeparse(call("<-", as.name(var_name), var_value))
        res$set_code(var_code, var_name)
      }
    }
  }
  return(res)
}

#' Create call from string
#'
#' @param x (\code{character}) string containing the code.
#'
#' @return (\code{call}) object.
text_to_call <- function(x) {
  parsed <- parse(text = x)
  if (is_empty(parsed)) {
    return(NULL)
  } else {
    return(as.list(as.call(parsed))[[1]])
  }
}

#' Format a vector of code into a string
#'
#' @param code_vector (\code{character}) vector containing lines of
#'   code to format into a string.
#'
#' @return (\code{character}) string containing the formatted code.
pretty_code_string <- function(code_vector) {
  # in order to remove bad formatting: text -> code -> text
  ulapply(
    code_vector,
    function(code_single) {
      if (is_empty(parse(text = code_single))) {
        # if string code cannot be passed into expression (e.g. code comment) then pass on the string
        code_single
      } else {
        vapply(as.list(as.call(parse(text = code_single))), pdeparse, character(1))
      }
    }
  )
}
