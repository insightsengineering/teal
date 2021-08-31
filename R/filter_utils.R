#' Get call to subset and select array
#'
#' Get call to subset and select array
#' @param dataname (`character(1)` or `name`)\cr
#' @param row (`character(1)` or `call`)\cr
#'   optional, name of the `row` or condition
#' @param column (`character(1)` or `call`)\cr
#'   optional, name of the `column` or condition
#' @param aisle (`character(1)` or `call`)\cr
#'   optional, name of the `row` or condition
#' @return `[` call with all conditions included
#' @examples
#' teal:::call_extract_array(
#'   dataname = "my_array",
#'   row = teal:::call_condition_choice("my_array$SEX", "M"),
#'   column = call("c", "SEX", "AGE"),
#'   aisle = "RNAseq_rnaaccess"
#' )
#' teal:::call_extract_array(
#'   "mae_object",
#'   column = teal:::call_condition_choice("SEX", "M")
#' )
#' @return specific \code{\link[base]{Extract}} `call`
call_extract_array <- function(dataname = ".", row = NULL, column = NULL, aisle = NULL) {
  stopifnot(is.character(dataname) || is.name(dataname))
  stopifnot(is.null(row) || is.call(row) || is.character(row) || is.name(row))
  stopifnot(is.null(column) || is.call(column) || is.character(column) || is.name(column))
  stopifnot(is.null(aisle) || is.call(aisle) || is.character(aisle) || is.name(aisle))

  if (missing(dataname)) {
    dataname <- as.name(".")
  } else if (is.name(dataname)) {
    dataname <-  pdeparse(dataname)
  }

  row <- if (is.null(row)) {
    ""
  } else {
    pdeparse(row, width.cutoff = 500L)
  }
  column <- if (is.null(column)) {
    ""
  } else {
    pdeparse(column, width.cutoff = 500L)
  }
  aisle <- if (is.null(aisle)) {
    ""
  } else {
    pdeparse(aisle, width.cutoff = 500L)
  }

  parse(
    text = sprintf("%s[%s, %s, %s]", dataname, row, column, aisle)
  )[[1]]
}

#' Get call to subset and select matrix
#'
#' Get call to subset and select matrix
#' @param dataname (`character(1)` or `name`)\cr
#' @param row (`character(1)` or `call`)\cr
#'   optional, name of the `row` or condition
#' @param column (`character(1)` or `call`)\cr
#'   optional, name of the `column` or condition
#' @return `[` call with all conditions included
#' @examples
#' teal:::call_extract_matrix(
#'   dataname = "my_array",
#'   row = teal:::call_condition_choice("my_array$SEX", "M"),
#'   column = call("c", "SEX", "AGE")
#' )
#' teal:::call_extract_matrix(
#'   "mae_object",
#'   column = teal:::call_condition_choice("SEX", "M")
#' )
#' @return specific \code{\link[base]{Extract}} `call`
call_extract_matrix <- function(dataname = ".", row = NULL, column = NULL) {
  stopifnot(is.character(dataname) || is.name(dataname))
  stopifnot(is.null(row) || is.call(row) || is.character(row) || is.name(row))
  stopifnot(is.null(column) || is.call(column) || is.character(column) || is.name(column))

  if (missing(dataname)) {
    dataname <- as.name(".")
  } else if (is.name(dataname)) {
    dataname <-  pdeparse(dataname)
  }

  row <- if (is.null(row)) {
    ""
  } else {
    pdeparse(row, width.cutoff = 500L)
  }
  column <- if (is.null(column)) {
    ""
  } else {
    pdeparse(column, width.cutoff = 500L)
  }

  parse(
    text = sprintf("%s[%s, %s]", dataname, row, column)
  )[[1]]
}


#' Compose extract call with `$` operator
#'
#' Compose extract call with `$` operator
#'
#' @param dataname (`character(1)` or `name`)\cr
#'   name of the object
#'
#' @param varname (`character(1)` or `name`)\cr
#'   name of the slot in data
#'
#' @param dollar (`logical(1)`)\cr
#'   whether returned call should use `$` or `[[` operator
#'
#' @return `$` call with all conditions included
#' @examples
#' teal:::call_extract_list("ADSL", "SEX")
#' teal:::call_extract_list("ADSL", "named element")
#' teal:::call_extract_list(as.name("ADSL"), as.name("AGE"))
#' teal:::call_extract_list(as.name("weird name"), as.name("AGE"))
#' teal:::call_extract_list(as.name("ADSL"), "AGE", dollar = FALSE)
call_extract_list <- function(dataname, varname, dollar = TRUE) {
  stopifnot(utils.nest::is_character_single(dataname) || is.name(dataname))
  stopifnot(utils.nest::is_character_single(varname) || is.name(varname))
  stopifnot(utils.nest::is_logical_single(dollar))
  if (is.character(dataname)) {
    dataname <- as.name(dataname)
  }

  if (dollar) {
    call("$", dataname, varname)
  } else {
    call("[[", dataname, varname)
  }
}

#' Combine calls by operator
#'
#' Combine list of calls by specific operator
#'
#' @param operator (`character(1)` or `name`)\cr
#'   name/symbol of the operator.
#'
#' @param calls (`list` of calls)\cr
#'   list containing calls to be combined by `operator`
#'
#' @return call
#' @examples
#' teal:::calls_combine_by(
#'   "&",
#'   calls = list(
#'     teal:::call_condition_choice("SEX", "F"),
#'     teal:::call_condition_range("AGE", c(20, 50)),
#'     teal:::call_condition_choice("ARM", "ARM: A"),
#'     TRUE
#'   )
#' )
#' @return a combined `call`
calls_combine_by <- function(operator, calls) {
  stopifnot(utils.nest::is_character_single(operator))
  stopifnot(
    all(
      vapply(
        X = calls,
        FUN.VALUE = logical(1),
        function(x) is.language(x) || is.logical(x)
      )
    )
  )

  Reduce(
    x = calls,
    f = function(x, y) call(operator, x, y)
  )
}

#' Choices condition call
#'
#' Compose choices condition call from inputs.
#'
#' @param varname (`name` or `character(1)`)\cr
#'   name of the variable
#'
#' @param choices (`vector`)\cr
#'   `varname` values to match the `==` (single value) or
#'   `%in%` (vector) condition.
#'
#' @examples
#' teal:::call_condition_choice(as.name("SEX"), choices = "F")
#' teal:::call_condition_choice("SEX", choices = c("F", "M"))
#' teal:::call_condition_choice("SEX", choices = c(1, 2))
#' @return `call`
call_condition_choice <- function(varname, choices) {
  stopifnot(utils.nest::is_character_single("varname") || is.name(varname))
  if (is.character(varname)) {
    varname <- as.name(varname)
  }

  if (length(choices) == 1) {
    call("==", varname, choices)
  } else {
    c_call <- do.call(
      "call",
      append(list("c"), choices)
    )
    # c_call needed because it needs to be vector call
    # instead of vector. SummarizedExperiment.subset
    # handles only vector calls
    call("%in%", varname, c_call)
  }
}

#' `numeric` range condition call
#'
#' Compose `numeric` range condition call from inputs
#'
#' @param varname (`name` or `character(1)`)\cr
#'   name of the variable
#'
#' @param range (`numeric(2)`)\cr
#'   range of the variable
#'
#' @return call
#' @examples
#' teal:::call_condition_range("AGE", range = c(1, 2))
#' teal:::call_condition_range(as.name("AGE"), range = c(-1.2, 2.1))
#' teal:::call_condition_range(
#'   teal:::call_extract_list("ADSL", "AGE"), range = c(-1.2, 2.1)
#' )
#' @return a `call`
call_condition_range <- function(varname, range) {
  stopifnot(utils.nest::is_character_single(varname) || is.name(varname) || is.call(varname))
  stopifnot(is.numeric(range) && length(range) == 2)

  if (is.character(varname)) {
    varname <- as.name(varname)
  }
  call(
    "&",
    call(">=", varname, range[1]),
    call("<=", varname, range[2])
  )
}

#' `logical` variable condition call
#'
#' Compose `logical` variable condition call from inputs
#'
#' @param varname (`name` or `character(1)`)\cr
#'   name of the variable
#'
#' @param choice (`logical(1)`)\cr
#'   chosen value
#'
#' @return call
#' @examples
#' teal:::call_condition_logical("event", choice = TRUE)
#' teal:::call_condition_logical("event", choice = FALSE)
#' @return a `call`
call_condition_logical <- function(varname, choice) {
  stopifnot(utils.nest::is_character_single(varname) || is.name(varname) || is.call(varname))
  stopifnot(utils.nest::is_logical_single(choice))

  if (is.character(varname)) {
    varname <- as.name(varname)
  }

  if (choice) {
    varname
  } else if (!choice) {
    call("!", varname)
  } else {
    stop(
      "Unknown filter state", toString(choice),
      " for logical var ", as.character(varname)
    )
  }
}


#' `POSIXct` range condition call
#'
#' Compose `POSIXct` range condition call from inputs
#'
#' @param varname (`name` or `character(1)`)\cr
#'   name of the variable
#'
#' @param range (`POSIXct`)\cr
#'   range of the variable
#'
#' @param timezone (`character(1)`)\cr
#'   specifies the time zone to be used for the conversion.
#'   By default `Sys.timezone()` is used.
#'
#' @examples
#' teal:::call_condition_range_posixct(
#'   varname = as.name("datetime"),
#'   range = c(Sys.time(), Sys.time() + 1),
#'   timezone = "UTC"
#' )
#' @return a `call`
call_condition_range_posixct <- function(varname, range, timezone = Sys.timezone()) {
  stopifnot(utils.nest::is_character_single(varname) || is.name(varname) || is.call(varname))
  stopifnot(is(range, "POSIXct") && length(range) == 2)
  stopifnot(utils.nest::is_character_single(timezone))

  if (is.character(varname)) {
    varname <- as.name(varname)
  }

  range[1] <- trunc(range[1], units = c("secs"))
  range[2] <- trunc(range[2] + 1, units = c("secs"))

  range <- format(
    range,
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )

  call(
    "&",
    call(">=", varname, call("as.POSIXct", range[1], tz = timezone)),
    call("<", varname, call("as.POSIXct", range[2], tz = timezone))
  )
}

#' `Date` range condition call
#'
#' Compose `Date` range condition call from inputs
#'
#' @param varname (`name` or `character(1)`)\cr
#'   name of the variable
#'
#' @param range (`Date`)\cr
#'   range of the variable
#'
#' @examples
#' teal:::call_condition_range_date(
#'   as.name("date"),
#'   range = c(Sys.Date(), Sys.Date() + 1)
#' )
#' @return `call`
call_condition_range_date <- function(varname, range) {
  stopifnot(utils.nest::is_character_single(varname) || is.name(varname) || is.call(varname))
  stopifnot(is(range, "Date") && length(range) == 2)


  if (is.character(varname)) {
    varname <- as.name(varname)
  }

  call(
    "&",
    call(">=", varname, call("as.Date", as.character(range[1]))),
    call("<=", varname, call("as.Date", as.character(range[2])))
  )

}




#' Set filter state in `FilterState` object
#'
#' Sets values of the selection in `FilterState` object
#' @param x (`list`,`vector`, `default_filter`)\cr
#'  values of the variable used in filter. Depending on the `FilterState` type
#'  list should contain:
#'  \itemize{
#'  \item{`selected`}{ defines initial selection}
#'  \item{`keep_na` (`logical`)}{ defines whether to keep or remove `NA` values}
#'  \item{`keep_inf` (`logical`)}{ defines whether to keep or remove `Inf` values}
#'  }
#'  If `vector` is provided then `keep_na` and `keep_inf` can be specified
#'  adding `NA` and `Inf` to the selection vector.
#'
#' @param filter_state (`FilterState`)\cr
#'
#' @examples
#' filter_state <- teal:::RangeFilterState$new(
#'   c(1:10, NA, Inf),
#'   varname = "x"
#' )
#'
#' teal:::set_filter_state(
#'   list(selected = c(1, 2), keep_na = FALSE, keep_inf = TRUE),
#'   filter_state
#' )
#' teal:::set_filter_state(c(1, 2, Inf), filter_state)
#' teal:::set_filter_state(default_filter(), filter_state)
set_filter_state <- function(x, filter_state) {
   UseMethod("set_filter_state")
}

#' @export
set_filter_state.default <- function(x, filter_state) {
  state <- list()
  if (any(is.na(x))) {
    state$keep_na <- TRUE
  }

  if (any(is.infinite(x))) {
    state$keep_inf <- TRUE
  }

  if (length(x[!(is.infinite(x) | is.na(x))]) > 0) {
    state$selected <- x[!(is.infinite(x) | is.na(x))]
  }

  filter_state$set_state(state)
}

#' @export
set_filter_state.default_filter <- function(x, filter_state) { # nolint
  invisible(NULL)
}

#' @export
set_filter_state.list <- function(x, filter_state) {
  filter_state$set_state(state = x)
}
