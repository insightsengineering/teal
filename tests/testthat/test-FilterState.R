testthat::test_that("The constructor accepts character, name or call as varname", {
  testthat::expect_error(FilterState$new(c(7), varname = "test"), NA)
  testthat::expect_error(FilterState$new(c(7), varname = quote(pi)), NA)
  testthat::expect_error(FilterState$new(c(7), varname = call("test")), NA)
})

testthat::test_that("The constructor requires a varname", {
  testthat::expect_error(FilterState$new(c(7)), regexp = "argument \"varname\" is missing")
})

testthat::test_that("The constructor accepts a string as varlabel", {
  testthat::expect_error(FilterState$new(c(7), varname = "test", varlabel = "test"), NA)
})

testthat::test_that("get_call returns NULL", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_null(filter_state$get_call())
})

test_that("'extract_type' must be specified with 'input_dataname'", {
  testthat::expect_error(
    FilterState$new(
      c("F", "M"),
      varname = "SEX",
      input_dataname = NULL,
      extract_type = "matrix"
    )
  )
})

testthat::test_that("get_dataname returns a string when input_dataname is NULL", {
  filter_state <- FilterState$new(7, varname = "7", input_dataname = NULL)
  testthat::expect_equal(filter_state$get_dataname(deparse = FALSE), quote(NULL))
  testthat::expect_equal(filter_state$get_dataname(deparse = TRUE), "NULL")
})

testthat::test_that("get_dataname returns a string when input_dataname is name", {
  filter_state <- FilterState$new(7, varname = "7", input_dataname = quote(test))
  testthat::expect_equal(filter_state$get_dataname(deparse = FALSE), quote(test))
  testthat::expect_equal(filter_state$get_dataname(deparse = TRUE), "test")
})

testthat::test_that("get_dataname(deparse = TRUE) returns a string when input_dataname is call", {
  filter_state <- FilterState$new(7, varname = "7", input_dataname = call("test_function"))
  testthat::expect_equal(filter_state$get_dataname(deparse = TRUE), "test_function()")
})

testthat::test_that("get_dataname(deparse = FALSE) returns a call when input_dataname is call", {
  filter_state <- FilterState$new(7, varname = "7", input_dataname = call("test_function"))
  testthat::expect_equal(filter_state$get_dataname(deparse = FALSE), call("test_function"))
})

testthat::test_that("get_varlabel returns a string passed to the constructor", {
  filter_state <- FilterState$new(7, varname = "7", varlabel = "test")
  testthat::expect_equal(filter_state$get_varlabel(), "test")
})

testthat::test_that("get_varname(deparse = FALSE) returns a name if varname passed to the constructor is a string", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_equal(filter_state$get_varname(deparse = FALSE), quote(`7`))
})

testthat::test_that("get_varname(deparse = TRUE) returns a string if varname passed to the constructor is a string", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_equal(filter_state$get_varname(deparse = TRUE), "7")
})

testthat::test_that("get_varname returns a call if call is passed to the constructor", {
  filter_state <- FilterState$new(7, varname = call("test"))
  testthat::expect_equal(filter_state$get_varname(deparse = FALSE), call("test"))
  testthat::expect_equal(filter_state$get_varname(deparse = TRUE), "test()")
})

testthat::test_that("get_selected returns NULL after initialization", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_null(isolate(filter_state$get_selected()))
})

testthat::test_that("set_selected sets value, get_selected returns the same", {
  filter_state <- FilterState$new(7L, varname = "7")
  filter_state$set_selected(7L)
  testthat::expect_identical(isolate(filter_state$get_selected()), 7L)
})

testthat::test_that("get_keep_na returns FALSE after initialization", {
  filter_state <- FilterState$new(7, varname = "7")
  testthat::expect_false(isolate(filter_state$get_keep_na()))
})

testthat::test_that("set_state sets selected and keep_na", {
  filter_state <- FilterState$new(c("a", NA_character_), varname = "var")
  state <- list(selected = "a", keep_na = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(
    state,
    isolate(
      list(
        selected = filter_state$get_selected(),
        keep_na = filter_state$get_keep_na()
      )
    )
  )
})

testthat::test_that("get_state returns a list identical to set_state input", {
  filter_state <- FilterState$new(c("a", NA_character_), varname = "var")
  state <- list(selected = "a", keep_na = TRUE)
  filter_state$set_state(state)
  testthat::expect_identical(isolate(filter_state$get_state()), state)
})

testthat::test_that("label_keep_na_count returns the string with an appended element", {
  testthat::expect_equal(label_keep_na_count(7), "Keep NA (7)")
})

testthat::test_that(
  "add_keep_na_call does not add anything by default",
  code = {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        }
      )
    )
    filter_state <- test_class$new(c(1, NA), varname = "test")
    testthat::expect_identical(
      isolate(filter_state$test_add_keep_na_call()),
      quote(TRUE)
    )
  }
)

testthat::test_that(
  "add_keep_na_call adds `is.na` when `keep_na` is set",
  code = {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        }
      )
    )
    filter_state <- test_class$new(c(1, NA), varname = "test")
    isolate(filter_state$set_keep_na(TRUE))

    testthat::expect_identical(
      isolate(filter_state$test_add_keep_na_call()),
      quote(is.na(test) | TRUE)
    )
  }
)

testthat::test_that(
  "Setting private$na_rm to TRUE adds `!is.na` before condition via add_keep_na_call",
  code = {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        }
      )
    )
    filter_state <- test_class$new(c(1, NA), varname = "test")
    filter_state$set_na_rm(TRUE)

    testthat::expect_identical(
      isolate(filter_state$test_add_keep_na_call()),
      quote(!is.na(test) & TRUE)
    )
  }
)

testthat::test_that(
  "Setting private$na_rm to TRUE doesn't add `!is.na` before condition via add_keep_na_call
  when variable has no NAs",
  code = {
    test_class <- R6::R6Class(
      classname = "TestClass",
      inherit = FilterState,
      public = list(
        test_add_keep_na_call = function() {
          private$add_keep_na_call(TRUE)
        }
      )
    )
    filter_state <- test_class$new(c(1), varname = "test")
    filter_state$set_na_rm(TRUE)

    testthat::expect_identical(
      isolate(filter_state$test_add_keep_na_call()),
      quote(TRUE)
    )
  }
)
