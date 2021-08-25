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

test_that("'input_dataname' must be specified with 'extract_type'", {
  testthat::expect_error(
    FilterState$new(
      ADSL$SEX,
      varname = "SEX",
      input_dataname = as.name("ADSL"),
      extract_type = character(0)
    )
  )
})

test_that("'extract_type' must be specified with 'input_dataname'", {
  testthat::expect_error(
    FilterState$new(
      ADSL$SEX,
      varname = "SEX",
      input_dataname = NULL,
      extract_type = "matrix"
    )
  )
})

testthat::test_that("get_dataname returns a string when input_dataname is name", {
  filter_state <- FilterState$new(7, varname = "7", input_dataname = quote(test), extract_type = "list")
  testthat::expect_equal(filter_state$get_dataname(deparse = FALSE), quote(test))
  testthat::expect_equal(filter_state$get_dataname(deparse = TRUE), "test")
})

testthat::test_that("get_dataname(deparse = TRUE) returns a string when input_dataname is call", {
  filter_state <- FilterState$new(7, varname = "7", input_dataname = call("test_function"), extract_type = "list")
  testthat::expect_equal(filter_state$get_dataname(deparse = TRUE), "test_function()")
})

testthat::test_that("get_dataname(deparse = FALSE) returns a call when input_dataname is call", {
  filter_state <- FilterState$new(7, varname = "7", input_dataname = call("test_function"), extract_type = "list")
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

testthat::test_that("label_keep_na_count return the string with an appended element", {
  testthat::expect_equal(label_keep_na_count(7), "Keep NA (7)")
})
