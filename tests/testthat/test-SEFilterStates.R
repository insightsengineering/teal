get_test_data <- function() {
  library(SummarizedExperiment)
  nrows <- 200
  ncols <- 6
  counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
  row_ranges <- GRanges(
    rep(c("chr1", "chr2"), c(50, 150)),
    IRanges(floor(runif(200, 1e5, 1e6)), width = 100),
    strand = sample(c("+", "-"), 200, TRUE),
    feature_id = sprintf("ID%03d", 1:200))
  cdata <- DataFrame(
    Treatment = rep(c("ChIP", "Input"), 3),
    row.names = LETTERS[1:6])

  obj <- SummarizedExperiment(
    assays = list(counts = counts),
    rowRanges = row_ranges,
    colData = cdata
  )
}

testthat::test_that("The constructor does not throw", {
  testthat::expect_error(SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  ), NA)
})

testthat::test_that("The constructor initializes two queues", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_null(filter_states$queue_get(1))
  testthat::expect_null(filter_states$queue_get(2))
})

testthat::test_that("set_bookmark_state throws error when input is missing", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_error(filter_states$set_bookmark_state())
})

testthat::test_that("set_bookmark_state throws error when no data argument is inputted", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_error(filter_states$set_bookmark_state(state = list(subset = NULL, select = NULL)))
})

testthat::test_that("set_bookmark_state throws error when data argument is not of class SummarizedExperiment", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  testthat::expect_error(filter_states$set_bookmark_state(data = data, state = list(subset = NULL, select = NULL)))
})

testthat::test_that("set_bookmark_state throws error when state argument contains extra elements", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_error(filter_states$set_bookmark_state(data = data, state = list(A = "test")))
})

testthat::test_that("set_bookmark_state throws error when state argument is not a list", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_error(filter_states$set_bookmark_state(data = data, state = c(subset = "A", select = "B")))
})

## acceptable inputs to set_bookmark_state
testthat::test_that("set_bookmark_state returns NULL when state argument contains subset and select set as NULL", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )

  obj <- get_test_data()

  testthat::expect_null(filter_states$set_bookmark_state(data = obj, state = list(subset = NULL, select = NULL)))
})

testthat::test_that("set_bookmark_state returns NULL when state argument is an empty list", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  obj <- get_test_data()
  testthat::expect_null(filter_states$set_bookmark_state(data = obj, state = list()))
})

testthat::test_that("clone method returns object with the same state", {
  se <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_equal(se, se$clone())
})

testthat::test_that("get_call method returns NULL", {
  se <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_null(se$get_call())
})

testthat::test_that("get_fun method returns subset", {
  se <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_equal(se$get_fun(), "subset")
})

testthat::test_that("get_call returns `output_dataname <- input_dataname` when no state is set", {
  obj <- get_test_data()
  test <- obj

  sefs <- teal:::SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  testthat::expect_equal(
    isolate(sefs$get_call()),
    quote(test_filtered <- test)
  )

  # now testing for equality of object
  eval(isolate(sefs$get_call()))
  testthat::expect_equal(test_filtered, test)
})

testthat::test_that("SEFilterStates$set_bookmark_state sets state with only select", {
  obj <- get_test_data()
  test <- obj

  sefs <- teal:::SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  fs <- list(
    select = list(Treatment = "ChIP")
  )

  sefs$set_bookmark_state(state = fs, data = obj)

  eval(isolate(sefs$get_call()))
  testthat::expect_equal(test_filtered, subset(
    test,
    select = Treatment == "ChIP"
  ))
})

testthat::test_that("SEFilterStates$set_bookmark_state sets state with only subset", {
  obj <- get_test_data()
  test <- obj

  sefs <- teal:::SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  fs <- list(
    subset = list(feature_id = c("ID001", "ID002"))
  )

  sefs$set_bookmark_state(state = fs, data = obj)

  eval(isolate(sefs$get_call()))
  testthat::expect_equal(test_filtered, subset(
    test,
    subset = feature_id %in% c("ID001", "ID002")
  ))
})

testthat::test_that("SEFilterStates$set_bookmark_state sets state with neither subset nor select", {
  obj <- get_test_data()
  test <- obj

  sefs <- teal:::SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  sefs$set_bookmark_state(data = obj)

  eval(isolate(sefs$get_call()))
  testthat::expect_equal(test_filtered, test)
})

testthat::test_that("SEFilterStates$set_bookmark_state sets filters in ReactiveQueue specified by the named list", {
  obj <- get_test_data()
  test <- obj
  sefs <- teal:::SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  fs <- list(
    select = list(Treatment = "ChIP"),
    subset = list(feature_id = c("ID001", "ID002"))
  )

  sefs$set_bookmark_state(state = fs, data = obj)

  eval(isolate(sefs$get_call()))
  testthat::expect_equal(test_filtered, subset(
    test,
    subset = feature_id %in% c("ID001", "ID002"),
    select = Treatment == "ChIP"
  ))
})
