get_test_data <- function() {
  library(SummarizedExperiment)
  nrows <- 200
  ncols <- 6
  counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
  row_ranges <- GRanges(
    rep(c("chr1", "chr2"), c(50, 150)),
    IRanges(floor(runif(200, 1e5, 1e6)), width = 100),
    strand = sample(c("+", "-"), 200, TRUE),
    feature_id = sprintf("ID%03d", 1:200)
  )
  cdata <- DataFrame(
    Treatment = rep(c("ChIP", "Input"), 3),
    row.names = LETTERS[1:6]
  )

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

testthat::test_that("set_filter_state throws error when input is missing", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_error(filter_states$set_filter_state())
})

testthat::test_that("set_filter_state throws error when no data argument is inputted", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  testthat::expect_error(filter_states$set_filter_state(state = list(subset = NULL, select = NULL)))
})

testthat::test_that("set_filter_state throws error when data argument is not of class SummarizedExperiment", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  testthat::expect_error(
    filter_states$set_filter_state(data = data, state = list(subset = NULL, select = NULL))
  )
})

testthat::test_that("set_filter_state throws error when state argument contains extra elements", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_error(filter_states$set_filter_state(data = data, state = list(A = "test")))
})

testthat::test_that("set_filter_state throws error when state argument is not a list", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  data <- data.frame(a = "test")
  class(data) <- "SummarizedExperiment"
  testthat::expect_error(filter_states$set_filter_state(data = data, state = c(subset = "A", select = "B")))
})

## acceptable inputs to set_filter_state
testthat::test_that(
  "set_filter_state returns NULL when state argument contains subset and select set as NULL",
  code = {
    filter_states <- SEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test",
      datalabel = "test"
    )

    obj <- get_test_data()
    testthat::expect_null(filter_states$set_filter_state(data = obj, state = list(subset = NULL, select = NULL)))
  }
)

testthat::test_that("set_filter_state returns NULL when state argument is an empty list", {
  filter_states <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "test"
  )
  obj <- get_test_data()
  testthat::expect_null(filter_states$set_filter_state(data = obj, state = list()))
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
  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  testthat::expect_equal(
    isolate(sefs$get_call()),
    quote(test_filtered <- test)
  )
})

testthat::test_that("SEFilterStates$set_filter_state sets state with only subset", {
  obj <- get_test_data()
  test <- obj

  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  fs <- list(
    subset = list(feature_id = c("ID001", "ID002"))
  )
  sefs$set_filter_state(state = fs, data = obj)
  testthat::expect_equal(
    isolate(sefs$get_call()),
    quote(
      test_filtered <- subset(
        test,
        subset = feature_id %in% c("ID001", "ID002")
      )
    )
  )
})

testthat::test_that("SEFilterStates$set_filter_state updates select state which has been set already", {
  obj <- get_test_data()
  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )
  sefs$set_filter_state(state = list(select = list(Treatment = c("ChIP", "Input"))), data = obj)
  sefs$set_filter_state(state = list(select = list(Treatment = "ChIP")), data = obj)
  testthat::expect_equal(
    isolate(sefs$get_filter_state()),
    list(select = list(Treatment = list(selected = "ChIP", keep_na = FALSE)))
  )
})

testthat::test_that("SEFilterStates$set_filter_state updates subset state which has been set already", {
  obj <- get_test_data()
  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )
  sefs$set_filter_state(state = list(subset = list(feature_id = c("ID001", "ID002"))), data = obj)
  sefs$set_filter_state(state = list(subset = list(feature_id = "ID001")), data = obj)
  testthat::expect_equal(
    isolate(sefs$get_filter_state()),
    list(subset = list(feature_id = list(selected = "ID001", keep_na = FALSE)))
  )
})

testthat::test_that("SEFilterStates$set_filter_state updates subset state which has been set already", {
  obj <- get_test_data()
  test <- obj

  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  sefs$set_filter_state(state = list(subset = list(feature_id = c("ID001", "ID002"))), data = obj)
  sefs$set_filter_state(state = list(subset = list(feature_id = "ID001")), data = obj)
  testthat::expect_equal(
    isolate(sefs$get_filter_state()),
    list(subset = list(feature_id = list(selected = "ID001", keep_na = FALSE)))
  )
})

testthat::test_that("SEFilterStates$set_filter_state sets state with neither subset nor select", {
  obj <- get_test_data()
  test <- obj

  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  sefs$set_filter_state(state = list(), data = obj)

  eval(isolate(sefs$get_call()))
  testthat::expect_equal(test_filtered, test)
})

testthat::test_that(
  "SEFilterStates$set_filter_state sets filters in ReactiveQueue specified by the named list",
  code = {
    obj <- get_test_data()
    test <- obj
    sefs <- SEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test_filtered",
      datalabel = character(0)
    )

    fs <- list(
      select = list(Treatment = "ChIP"),
      subset = list(feature_id = c("ID001", "ID002"))
    )
    sefs$set_filter_state(state = fs, data = obj)

    eval(isolate(sefs$get_call()))
    testthat::expect_equal(test_filtered, subset(
      test,
      subset = feature_id %in% c("ID001", "ID002"),
      select = Treatment == "ChIP"
    ))
  }
)

testthat::test_that("SEFilterStates$get_filter_state returns list identical to input", {
  obj <- get_test_data()
  test <- obj
  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  fs <- list(
    subset = list(feature_id = list(selected = c("ID001", "ID002"), keep_na = TRUE)),
    select = list(Treatment = list(selected = "ChIP", keep_na = FALSE))
  )
  sefs$set_filter_state(state = fs, data = obj)
  testthat::expect_identical(isolate(sefs$get_filter_state()), fs)
})

testthat::test_that("SEFilterStates$remove_filter_state removes filters in ReactiveQueue", {
  obj <- get_test_data()
  test <- obj
  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  fs <- list(
    select = list(Treatment = "ChIP"),
    subset = list(feature_id = c("ID001", "ID002"))
  )

  sefs$set_filter_state(state = fs, data = obj)
  sefs$remove_filter_state(list(subset = "feature_id"))

  eval(isolate(sefs$get_call()))
  testthat::expect_equal(test_filtered, subset(
    test,
    select = Treatment == "ChIP"
  ))
})

testthat::test_that("SEFilterStates$remove_filter_state removes all filters in ReactiveQueue", {
  obj <- get_test_data()
  test <- obj
  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  fs <- list(
    select = list(Treatment = "ChIP"),
    subset = list(feature_id = c("ID001", "ID002"))
  )

  sefs$set_filter_state(state = fs, data = obj)
  sefs$remove_filter_state(list(subset = "feature_id", select = "Treatment"))

  eval(isolate(sefs$get_call()))
  testthat::expect_equal(test_filtered, test)
})

testthat::test_that("SEFilterStates$remove_filter_state throws error when list is not named", {
  obj <- get_test_data()
  test <- obj
  sefs <- SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )

  fs <- list(
    select = list(Treatment = "ChIP"),
    subset = list(feature_id = c("ID001", "ID002"))
  )

  sefs$set_filter_state(state = fs, data = obj)
  testthat::expect_error(sefs$remove_filter_state(list("feature_id")))
})

testthat::test_that(
  "SEFilterStates$remove_filter_state throws warning when list has unknown name in the FilterState",
  code = {
    suppress_logs()
    obj <- get_test_data()
    test <- obj
    sefs <- SEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test_filtered",
      datalabel = character(0)
    )

    fs <- list(
      select = list(Treatment = "ChIP"),
      subset = list(feature_id = c("ID001", "ID002"))
    )

    sefs$set_filter_state(state = fs, data = obj)
    testthat::expect_warning(sefs$remove_filter_state(list(subset = list("feature_id2"))))
  }
)
