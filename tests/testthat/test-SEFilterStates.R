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
  testthat::expect_null(filter_states$queue_get(2))
})

testthat::test_that("set_bookmark_state sets correct filters for SEFilterStates", {
  sefs <- teal:::SEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0)
  )
  library(SummarizedExperiment)
  nrows <- 200
  ncols <- 6
  counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
  rowRanges <- GRanges(rep(c("chr1", "chr2"), c(50, 150)),
                       IRanges(floor(runif(200, 1e5, 1e6)), width = 100),
                       strand = sample(c("+", "-"), 200, TRUE),
                       feature_id = sprintf("ID%03d", 1:200))
  colData <- DataFrame(Treatment = rep(c("ChIP", "Input"), 3),
                       row.names = LETTERS[1:6])

  obj <- SummarizedExperiment(
    assays = list(counts = counts),
    rowRanges = rowRanges,
    colData = colData
  )

  fs <- list(
    select = list(Treatment = "ChIP"),
    subset = list(feature_id = c("ID001", "ID002"))
  )

  testthat::expect_error(sefs$set_bookmark_state(state = fs, data = obj), NA)
  expect_equal(
    isolate(sefs$get_call()),
    quote(
      test_filtered <- subset(
        test,
        subset = feature_id %in% c("ID001", "ID002"),
        select = Treatment == "ChIP"
      )
    )
  )
})
