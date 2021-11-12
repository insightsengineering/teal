testthat::test_that("The constructor does not throw", {
  testthat::expect_error(MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  ), NA)
})

testthat::test_that("get_fun returns the MAE specific subset function", {
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  testthat::expect_equal(filter_states$get_fun(), "MultiAssayExperiment::subsetByColData")
})

testthat::test_that("The constructor initializes a queue", {
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  testthat::expect_null(filter_states$queue_get(1))
})

testthat::test_that("get_call returns a call filtering an MAE object using ChoicesFilterState", {
  library(MultiAssayExperiment)
  data(miniACC)
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  filter_state <- ChoicesFilterState$new(
    x = c("white", NA_character_),
    varname = c("race"),
    input_dataname = as.name("test"),
    extract_type = "list"
  )
  filter_state$set_na_rm(TRUE)
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")

  test <- miniACC
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(
    output,
    MultiAssayExperiment::subsetByColData(test, !is.na(test$race) & test$race == "white")
  )
})

testthat::test_that("get_call returns a call filtering an MAE object using RangeFilterState", {
  library(MultiAssayExperiment)
  data(miniACC)
  filter_states <- MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "output",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  filter_state <- RangeFilterState$new(
    x = miniACC$purity,
    varname = as.name("purity"),
    input_dataname = as.name("test"),
    extract_type = "list"
  )
  filter_state$set_na_rm(TRUE)
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")

  test <- miniACC
  eval(isolate(filter_states$get_call()))

  min_purity <- min(miniACC$purity, na.rm = TRUE)
  max_purity <- max(miniACC$purity, na.rm = TRUE)

  testthat::expect_equal(
    output,
    MultiAssayExperiment::subsetByColData(
      test,
      !is.na(miniACC$purity) & (miniACC$purity >= min_purity & miniACC$purity <= max_purity)
    )
  )
})

testthat::test_that("MAEFilterStates$set_bookmark_state sets filters in FilterState(s) specified by the named list", {
  maefs <- teal:::MAEFilterStates$new(
    input_dataname = "test",
    output_dataname = "test_filtered",
    datalabel = character(0),
    varlabels = character(0),
    keys = character(0)
  )
  fs <- list(
    years_to_birth = c(30, 50),
    vital_status = 1,
    gender = "female"
  )
  shiny::testServer(
    maefs$set_bookmark_state,
    args = list(state = fs, data = MultiAssayExperiment::miniACC),
    expr = NULL
  )
  testthat::expect_equal(
    isolate(maefs$get_call()),
    quote(
      test_filtered <- MultiAssayExperiment::subsetByColData(
        test,
        y = test$years_to_birth >=  30 & test$years_to_birth <= 50 &
          test$vital_status == "1" &
          test$gender == "female"
      )
    )
  )
})

testthat::test_that(
  "MultiAssayExperiment::subsetByColData returns error when variable contains NAs", {
    # if this test fails it means that we can remove FilterState$set_na_rm which
    # has been created after breaking change in MAE
    library(MultiAssayExperiment)
    data(miniACC)
    miniACC$test <- sample(c(TRUE, NA), size = nrow(miniACC@colData), replace = TRUE)
    if (compareVersion(as.character(BiocManager::version()), "3.14") >= 0) {
      testthat::expect_error(
        subsetByColData(miniACC, miniACC$test),
        "logical subscript contains NAs"
      )
    }
  }
)
