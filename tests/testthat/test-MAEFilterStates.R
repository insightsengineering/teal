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
    varname = as.name("race"),
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

testthat::test_that(
  "MAEFilterStates$set_filter_state sets filters in FilterState(s) specified by the named list",
  { # nolint
    maefs <- MAEFilterStates$new(
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
    maefs$set_filter_state(state = fs, data = MultiAssayExperiment::miniACC)

    testthat::expect_equal(
      isolate(maefs$get_call()),
      quote(
        test_filtered <- MultiAssayExperiment::subsetByColData(
          test,
          y = test$years_to_birth >= 30 & test$years_to_birth <= 50 &
            test$vital_status == "1" &
            test$gender == "female"
        )
      )
    )
  }
)

testthat::test_that(
  "MAEFilterStates$set_filter_state throws error when not using a named list",
  { # nolint
    maefs <- MAEFilterStates$new(
      input_dataname = "test",
      output_dataname = "test_filtered",
      datalabel = character(0),
      varlabels = character(0),
      keys = character(0)
    )
    fs <- list(
      c(30, 50),
      vital_status = 1,
      gender = "female"
    )
    testthat::expect_error(maefs$set_filter_state(state = fs, data = MultiAssayExperiment::miniACC))
  }
)

testthat::test_that(
  "MAEFilterStates$remove_filter_state removes filters in FilterState(s)",
  { # nolint
    maefs <- MAEFilterStates$new(
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
    years_to_birth_remove_fs <- "years_to_birth"

    maefs$set_filter_state(state = fs, data = MultiAssayExperiment::miniACC)
    maefs$remove_filter_state(years_to_birth_remove_fs)

    testthat::expect_equal(
      isolate(maefs$get_call()),
      quote(
        test_filtered <- MultiAssayExperiment::subsetByColData(
          test,
          y = test$vital_status == "1" &
            test$gender == "female"
        )
      )
    )
  }
)

testthat::test_that(
  "MAEFilterStates$remove_filter_state throws warning when name is not in FilterStates",
  { # nolint
    maefs <- MAEFilterStates$new(
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
    years_to_birth_remove_fs <- "years_to_birth2"

    maefs$set_filter_state(state = fs, data = MultiAssayExperiment::miniACC)
    testthat::expect_warning(maefs$remove_filter_state(years_to_birth_remove_fs))
  }
)
