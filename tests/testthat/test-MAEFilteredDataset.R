library(MultiAssayExperiment)
data(miniACC)

testthat::test_that("MAEFilteredDataset accepts a MAEDataset object", {
  testthat::expect_error(MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC)), NA)
})

testthat::test_that("MAEFilteredDataset throws error with non-MultiAssayExperiment dataset", {
  testthat::expect_error(
    MAEFilteredDataset$new(dataset = MAEDataset$new("iris", head(iris))),
    "is(x, \"MultiAssayExperiment\") is not TRUE"
    , fixed = TRUE)
})

testthat::test_that("MAEFilteredDataset$get_call returns a call without applying filter", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))
  get_call_output <- filtered_dataset$get_call()
  testthat::expect_true(is_class_list("language")(get_call_output))
  testthat::expect_identical(deparse(get_call_output$subjects), "miniACC_FILTERED <- miniACC")
})

testthat::test_that("MAEFilteredDataset$get_call returns a call with applying filter", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))
  filter_state_mae <- ChoicesFilterState$new(
    x = c("white"),
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )

  queue <- filtered_dataset$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")

  get_call_output <- isolate(filtered_dataset$get_call())

  testthat::expect_true(is_class_list("language")(get_call_output))
  testthat::expect_identical(
    deparse(get_call_output$subjects),
    c("miniACC_FILTERED <- MultiAssayExperiment::subsetByColData(miniACC, ", "    y = miniACC$race == \"white\")")
  )
})

testthat::test_that("MAEFilteredDataset$get_data throws error without filtered argument given", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))
  expect_error(isolate(filtered_dataset$get_data()), "argument \"filtered\" is missing, with no default")
})

testthat::test_that("MAEFilteredDataset$get_data returns identical filtered and
                    non-filtered MAE data when no filter is applied", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))
  filtered_mae <- isolate(filtered_dataset$get_data(filtered = TRUE))
  non_filtered_mae <- isolate(filtered_dataset$get_data(filtered = FALSE))
  expect_identical(filtered_mae, non_filtered_mae)
})

testthat::test_that("MAEFilteredDataset get_data returns filtered MAE data when filter is applied", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))
  filter_state_mae <- ChoicesFilterState$new(
    x = c("white"),
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )

  queue <- filtered_dataset$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")

  filtered_mae <- isolate(filtered_dataset$get_data(filtered = TRUE))
  non_filtered_mae <- isolate(filtered_dataset$get_data(filtered = FALSE))

  expect_false(identical(filtered_mae, non_filtered_mae))
  expect_identical(unique(filtered_mae$race), "white")
})

testthat::test_that("get_filter_overview_info returns overview matrix for MAEFilteredDataset without filtering", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))
  testthat::expect_equal(
    isolate(filtered_dataset$get_filter_overview_info()),
    matrix(
      list("", "92/92", "79/79", "79/79", "90/90", "90/90", "46/46", "46/46", "90/90", "90/90", "80/80", "80/80"),
      nrow = 6,
      byrow = TRUE,
      dimnames = list(
        c("miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray", "- Mutations", "- miRNASeqGene"),
        c("Obs", "Subjects")
      )
    )
  )
})

testthat::test_that("get_filter_overview_info returns overview matrix for MAEFilteredDataset with filtering", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))

  filter_state_mae <- ChoicesFilterState$new(
    x = c("white"),
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )

  queue <- filtered_dataset$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")

  testthat::expect_equal(
    isolate(filtered_dataset$get_filter_overview_info()),
    matrix(
      list("", "78/92", "66/79", "66/79", "76/90", "76/90", "35/46", "35/46", "77/90", "77/90", "67/80", "67/80"),
      nrow = 6,
      byrow = TRUE,
      dimnames = list(
        c("miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray", "- Mutations", "- miRNASeqGene"),
        c("Obs", "Subjects")
      )
    )
  )
})

testthat::test_that("MAEFilteredDataset$set_bookmark_state sets filters in FilterStates specified by list names", {
  dataset <- teal:::MAEFilteredDataset$new(dataset("MAE", MultiAssayExperiment::miniACC))
  fs <- list(
    subjects = list(
      years_to_birth = c(30, 50),
      vital_status = 1,
      gender = "female"
    ),
    RPPAArray = list(
      subset = list(ARRAY_TYPE = "")
    )
  )
  shiny::testServer(dataset$set_bookmark_state, args = list(state = fs), expr = NULL)
  testthat::expect_equal(
    isolate(dataset$get_call()),
    list(
      subjects = quote(
        MAE_FILTERED <- MultiAssayExperiment::subsetByColData( # nolint
          MAE,
          y = MAE$years_to_birth >=  30 & MAE$years_to_birth <= 50 &
            MAE$vital_status == "1" &
            MAE$gender == "female"
        )
      ),
      RPPAArray = quote(
        MAE_FILTERED[["RPPAArray"]] <- subset( # nolint
          MAE_FILTERED[["RPPAArray"]],
          subset = ARRAY_TYPE == ""
        )
      )
    )
  )
})

testthat::test_that("MAEFilteredDataset$set_bookmark_state throws error if state argument is not a list ", {
  dataset <- teal:::MAEFilteredDataset$new(dataset("MAE", MultiAssayExperiment::miniACC))
  fs <- c("not_list")
  expect_error(
    shiny::testServer(dataset$set_bookmark_state, args = list(state = fs), expr = NULL),
    "is.list(state) is not TRUE",
    fixed = TRUE
  )
})

testthat::test_that("MAEFilteredDataset$get_filterable_varnames returns character(0)", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))
  expect_identical(filtered_dataset$get_filterable_varnames(), character(0))
})

testthat::test_that("MAEFilteredDataset$srv_add_filter_state adds filters", {
  filtered_dataset <- MAEFilteredDataset$new(dataset = MAEDataset$new("miniACC", miniACC))
  filter_state_mae <- ChoicesFilterState$new(
    x = c("white"),
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )

  queue <- filtered_dataset$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")

  shiny::testServer(filtered_dataset$srv_add_filter_state, expr = NULL)
  expect_identical(
    deparse(isolate(filtered_dataset$get_call())$subjects),
    c("miniACC_FILTERED <- MultiAssayExperiment::subsetByColData(miniACC, ", "    y = miniACC$race == \"white\")")
  )
})
