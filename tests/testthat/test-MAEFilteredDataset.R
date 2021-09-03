testthat::test_that("get_filter_overview_info returns overview matrix for MAEFilteredDataset without filtering", {
  library(MultiAssayExperiment)
  data(miniACC)
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
  library(MultiAssayExperiment)
  dataset <- teal:::MAEFilteredDataset$new(dataset("MAE", miniACC))
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
  dataset$set_bookmark_state(fs)
  expect_equal(
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
