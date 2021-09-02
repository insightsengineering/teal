testthat::test_that("get_filter_overview_info returns right array for MAEFilteredDataset without filtering", {
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

testthat::test_that("get_filter_overview_info returns right array for MAEFilteredDataset with filtering", {
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
