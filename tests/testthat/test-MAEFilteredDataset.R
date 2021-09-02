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
