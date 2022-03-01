testthat::test_that("MAETealDataset constructors do not raise exceptions", {
  testthat::expect_silent(MAETealDataset$new("testMAE", MultiAssayExperiment::miniACC))
  code_class <- CodeClass$new(
    "testMAE <- MultiAssayExperiment::miniACC",
    dataname = "testMAE"
  )
  testthat::expect_silent(
    MAETealDataset$new(dataname = "testMAE", x = MultiAssayExperiment::miniACC, code = code_class)
  )
})

testthat::test_that("MAETealDataset$recreate updates the class fields", {
  mae <- MAETealDataset$new("testMAE", MultiAssayExperiment::miniACC)

  suppressWarnings(new_data <- MultiAssayExperiment::miniACC[, , "RNASeq2GeneNorm"]) # warning only on rocker 4.1
  new_name <- "new_name"
  new_label <- "new_label"
  new_code <- "new_code"
  new_keys <- c("new_key")
  new_vars <- list(new_var = "new_var")

  testthat::expect_silent(mae$recreate(
    dataname = new_name,
    x = new_data,
    keys = new_keys,
    code = new_code,
    vars = new_vars
  ))
  testthat::expect_equal(mae$get_dataname(), new_name)
  testthat::expect_equal(mae$get_raw_data(), new_data)
  testthat::expect_equal(mae$get_keys(), new_keys)
  testthat::expect_equal(mae$get_code(), "new_var <- \"new_var\"\nnew_code")
})

testthat::test_that("MAETealDataset getters and setters", {
  mae <- MAETealDataset$new(
    dataname = "miniACC",
    x = MultiAssayExperiment::miniACC,
    metadata = list(A = 5, B = TRUE, C = "foo")
  )

  testthat::expect_equal(mae$get_dataname(), "miniACC")
  testthat::expect_equal(mae$get_datanames(), mae$get_dataname())
  testthat::expect_equal(mae$get_metadata(), list(A = 5, B = TRUE, C = "foo"))
  testthat::expect_equal(mae$get_raw_data(), MultiAssayExperiment::miniACC)

  new_label <- "new_label"
  testthat::expect_silent(mae$set_dataset_label(new_label))
  testthat::expect_equal(mae$get_dataset_label(), new_label)

  new_keys <- c("new_key")
  testthat::expect_silent(mae$set_keys(new_keys))
  testthat::expect_equal(mae$get_keys(), new_keys)

  new_code <- "new_code"
  testthat::expect_silent(mae$set_code(new_code))
  testthat::expect_equal(mae$get_code(), new_code)

  new_vars <- list(new_var = "new_var")
  testthat::expect_silent(mae$set_vars(new_vars))
})

testthat::test_that("MAETealDataset$is_pulled returns true", {
  mae <- MAETealDataset$new(dataname = "miniACC", x = MultiAssayExperiment::miniACC)
  testthat::expect_true(mae$is_pulled())
})

testthat::test_that("MAETealDataset$check returns TRUE when constructed with the correct code", {
  exprss1 <- matrix(
    seq(from = 1, by = 0.1, length.out = 16),
    ncol = 4,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jill", "Bob", "Bobby"))
  )
  exprss2 <- matrix(
    seq(from = 5, by = 0.1, length.out = 12),
    ncol = 3,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jane", "Bob"))
  )
  double_exp <- list("methyl 2k" = exprss1, "methyl 3k" = exprss2)
  simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments = double_exp)

  mae_dataset <- MAETealDataset$new(
    dataname = "simple_mae",
    x = simple_mae,
    code = "exprss1 <- matrix(
      seq(from = 1, by = 0.1, length.out = 16),
      ncol = 4,
      dimnames = list(sprintf('ENST00000%i', seq.int(1, 4)), c('Jack', 'Jill', 'Bob', 'Bobby'))
    )
    exprss2 <- matrix(
      seq(from = 5, by = 0.1, length.out = 12), ncol = 3,
      dimnames = list(sprintf('ENST00000%i', seq.int(1, 4)), c('Jack', 'Jane', 'Bob'))
    )
    doubleExp <- list('methyl 2k' = exprss1, 'methyl 3k' = exprss2)
    simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments=doubleExp)"
  )
  testthat::expect_true(mae_dataset$check())
})

testthat::test_that("FALSE returned when executing MAETealDataset$check and code is not correct", {
  exprss1 <- matrix(
    seq(from = 1, by = 0.1, length.out = 16),
    ncol = 4,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jill", "Bob", "Bobby"))
  )
  exprss2 <- matrix(
    seq(from = 5, by = 0.1, length.out = 12),
    ncol = 3,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jane", "Bob"))
  )
  double_exp <- list("methyl 2k" = exprss1, "methyl 3k" = exprss2)
  simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments = double_exp)

  mae_dataset <- MAETealDataset$new(
    dataname = "simple_mae",
    x = simple_mae,
    code = "exprss1 <- matrix(
      seq(from = 1, by = 0.1, length.out = 16),
      ncol = 4,
      dimnames = list(sprintf('ENST00000%i', seq.int(1, 4)), c('Jack', 'Jill', 'Bob', 'Bobby'))
    )
    exprss2 <- matrix(
      seq(from = 5, by = 0.1, length.out = 12), ncol = 3,
      dimnames = list(sprintf('ENST00000%i', seq.int(1, 4)), c('Jack', 'Jane', 'Bob'))
    )
    doubleExp <- list('methyl 1k' = exprss1, 'methyl 3k' = exprss2)
    simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments=doubleExp)"
  )
  testthat::expect_false(mae_dataset$check())
})

testthat::test_that("Error raised when executing MAETealDataset$check and code is empty", {
  exprss1 <- matrix(
    seq(from = 1, by = 0.1, length.out = 16),
    ncol = 4,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jill", "Bob", "Bobby"))
  )
  exprss2 <- matrix(
    seq(from = 5, by = 0.1, length.out = 12),
    ncol = 3,
    dimnames = list(sprintf("ENST00000%i", seq.int(1, 4)), c("Jack", "Jane", "Bob"))
  )
  double_exp <- list("methyl 2k" = exprss1, "methyl 3k" = exprss2)
  simple_mae <- MultiAssayExperiment::MultiAssayExperiment(experiments = double_exp)

  mae_dataset <- MAETealDataset$new(dataname = "simple_mae", x = simple_mae, code = "")
  testthat::expect_error(
    mae_dataset$check(),
    regexp = "Cannot check preprocessing code of"
  )
})

testthat::test_that("MAETealDataset$check_keys doesn't throw if constructed with correct keys", {
  mae <- MAETealDataset$new(dataname = "miniACC", x = MultiAssayExperiment::miniACC, keys = "patientID")
  testthat::expect_silent(mae$check_keys())
})

testthat::test_that("MAETealDataset$check_keys throws if constructed with keys not present in colData", {
  mae <- MAETealDataset$new(dataname = "miniACC", x = MultiAssayExperiment::miniACC, keys = "wrong keys")
  testthat::expect_error(
    mae$check_keys(),
    regexp = "do not exist in the data"
  )
})

testthat::test_that("Error raised executing MAETealDataset$check_keys and duplicate rows found in key columns", {
  array_data <- matrix(
    seq(101, 108),
    ncol = 4,
    dimnames = list(c("probe1", "probe2"), c("sample1", "sample2", "sample3", "sample4"))
  )
  col_data <- data.frame(
    sample_id = c("sample1", "sample2", "sample3", "sample3"),
    row.names = c("sample1", "sample2", "sample3", "sample4")
  )
  test_mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = list("test_exp1" = array_data),
    colData = col_data
  )
  mae_dataset <- MAETealDataset$new(dataname = "test_mae", x = test_mae, keys = "sample_id")
  testthat::expect_error(
    mae_dataset$check_keys(),
    regexp = "Duplicate primary key values found in the dataset 'test_mae'"
  )
})

testthat::test_that("dataset() does not throw when passed a MultiAssayExperiment object", {
  testthat::expect_error(dataset("mae", MultiAssayExperiment::miniACC), NA)
})

testthat::test_that("dataset() constructor returns the same as MAETealDataset$new()", {
  mae1 <- dataset("mae", MultiAssayExperiment::miniACC)
  mae2 <- MAETealDataset$new("mae", MultiAssayExperiment::miniACC)
  testthat::expect_equal(mae1, mae2)
})
