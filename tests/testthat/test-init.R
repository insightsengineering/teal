dataset_1 <- TealDataset$new("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl_dataset <- CDISCTealDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))
mods <- teal:::get_dummy_modules()

testthat::test_that("init data accepts TealData objects", {
  teal_data_object <- teal_data(dataset_1)
  cdisc_data_object <- cdisc_data(adsl_dataset)
  testthat::expect_error(init(data = teal_data_object, modules = mods), NA)
  testthat::expect_error(init(data = cdisc_data_object, modules = mods), NA)
})

testthat::test_that("init data throws an error with input other than accepted input", {
  character_vector <- c("a", "b", "c")
  numeric_vector <- c(1, 2, 3)
  matrix_d <- as.matrix(c(1, 2, 3))
  teal_data_list <- list(teal_data(dataset_1))
  mods <- teal:::get_dummy_modules()
  testthat::expect_error(init(data = character_vector, modules = mods))
  testthat::expect_error(init(data = numeric_vector, modules = mods))
  testthat::expect_error(init(data = numeric_vector, modules = mods))
  testthat::expect_error(init(data = matrix_d, modules = mods))
  testthat::expect_error(init(data = teal_data_list, modules = mods))
})

testthat::test_that("init data accepts a single TealDataset/CDISCTealDataset", {
  testthat::expect_error(init(data = TealDataset$new("iris", head(iris)), modules = mods), NA)
  testthat::expect_error(
    init(
      data = CDISCTealDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL")),
      modules = mods
    ),
    NA
  )
  testthat::expect_error(init(data = dataset_1, modules = mods), NA)
  testthat::expect_error(init(data = adsl_dataset, modules = mods), NA)
})

testthat::test_that("init data accepts a list of single TealDataset/CDISCTealDataset without renaming", {
  dataset_list <- list(TealDataset$new("iris", head(iris)))
  cdisc_dataset_list <- list(
    CDISCTealDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))
  )

  testthat::expect_error(init(data = list(TealDataset$new("iris", head(iris))), modules = mods), NA)
  testthat::expect_error(init(
    data = list(CDISCTealDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))),
    modules = mods
  ), NA)
  testthat::expect_error(init(data = dataset_list, modules = mods), NA)
  testthat::expect_error(init(data = cdisc_dataset_list, modules = mods), NA)
})

testthat::test_that("init data accepts a single dataframe", {
  testthat::expect_error(init(data = adsl_df, modules = mods), NA)
})

testthat::test_that("init data accepts a list of single dataframe without renaming", {
  testthat::expect_error(init(data = list(adsl_df), modules = mods), NA)
})

testthat::test_that("init data accepts a list of single dataframe with renaming", {
  adsl_list <- list(data1 = adsl_df)
  testthat::expect_error(init(data = list(data1 = adsl_df), modules = mods), NA)
  testthat::expect_error(init(data = adsl_list, modules = mods), NA)
})

testthat::test_that("init data accepts a list of a TealDataset and a dataframe without renaming", {
  testthat::expect_error(init(data = list(dataset_1, adsl_df), modules = mods), NA)
})

testthat::test_that("init data accepts MultiAssayExperiment object", {
  mae <- MultiAssayExperiment::miniACC
  testthat::expect_error(init(data = mae, modules = mods), NA)
})

testthat::test_that("init data accepts a list of a TealDataset and a dataframe with renaming", {
  testthat::expect_error(init(
    data = list(
      data1 = TealDataset$new("iris", head(iris)),
      data2 = as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
    ),
    modules = mods
  ), NA)
  testthat::expect_error(init(data = list(data1 = dataset_1, data2 = adsl_df), modules = mods), NA)
})

testthat::test_that("init data accepts a list of mixed TealDataset and dataframe with mixed renaming", {
  testthat::expect_error(init(data = list(data1 = TealDataset$new("iris", head(iris)), adsl_df), modules = mods), NA)
  testthat::expect_error(init(data = list(dataset_1, data2 = adsl_df), modules = mods), NA)
})

testthat::test_that("init data accepts TealDatasetConnector object", {
  dsc1 <- TealDatasetConnector$new("iris", CallableFunction$new(function() head(iris)))
  testthat::expect_error(init(data = dsc1, modules = mods), NA)
  testthat::expect_error(init(
    data = TealDatasetConnector$new("iris", CallableFunction$new(function() head(iris))),
    modules = mods
  ), NA)
})

testthat::test_that("init data accepts a list of TealDatasetConnector object", {
  dsc1 <- list(TealDatasetConnector$new("iris", CallableFunction$new(function() head(iris))))
  testthat::expect_error(init(data = dsc1, modules = mods), NA)
  testthat::expect_error(
    init(data = list(TealDatasetConnector$new("iris", CallableFunction$new(function() head(iris)))), modules = mods),
    NA
  )
})
