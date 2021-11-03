dataset_1 <- Dataset$new("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl_dataset <- CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))
mods <- teal:::get_dummy_modules()

testthat::test_that("init data accepts RelationalData objects", {
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

testthat::test_that("init data accepts a single Dataset/CDISCDataset", {
  testthat::expect_error(init(data = Dataset$new("iris", head(iris)), modules = mods), NA)
  testthat::expect_error(init(
    data = CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL")),
    modules = mods), NA)
  testthat::expect_error(init(data = dataset_1, modules = mods), NA)
  testthat::expect_error(init(data = adsl_dataset, modules = mods), NA)
})

testthat::test_that("init data accepts a list of single Dataset/CDISCDataset without renaming", {
  dataset_list <- list(Dataset$new("iris", head(iris)))
  cdisc_dataset_list <- list(CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL")))

  testthat::expect_error(init(data = list(Dataset$new("iris", head(iris))), modules = mods), NA)
  testthat::expect_error(init(
    data = list(CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))),
    modules = mods), NA)
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

testthat::test_that("init data accepts a list of a Dataset and a dataframe without renaming", {
  testthat::expect_error(
    init(
      data = list(
        dataset_1,
        adsl_df
      ),
      modules = mods),
  NA)
})

testthat::test_that("init data accepts a list of a Dataset and a dataframe with renaming", {
  testthat::expect_error(init(data = list(
    data1 = Dataset$new("iris", head(iris)),
    data2 = as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
    ),
    modules = mods), NA)
  testthat::expect_error(init(data = list(
    data1 = dataset_1,
    data2 = adsl_df
  ),
  modules = mods), NA)
})

testthat::test_that("init data accepts a list of mixed Dataset and dataframe with mixed renaming", {
  testthat::expect_error(init(data = list(data1 = Dataset$new("iris", head(iris)), adsl_df), modules = mods), NA)
  testthat::expect_error(init(data = list(dataset_1, data2 = adsl_df), modules = mods), NA)
})

testthat::test_that("init data accepts DatasetConnector object", {
  dsc1 <- DatasetConnector$new("iris", CallableFunction$new(function() head(iris)))
  testthat::expect_error(init(data = dsc1, modules = mods), NA)
  testthat::expect_error(init(
    data = DatasetConnector$new("iris", CallableFunction$new(function() head(iris))),
    modules = mods
    ),
  NA)
})

testthat::test_that("init data accepts a list of DatasetConnector object", {
  dsc1 <- list(DatasetConnector$new("iris", CallableFunction$new(function() head(iris))))
  testthat::expect_error(init(data = dsc1, modules = mods), NA)
  testthat::expect_error(
    init(data = list(DatasetConnector$new("iris", CallableFunction$new(function() head(iris)))), modules = mods),
  NA)
})
