dataset_1 <- teal.data::dataset("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
adsl_dataset <- teal.data::cdisc_dataset(
  "ADSL", adsl_df,
  parent = character(0), keys = teal.data::get_cdisc_keys("ADSL")
)
mods <- teal:::example_modules()

testthat::test_that("init data accepts TealData objects", {
  teal_data_object <- teal.data::teal_data(dataset_1)
  cdisc_data_object <- teal.data::cdisc_data(adsl_dataset)
  testthat::expect_no_error(init(data = teal_data_object, modules = mods))
  testthat::expect_no_error(init(data = cdisc_data_object, modules = mods))
})

testthat::test_that("init data throws an error with input other than accepted input", {
  character_vector <- c("a", "b", "c")
  numeric_vector <- c(1, 2, 3)
  matrix_d <- as.matrix(c(1, 2, 3))
  teal_data_list <- list(teal.data::teal_data(dataset_1))
  mods <- teal:::example_modules()
  testthat::expect_error(init(data = character_vector, modules = mods))
  testthat::expect_error(init(data = numeric_vector, modules = mods))
  testthat::expect_error(init(data = numeric_vector, modules = mods))
  testthat::expect_error(init(data = matrix_d, modules = mods))
  testthat::expect_error(init(data = teal_data_list, modules = mods))
})

testthat::test_that("init data accepts a single TealDataset/CDISCTealDataset", {
  testthat::expect_no_error(init(data = teal.data::dataset("iris", head(iris)), modules = mods))
  testthat::expect_no_error(
    init(
      data = teal.data::cdisc_dataset("ADSL", adsl_df, parent = character(0), keys = teal.data::get_cdisc_keys("ADSL")),
      modules = mods
    )
  )
  testthat::expect_no_error(init(data = dataset_1, modules = mods))
  testthat::expect_no_error(init(data = adsl_dataset, modules = mods))
})

testthat::test_that("init data accepts a list of single TealDataset/CDISCTealDataset without renaming", {
  dataset_list <- list(teal.data::dataset("iris", head(iris)))
  cdisc_dataset_list <- list(
    teal.data::cdisc_dataset("ADSL", adsl_df, parent = character(0), keys = teal.data::get_cdisc_keys("ADSL"))
  )

  testthat::expect_no_error(init(data = list(teal.data::dataset("iris", head(iris))), modules = mods))
  testthat::expect_no_error(
    init(
      data = list(
        teal.data::cdisc_dataset("ADSL", adsl_df, parent = character(0), keys = teal.data::get_cdisc_keys("ADSL"))
      ),
      modules = mods
    )
  )
  testthat::expect_no_error(init(data = dataset_list, modules = mods))
  testthat::expect_no_error(init(data = cdisc_dataset_list, modules = mods))
})

testthat::test_that("init data accepts a single dataframe", {
  testthat::expect_no_error(init(data = adsl_df, modules = mods))
})

testthat::test_that("init data accepts a list of single dataframe without renaming", {
  testthat::expect_no_error(init(data = list(adsl_df), modules = mods))
})

testthat::test_that("init data accepts a list of single dataframe with renaming", {
  adsl_list <- list(data1 = adsl_df)
  testthat::expect_no_error(init(data = list(data1 = adsl_df), modules = mods))
  testthat::expect_no_error(init(data = adsl_list, modules = mods))
})

testthat::test_that("init data accepts a list of a TealDataset and a dataframe without renaming", {
  testthat::expect_no_error(init(data = list(dataset_1, adsl_df), modules = mods))
})

testthat::test_that("init data accepts a single MultiAssayExperiment object", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = miniACC, modules = mods))
})

testthat::test_that("init data accepts a list of a single MultiAssayExperiment object without renaming", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = list(miniACC), modules = mods))
})

testthat::test_that("init data accepts a list of a single MultiAssayExperiment object with renaming", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = list(x = miniACC), modules = mods))
})

testthat::test_that("init data acceptsa mixed list of MultiAssayExperiment object and data.frame", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = list(x = miniACC, y = head(iris)), modules = mods))
})

testthat::test_that("init data accepts a list of a TealDataset and a dataframe with renaming", {
  testthat::expect_no_error(
    init(
      data = list(
        data1 = teal.data::dataset("iris", head(iris)),
        data2 = as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
      ),
      modules = mods
    )
  )
  testthat::expect_no_error(init(data = list(data1 = dataset_1, data2 = adsl_df), modules = mods))
})

testthat::test_that("init data accepts a list of mixed TealDataset and dataframe with mixed renaming", {
  testthat::expect_no_error(init(data = list(data1 = teal.data::dataset("iris", head(iris)), adsl_df), modules = mods))
  testthat::expect_no_error(init(data = list(dataset_1, data2 = adsl_df), modules = mods))
})

testthat::test_that("init data accepts TealDatasetConnector object", {
  dsc1 <- teal.data::dataset_connector("iris", teal.data::callable_function(function() head(iris)))
  testthat::expect_no_error(init(data = dsc1, modules = mods))
  testthat::expect_no_error(
    init(
      data = teal.data::dataset_connector("iris", teal.data::callable_function(function() head(iris))),
      modules = mods
    )
  )
})

testthat::test_that("init data accepts a list of TealDatasetConnector object", {
  dsc1 <- list(teal.data::dataset_connector("iris", teal.data::callable_function(function() head(iris))))
  testthat::expect_no_error(init(data = dsc1, modules = mods))
  testthat::expect_no_error(
    init(data = list(
      teal.data::dataset_connector("iris", teal.data::callable_function(function() head(iris)))
    ),
    modules = mods
    )
  )
})

testthat::test_that("init modules accepts a teal_modules object", {
  mods <- modules(example_module(), example_module())
  testthat::expect_no_error(init(data = iris, modules = mods))
})

testthat::test_that("init modules accepts a list of teal_module elements", {
  mods <- list(example_module(), example_module())
  testthat::expect_no_error(init(data = iris, modules = mods))
})

testthat::test_that("init modules accepts a teal_module object", {
  mods <- example_module()
  testthat::expect_no_error(init(data = iris, modules = mods))
})

testthat::test_that("init filter accepts named list or `teal_slices`", {
  fl <- list(
    "iris" = list(
      "Species" = list(selected = "setosa")
    )
  )
  fs <- teal.slice::teal_slices(
    teal.slice::teal_slice(dataname = "iris", varname = "species", selected = "setosa")
  )
  testthat::expect_no_error(init(data = dataset_1, modules = mods, filter = fl))
  testthat::expect_no_error(init(data = dataset_1, modules = mods, filter = fs))
  testthat::expect_error(init(data = dataset_1, modules = mods, filter = unclass(fs)), "Assertion failed")
})
