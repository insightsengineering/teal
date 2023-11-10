testthat::test_that("init data accepts TealData object", {
  lifecycle::expect_deprecated(
    init(
      data = teal.data::cdisc_data(
        teal.data::cdisc_dataset(
          "ADSL",
          as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADSL")))),
          parent = character(0),
          keys = teal.data::get_cdisc_keys("ADSL")
        )
      ),
      modules = teal:::example_modules(datanames = "ADSL")
    )
  )
})

testthat::test_that("init data accepts teal_data object", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(teal:::example_module())
    )
  )
})

testthat::test_that("init data throws an error with input other than TealData, teal_data and list(ui, server)", {
  character_vector <- c("a", "b", "c")
  numeric_vector <- c(1, 2, 3)
  matrix_d <- as.matrix(c(1, 2, 3))
  teal_data_list <- list(teal.data::teal_data(teal.data::dataset("iris", iris)))
  testthat::expect_error(init(data = character_vector, modules = modules(example_module())))
  testthat::expect_error(init(data = numeric_vector, modules = modules(example_module())))
  testthat::expect_error(init(data = numeric_vector, modules = modules(example_module())))
  testthat::expect_error(init(data = matrix_d, modules = modules(example_module())))
  testthat::expect_error(init(data = teal_data_list, modules = modules(example_module())))
})

testthat::test_that("init data accepts a single TealDataset", {
  testthat::expect_no_error(
    init(
      data = teal.data::dataset("ADSL", head(iris)),
      modules = teal:::example_modules(datanames = "ADSL")
    )
  )
})

testthat::test_that("init data accepts a list of single TealDataset without renaming", {
  testthat::expect_no_error(
    init(
      data = list(
        teal.data::dataset("ADSL", head(iris)),
        teal.data::dataset("ADTTE", head(iris))
      ),
      modules = teal:::example_modules()
    )
  )
})

testthat::test_that("init data accepts a single dataframe", {
  testthat::expect_no_error(
    init(data = list(iris = iris), modules = modules(example_module()))
  )
})

testthat::test_that("init data accepts a list of single dataframe without renaming", {
  testthat::expect_no_error(
    init(data = list(iris, mtcars), modules = modules(example_module()))
  )
})

testthat::test_that("init data accepts a list of single dataframe with renaming", {
  testthat::expect_no_error(
    init(
      data = list(iris2 = iris),
      modules = modules(example_module())
    )
  )
})

testthat::test_that("init data accepts a list of a TealDataset and a dataframe without renaming", {
  testthat::expect_no_error(
    init(
      data = list(teal.data::dataset("ADSL", head(iris)), iris),
      modules = modules(example_module())
    )
  )
})

testthat::test_that("init data accepts a single MultiAssayExperiment object", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(
    init(data = list(MAE = miniACC), modules = modules(example_module()))
  )
})

testthat::test_that("init data accepts a list of a single MultiAssayExperiment object without renaming", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = list(miniACC), modules = modules(example_module())))
})

testthat::test_that("init data accepts a list of a single MultiAssayExperiment object with renaming", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = list(x = miniACC), modules = modules(example_module())))
})

testthat::test_that("init data acceptsa mixed list of MultiAssayExperiment object and data.frame", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = list(x = miniACC, y = head(iris)), modules = modules(example_module())))
})

testthat::test_that("init data accepts a list of a TealDataset and a dataframe with renaming", {
  testthat::expect_no_error(init(
    data = list(
      data1 = teal.data::dataset("iris", head(iris)),
      data2 = as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
    ),
    modules = modules(example_module())
  ))
})

testthat::test_that("init data accepts a list of mixed TealDataset and dataframe with mixed renaming", {
  testthat::expect_no_error(
    init(
      data = list(
        data1 = teal.data::dataset("iris", head(iris)),
        iris2 = iris
      ),
      modules = modules(example_module())
    )
  )
})

testthat::test_that("init data accepts TealDatasetConnector object", {
  dsc1 <- teal.data::dataset_connector("iris", teal.data::callable_function(function() head(iris)))
  testthat::expect_no_error(init(data = dsc1, modules = modules(example_module())))
})

testthat::test_that("init data accepts a list of TealDatasetConnector object", {
  dsc1 <- list(teal.data::dataset_connector("iris", teal.data::callable_function(function() head(iris))))
  testthat::expect_no_error(init(data = dsc1, modules = modules(example_module())))
})

testthat::test_that("init data accepts teal_data_module", {
  testthat::expect_no_error(
    init(
      data = teal_data_module(ui = function(id) div(), server = function(id) NULL),
      modules = modules(teal:::example_module())
    )
  )
})

testthat::test_that("init teal_data_module doesn't accept ui and server with other formals than id", {
  testthat::expect_error(
    init(
      data = teal_data_module(ui = function(id, x) div(), server = function(id) NULL),
      modules = modules(teal:::example_module())
    ),
    "Must have exactly 1 formal arguments"
  )
  testthat::expect_error(
    init(
      data = teal_data_module(ui = function(id) div(), server = function(id, x) NULL),
      modules = modules(teal:::example_module())
    ),
    "Must have exactly 1 formal arguments"
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

testthat::test_that("init filter accepts `teal_slices`", {
  fs <- teal.slice::teal_slices(
    teal.slice::teal_slice(dataname = "iris", varname = "species", selected = "setosa")
  )
  testthat::expect_no_error(init(data = list(iris), modules = modules(example_module()), filter = fs))
  testthat::expect_error(
    init(data = list(iris), modules = modules(example_module()), filter = unclass(fs)),
    "Assertion failed"
  )
})

testthat::test_that("init throws when data has no datanames", {
  testthat::expect_error(
    init(data = teal_data(), modules = list(example_module())),
    "has no datanames"
  )
})

testthat::test_that("init throws when incompatible module's datanames", {
  msg <- "Module 'example teal module' uses datanames not available in 'data'"
  testthat::expect_output(
    testthat::expect_error(
      init(data = teal_data(mtcars = mtcars), modules = list(example_module(datanames = "iris"))),
      msg
    ),
    msg
  )
})

testthat::test_that("init throws when incompatible filter's datanames", {
  testthat::expect_output(
    init(
      data = teal_data(mtcars = mtcars),
      modules = modules(example_module()),
      filter = teal_slices(teal_slice(dataname = "iris", varname = "Species"))
    ),
    "Filter 'iris Species' refers to dataname not available in 'data'"
  )
})
