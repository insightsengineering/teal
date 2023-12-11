testthat::test_that("init data accepts teal_data object", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(teal:::example_module())
    )
  )
})

testthat::test_that("init data accepts a single dataframe", {
  testthat::expect_no_error(
    init(data = list(iris = iris), modules = modules(example_module()))
  )
})

testthat::test_that("init data accepts a list of single dataframe without renaming", {
  testthat::skip("todo: should we support data as unnamed list in teal?")
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

testthat::test_that("init data accepts a single MultiAssayExperiment object", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(
    init(data = list(MAE = miniACC), modules = modules(example_module()))
  )
})

testthat::test_that("init data accepts a list of a single MultiAssayExperiment object with renaming", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = list(x = miniACC), modules = modules(example_module())))
})

testthat::test_that("init data acceptsa mixed list of MultiAssayExperiment object and data.frame", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  testthat::expect_no_error(init(data = list(x = miniACC, y = head(iris)), modules = modules(example_module())))
})

testthat::test_that("init data accepts teal_data_module", {
  testthat::expect_no_error(
    init(
      data = teal_data_module(ui = function(id) div(), server = function(id) NULL),
      modules = modules(teal:::example_module())
    )
  )
})

testthat::test_that("init modules accepts a teal_modules object", {
  mods <- modules(example_module(), example_module())
  testthat::expect_no_error(init(data = list(iris = iris), modules = mods))
})

testthat::test_that("init modules accepts a list of teal_module elements", {
  mods <- list(example_module(), example_module())
  testthat::expect_no_error(init(data = list(iris = iris), modules = mods))
})

testthat::test_that("init modules accepts a teal_module object", {
  mods <- example_module()
  testthat::expect_no_error(init(data = list(iris = iris), modules = mods))
})

testthat::test_that("init filter accepts `teal_slices`", {
  fs <- teal.slice::teal_slices(
    teal.slice::teal_slice(dataname = "iris", varname = "species", selected = "setosa")
  )
  testthat::expect_no_error(init(data = list(iris = iris), modules = modules(example_module()), filter = fs))
  testthat::expect_error(
    init(data = list(iris = iris), modules = modules(example_module()), filter = unclass(fs)),
    "Assertion failed"
  )
})

testthat::test_that("init throws when datanames is specified from environment.", {
  data <- within(teal_data(), {
    iris <- iris
  })

  testthat::expect_warning(
    init(data = data, modules = list(example_module())),
    "datanames are specified from environment"
  )
})

testthat::test_that("init throws when data has no datanames", {
  testthat::expect_error(
    init(data = teal_data(), modules = list(example_module())),
    "cannot assign datanames"
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
