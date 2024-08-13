# data ----
testthat::test_that("init data accepts teal_data object", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )
  )
})

testthat::test_that("init data accepts teal_data_module", {
  testthat::expect_no_error(
    init(
      data = teal_data_module(ui = function(id) tags$div(), server = function(id) NULL),
      modules = modules(example_module())
    )
  )
})


# modules ----
testthat::test_that("init modules accepts a teal_modules object", {
  mods <- modules(example_module(), example_module())
  testthat::expect_no_error(init(data = teal.data::teal_data(iris = iris), modules = mods))
})

testthat::test_that("init modules accepts a list of teal_module elements", {
  mods <- list(example_module(), example_module())
  testthat::expect_no_error(init(data = teal.data::teal_data(iris = iris), modules = mods))
})

testthat::test_that("init modules accepts a teal_module object", {
  mods <- example_module()
  testthat::expect_no_error(init(data = teal.data::teal_data(iris = iris), modules = mods))
})

# filter ----
testthat::test_that("init filter accepts `teal_slices`", {
  fs <- teal.slice::teal_slices(
    teal.slice::teal_slice(dataname = "iris", varname = "species", selected = "setosa")
  )
  testthat::expect_no_error(
    init(data = teal.data::teal_data(iris = iris), modules = modules(example_module()), filter = fs)
  )
  testthat::expect_error(
    init(data = teal.data::teal_data(iris = iris), modules = modules(example_module()), filter = unclass(fs)),
    "Assertion on 'filter' failed"
  )
})

# data + modules ----
testthat::test_that("init throws when an empty `data` is used", {
  testthat::expect_error(
    init(data = teal.data::teal_data(), modules = list(example_module())),
    "The environment of `data` is empty."
  )
})

testthat::test_that("init throws warning when datanames in modules incompatible w/ datanames in data", {
  testthat::local_mocked_bindings(log_warn = warning, .package = "logger")

  testthat::expect_warning(
    init(
      data = teal.data::teal_data(mtcars = mtcars),
      modules = list(example_module(datanames = "iris"))
    ),
    "Dataset \"iris\" is missing for tab 'example teal module'. Dataset available in data: \"mtcars\"."
  )
})

testthat::test_that("init throws when dataname in filter incompatible w/ datanames in data", {
  testthat::expect_warning(
    init(
      data = teal.data::teal_data(mtcars = mtcars),
      modules = modules(example_module()),
      filter = teal_slices(teal_slice(dataname = "iris", varname = "Species"))
    ),
    "Filter 'iris Species' refers to dataname not available in 'data'"
  )
})

testthat::test_that("init creates a lockfile during the execution", {
  renv_file_name <- "teal_app.lock"
  withr::defer(file.remove(renv_file_name))
  app <- init(
    data = teal.data::teal_data(iris = iris),
    modules = example_module(label = "example teal module")
  )

  iter <- 1
  while (!file.exists(renv_file_name) && iter <= 100) {
    Sys.sleep(0.25)
    iter <- iter + 1 # max wait time is 25 seconds
  }

  testthat::expect_true(file.exists(renv_file_name))
})
