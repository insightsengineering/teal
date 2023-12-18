teal_data <- teal.data::teal_data()
teal_data <- within(teal_data, iris <- head(iris))
teal_data <- within(teal_data, mtcars <- head(mtcars))
datanames(teal_data) <- c("iris", "mtcars")
filtered_data <- teal_data_to_filtered_data(teal_data)

test_module1 <- module(
  label = "iris tab",
  server = function(id, data, ...) {
    moduleServer(id, function(input, output, session) {
    })
  },
  datanames = "iris"
)
test_module2 <- module(
  label = "mtcars tab",
  server = function(id, data, ...) {
    moduleServer(id, function(input, output, session) {
    })
  },
  datanames = "mtcars"
)

testthat::test_that("srv_tabs_with_filters throws error if reporter is not of class Reporter", {
  testthat::expect_error(
    srv_tabs_with_filters(
      id,
      datasets = list(`iris tab` = filtered_data),
      modules = modules(test_module1),
      reporter = list()
    ),
    "Assertion on 'reporter' failed"
  )
})

testthat::test_that("active_module() returns module specs from active tab when filter.module_specific = FALSE", {
  shiny::testServer(
    app = srv_tabs_with_filters,
    args = list(
      id = "test",
      datasets = list(`iris tab` = filtered_data, `mtcars tab` = filtered_data),
      modules = modules(test_module1, test_module2),
      filter = teal_slices(module_specific = FALSE),
      reporter = teal.reporter::Reporter$new()
    ),
    expr = {
      session$setInputs(`root-active_tab` = "iris_tab")
      testthat::expect_identical(active_module(), test_module1)
      session$setInputs(`root-active_tab` = "mtcars_tab")
      testthat::expect_identical(active_module(), test_module2)
    }
  )
})

testthat::test_that("srv_tabs_with_filters throws error if reporter is not of class Reporter", {
  testthat::expect_error(
    srv_tabs_with_filters(
      id,
      datasets = list(`iris tab` = filtered_data),
      modules = modules(test_module1),
      reporter = list()
    ),
    "Assertion on 'reporter' failed"
  )
})

testthat::test_that("active_datanames() returns dataname from single tab", {
  shiny::testServer(
    app = srv_tabs_with_filters,
    args = list(
      id = "test",
      datasets = list(`iris tab` = filtered_data),
      modules = modules(test_module1),
      filter = teal_slices()
    ),
    expr = {
      testthat::expect_identical(active_datanames(), "iris")
    }
  )
})

testthat::test_that("active_datanames() returns dataname from active tab after change", {
  shiny::testServer(
    app = srv_tabs_with_filters,
    args = list(
      id = "test",
      datasets = list(`iris tab` = filtered_data, `mtcars tab` = filtered_data),
      modules = modules(test_module1, test_module2),
      filter = teal_slices(),
      reporter = teal.reporter::Reporter$new()
    ),
    expr = {
      testthat::expect_error(active_datanames()) # to trigger active_module
      session$setInputs(`root-active_tab` = "iris_tab")
      testthat::expect_identical(active_datanames(), "iris")
      session$setInputs(`root-active_tab` = "mtcars_tab")
      testthat::expect_identical(active_datanames(), "mtcars")
    }
  )
})
