filtered_data <- teal.slice:::FilteredData$new()
filtered_data$set_dataset(teal.data::dataset(dataname = "iris", x = head(iris)))
filtered_data$set_dataset(teal.data::dataset(dataname = "mtcars", x = head(mtcars)))

test_module1 <- module(
  label = "iris tab",
  ui = function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) NULL),
  filters = "iris"
)
test_module2 <- module(
  label = "mtcars tab",
  ui = function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) NULL),
  filters = "mtcars"
)

testthat::test_that("srv_tabs_with_filters throws error if reporter is not of class Reporter", {
  testthat::expect_error(
    srv_tabs_with_filters(id, datasets = filtered_data, modules = modules(test_module1), reporter = list()),
    "Assertion on 'reporter' failed"
  )
})

testthat::test_that("active_datanames() returns dataname from single tab", {
  shiny::testServer(
    app = srv_tabs_with_filters,
    args = list(
      id = "test",
      datasets = filtered_data,
      modules = modules(test_module1),
      filter = list(),
      reporter = teal.reporter::Reporter$new()
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
      datasets = filtered_data,
      modules = modules(test_module1, test_module2),
      filter = list(),
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
