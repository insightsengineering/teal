filtered_data <- FilteredData$new()
filtered_data$set_dataset(Dataset$new("iris", head(iris)))
filtered_data$set_dataset(Dataset$new("mtcars", head(mtcars)))

test_module1 <- module(
  label = "iris_tab",
  ui =  function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) NULL),
  filters = "iris"
)
test_module2 <- module(
  label = "mtcars_tab",
  ui =  function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) NULL),
  filters = "mtcars"
)

testthat::test_that("active_datanames() returns dataname from single tab", {
  shiny::testServer(
    app = srv_tabs_with_filters,
    args = list(
      id = "test",
      datasets = filtered_data,
      modules = root_modules(test_module1)
    ),
    expr = {
      testthat::expect_error(active_datanames()) # to trigger active_module
      session$setInputs(`modules_ui-root` = "iris_tab")
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
      modules = root_modules(test_module1, test_module2)
    ),
    expr = {
      testthat::expect_error(active_datanames()) # to trigger active_module
      session$setInputs(`modules_ui-root` = "iris_tab")
      testthat::expect_identical(active_datanames(), "iris")
      session$setInputs(`modules_ui-root` = "mtcars_tab")
      testthat::expect_identical(active_datanames(), "mtcars")
    }
  )
})

