iris_ds <- Dataset$new("iris", head(iris))
mtcars_ds <- Dataset$new("mtcars", head(mtcars))
data <- teal_data(iris_ds, mtcars_ds)

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

testthat::test_that("srv_teal fails when raw_data is not reactive", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        raw_data = data,
        modules = root_modules(test_module1)
      ),
      expr = NULL
    ),
    regexp = "is.reactive\\(raw_data\\)"
  )
})

testthat::test_that("srv_teal initialized FilteredData based on the raw_data input", {
  filtered_data <- filtered_data_new(data)
  filtered_data_set(data, filtered_data)

  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = root_modules(test_module1)
    ),
    expr = {
      expect_identical(datasets_reactive()$datanames(), filtered_data$datanames())
      expect_identical(datasets_reactive()$get_data("iris"), filtered_data$get_data(dataname = "iris"))
      expect_identical(datasets_reactive()$get_data("mtcars"), filtered_data$get_data(dataname = "mtcars"))
    }
  )
})

testthat::test_that("srv_teal applies initial filter settings to initialized FilteredData", {
  filtered_data <- filtered_data_new(data)
  filtered_data_set(data, filtered_data)

  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = root_modules(test_module1),
      filter = list(iris = list(Species = "setosa"), mtcars = list(mpg = c(15, 20)))
    ),
    expr = {
      expect_identical(
        datasets_reactive()$get_data("iris", filtered = TRUE),
        subset(filtered_data$get_data(dataname = "iris"), Species == "setosa")
      )
      expect_identical(
        datasets_reactive()$get_data("mpg", filtered = TRUE),
        subset(filtered_data$get_data(dataname = "mtcars"), mpg >= 15 & mpg <= 20)
      )
    }
  )
})
