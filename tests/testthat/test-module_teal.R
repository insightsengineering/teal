iris_ds <- teal.data::dataset(dataname = "iris", x = iris)
mtcars_ds <- teal.data::dataset(dataname = "mtcars", x = mtcars)
data <- teal_data(iris_ds, mtcars_ds)

test_module1 <- module(
  label = "iris_tab",
  filters = "iris"
)
test_module2 <- module(
  label = "mtcars_tab",
  filters = "mtcars"
)

testthat::test_that("srv_teal fails when raw_data is not reactive", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        raw_data = data,
        modules = modules(test_module1)
      ),
      expr = NULL
    ),
    regexp = "is.reactive\\(raw_data\\)"
  )
})

testthat::test_that("srv_teal initializes the data when raw_data changes", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(NULL),
      modules = modules(test_module1)
    ),
    expr = {
      testthat::expect_null(datasets_reactive())
      raw_data(data)
      testthat::expect_named(datasets_reactive(), "iris_tab")
    }
  )
})

testthat::test_that("srv_teal initialized FilteredData based on the raw_data input", {
  filtered_data <- teal.slice::init_filtered_data(data)

  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = modules(test_module1, test_module2)
    ),
    expr = {
      # todo: test creation of filtered data based on modules and filters
    }
  )
})
