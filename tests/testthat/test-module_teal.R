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

testthat::test_that("srv_teal initialized data list structure reflects modules", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = modules(test_module1, modules(label = "tab", test_module1, test_module2))
    ),
    expr = {
      raw_data(data)
      testthat::expect_named(datasets_reactive(), c("iris_tab", "tab"))
      testthat::expect_named(datasets_reactive()$tab, c("iris_tab", "mtcars_tab"))
    }
  )
})

testthat::test_that("srv_teal initialized data containing same FilteredData when the filter is global", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = modules(test_module1, modules(label = "tab", test_module1, test_module2)),
      filter = module_slices(module_specific = FALSE)
    ),
    expr = {
      raw_data(data)
      unlisted_fd <- unlist(datasets_reactive(), use.names = FALSE)
      testthat::expect_identical(unlisted_fd[[1]], unlisted_fd[[2]])
      testthat::expect_identical(unlisted_fd[[2]], unlisted_fd[[3]])
    }
  )
})

testthat::test_that("srv_teal initialized data containing different FilteredData when the filter is module_specific", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = modules(test_module1, modules(label = "tab", test_module1, test_module2)),
      filter = module_slices(module_specific = TRUE)
    ),
    expr = {
      raw_data(data)
      unlisted_fd <- unlist(datasets_reactive(), use.names = FALSE)
      testthat::expect_false(identical(unlisted_fd[[1]], unlisted_fd[[2]]))
      testthat::expect_false(identical(unlisted_fd[[2]], unlisted_fd[[3]]))
    }
  )
})
