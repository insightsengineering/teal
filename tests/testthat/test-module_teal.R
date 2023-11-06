testthat::test_that("srv_teal fails when raw_data is not reactive", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        raw_data = teal_data(iris = iris),
        modules = modules(example_module())
      ),
      expr = NULL
    ),
    regexp = "is.reactive\\(raw_data\\)"
  )
})

testthat::test_that("srv_teal when raw_data changes, datasets_reactive is initialized as list of FilteredData", {
  data <- teal_data(iris1 = iris, mtcars1 = mtcars)
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(NULL),
      modules = modules(
        example_module(label = "iris_tab"),
        example_module(label = "mtcars_tab")
      )
    ),
    expr = {
      raw_data(data)
      checkmate::expect_list(datasets_reactive(), types = "FilteredData")
    }
  )
})

testthat::test_that("srv_teal initialized datasets_reactive (list) reflects modules structure", {
  data <- teal_data(iris1 = iris, mtcars1 = mtcars)
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = modules(
        example_module("iris_tab"),
        modules(label = "tab", example_module("iris_tab"), example_module("mtcars_tab"))
      )
    ),
    expr = {
      raw_data(data)
      testthat::expect_named(datasets_reactive(), c("iris_tab", "tab"))
      testthat::expect_named(datasets_reactive()$tab, c("iris_tab", "mtcars_tab"))
    }
  )
})

testthat::test_that("srv_teal initialized data containing same FilteredData when the filter is global", {
  data <- teal_data(iris1 = iris, mtcars1 = mtcars)
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = modules(
        example_module("iris_tab"),
        modules(label = "tab", example_module("iris_tab"), example_module("mtcars_tab"))
      ),
      filter = teal_slices(module_specific = FALSE)
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
  data <- teal_data(iris1 = iris, mtcars1 = mtcars)
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      raw_data = reactiveVal(data),
      modules = modules(
        example_module("iris_tab"),
        modules(label = "tab", example_module("iris_tab"), example_module("mtcars_tab"))
      ),
      filter = teal_slices(module_specific = TRUE)
    ),
    expr = {
      raw_data(data)
      unlisted_fd <- unlist(datasets_reactive(), use.names = FALSE)
      testthat::expect_false(identical(unlisted_fd[[1]], unlisted_fd[[2]]))
      testthat::expect_false(identical(unlisted_fd[[2]], unlisted_fd[[3]]))
    }
  )
})
