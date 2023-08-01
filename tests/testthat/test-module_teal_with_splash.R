iris_ds <- teal.data::dataset(dataname = "iris", x = head(iris))
mtcars_ds <- teal.data::dataset(dataname = "mtcars", x = head(mtcars))
data <- teal_data(iris_ds, mtcars_ds)

test_module1 <- module(
  label = "iris_tab",
  datanames = "iris"
)

testthat::test_that("srv_teal_with_splash creates reactiveVal returning data input", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = data,
      modules = modules(test_module1)
    ),
    expr = {
      testthat::expect_is(raw_data, "reactiveVal")
      testthat::expect_identical(raw_data(), data)
    }
  )
})

testthat::test_that("srv_teal_with_splash creates raw_data based on DDL returns NULL before loading", {
  x <- dataset_connector(dataname = "test_dataset", pull_callable = callable_code("iris"))
  delayed_data <- teal_data(x)
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = delayed_data,
      modules = modules(test_module1)
    ),
    expr = testthat::expect_null(raw_data())
  )
})

testthat::test_that("srv_teal_with_splash creates raw_data based on DDL returns pulled data when loaded", {
  teal.logger::suppress_logs()
  x <- dataset_connector(dataname = "iris", pull_callable = callable_code("iris"))
  delayed_data <- teal_data(x)
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = delayed_data,
      modules = modules(test_module1)
    ),
    expr = {
      testthat::expect_null(raw_data())
      session$setInputs(`startapp_module-submit` = TRUE) # DDL has independent session id (without ns)
      testthat::expect_is(raw_data(), "TealData")
      testthat::expect_identical(raw_data()$get_dataset("iris")$get_raw_data(), iris)
    }
  )
})

testthat::test_that("srv_teal_with_splash gets observe event from srv_teal", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = data,
      modules = modules(test_module1)
    ),
    expr = {
      testthat::expect_is(res, "Observer")
    }
  )
})
