testthat::test_that("disable_src() doesn't break modules", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()) |> disable_src()
    ),
    expr = NULL
  )
})
testthat::test_that("disable_src() doesn't break multiple modules", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module(), example_module()) |> disable_src()
    ),
    expr = NULL
  )
})
