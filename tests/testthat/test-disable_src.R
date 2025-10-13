testthat::describe("srv_teal teal_modules disable_src", {
  testthat::it("doesn't break module", {
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
  testthat::it("doesn't break multiple modules", {
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
})
