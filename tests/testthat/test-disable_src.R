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
  testthat::it("Disable button on a module", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          example_module(label = "m1"),
          example_module(label = "m2")
        ) |> disable_src()
      ),
      expr = {
        browser()
        session

        "teal-teal_modules-nav-example_teal_module-source_code_wrapper-source_code_wrapper"
      }
    )
  })
  testthat::it("Disable button on nested modules", {
  })
  testthat::it("modifies server", {
  })
  testthat::it("changes data attribute", {
  })
})
