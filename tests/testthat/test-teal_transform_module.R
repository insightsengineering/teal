testthat::describe("make_teal_transform_server produces a valid teal_transform_module", {
  testthat::it("expression", {
    label <- "output_decorator"
    output_decorator <- teal_transform_module(
      label = label,
      server = make_teal_transform_server(
        expression(data1 <- rev(data1))
      )
    )

    shiny::testServer(
      app = srv_teal_transform_data,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(data1 = iris, data2 = mtcars)),
        transformators = output_decorator
      ),
      expr = {
        data_out <- transformators[[label]]$server(label, data = data)
        testthat::expect_identical(data_out()[["data1"]], rev(iris))
      }
    )
  })

  testthat::it("quote", {
    label <- "output_decorator"
    output_decorator <- teal_transform_module(
      label = label,
      server = make_teal_transform_server(
        quote(data1 <- rev(data1))
      )
    )

    shiny::testServer(
      app = srv_teal_transform_data,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(data1 = iris, data2 = mtcars)),
        transformators = output_decorator
      ),
      expr = {
        data_out <- transformators[[label]]$server(label, data = data)
        testthat::expect_identical(data_out()[["data1"]], rev(iris))
      }
    )
  })
})
