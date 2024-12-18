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
      app = srv_transform_teal_data,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(data1 = iris, data2 = mtcars)),
        transformators = output_decorator
      ),
      expr = {
        session$flushReact()
        testthat::expect_identical(module_output()[["data1"]], rev(iris))
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
      app = srv_transform_teal_data,
      args = list(
        id = "test",
        data = reactive(teal.data::teal_data(data1 = iris, data2 = mtcars)),
        transformators = output_decorator
      ),
      expr = {
        session$flushReact()
        testthat::expect_identical(module_output()[["data1"]], rev(iris))
      }
    )
  })
})

testthat::test_that(
  "ui_transform_teal_data and srv_transform_teal_data have the same namespace for transform module",
  {
    ttm <- teal_transform_module(
      ui = function(id) tags$div(id = NS(id, "a_div"), "a div"),
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          full_id <- session$ns("a_div")
          reactive(within(data(), id <- full_id, full_id = full_id))
        })
      }
    )

    initial_id <- "a-path-to-an-inner-namespace"
    ui <- ui_transform_teal_data(initial_id, ttm)
    # Find element that ends in "-a_div"
    expected_id <- unname(unlist(ui)[grepl(".*-a_div$", unlist(ui))][1])

    testServer(
      app = srv_transform_teal_data,
      args = list(
        id = initial_id,
        data = reactive(within(teal_data(), iris <- iris)),
        transformators = ttm
      ),
      expr = {
        session$flushReact()
        testthat::expect_equal(module_output()$id, expected_id)
      }
    )
  }
)
