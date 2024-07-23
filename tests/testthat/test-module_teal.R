# comment: srv_teal is exported so the tests here are extensive and cover srv_data as well.
#          testing of srv_data is not needed.
module_output_table <- function(output, id) {
  table_id <- sprintf("teal_modules-%s-data_summary-table", id)
  html <- output[[table_id]]$html
  as.data.frame(rvest::html_table(rvest::read_html(html), header = TRUE)[[1]])
}

transform_list <- list(
  fail = teal_data_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          stop("oh no!")
        })
      })
    }
  ),
  iris = teal_data_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          within(data(), iris <- head(iris))
        })
      })
    }
  ),
  mtcars = teal_data_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          within(data(), mtcars <- head(mtcars))
        })
      })
    }
  )
)

testthat::describe("srv_teal arguments", {
  testthat::it("accepts data to be teal_data", {
    testthat::expect_no_error(
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris),
          modules = modules(example_module())
        ),
        expr = NULL
      )
    )
  })

  testthat::it("accepts data to be teal_data_module returning reactive teal_data", {
    testthat::expect_no_error(
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal_data_module(ui = function(id) NULL, server = function(id) reactive(teal_data(iris = iris))),
          modules = modules(example_module())
        ),
        expr = NULL
      )
    )
  })

  testthat::it("accepts data to a reactive or reactiveVal teal_data", {
    testthat::expect_no_error(
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = reactive(teal_data(iris = iris)),
          modules = modules(example_module())
        ),
        expr = NULL
      )
    )

    reactive_val <- reactiveVal(teal_data(iris = iris))
    testthat::expect_no_error(
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = reactive_val,
          modules = modules(example_module())
        ),
        expr = NULL
      )
    )
  })

  testthat::it("fails when data is not teal_data or teal_data_module", {
    testthat::expect_error(
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = data.frame(),
          modules = modules(example_module())
        ),
        expr = NULL
      ),
      "Must inherit from class 'teal_data'/'teal_data_module'/'reactive'/'reactiveVal'"
    )
  })

  testthat::it("app fails when teal_data_module doesn't return a reactive", {
    testthat::expect_error(
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal_data_module(ui = function(id) NULL, server = function(id) teal_data(iris = iris)),
          modules = modules(example_module())
        ),
        expr = {
          session$flushReact()
        }
      ),
      "The `teal_data_module` passed to `data` must return a reactive expressio"
    )
  })
})


# teal_module --------
testthat::describe("srv_teal teal_modules", {
  testthat::it("are not called by default", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(teal_data(iris = iris)),
        modules = modules(
          module("module_1", server = function(id, data) 101L),
          module("module_2", server = function(id, data) 102L)
        )
      ),
      expr = {
        testthat::expect_s4_class(data_rv(), "teal_data")
        testthat::expect_null(modules_output$module_1())
        testthat::expect_null(modules_output$module_2())
      }
    )
  })

  testthat::it("are called once their tab is selected and data is `teal_data`", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data(iris = iris),
        modules = modules(
          module("module_1", server = function(id, data) 101L),
          module("module_2", server = function(id, data) 102L)
        )
      ),
      expr = {
        testthat::expect_s4_class(data_rv(), "teal_data")
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1(), 101L)
        testthat::expect_null(modules_output$module_2())

        session$setInputs(`teal_modules-active_tab` = "module_2")
        testthat::expect_identical(modules_output$module_1(), 101L)
        testthat::expect_identical(modules_output$module_2(), 102L)
      }
    )
  })

  testthat::it("are called once their tab is selected and data returns reactive `teal_data`", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(teal_data(iris = iris)),
        modules = modules(
          module("module_1", server = function(id, data) 101L),
          module("module_2", server = function(id, data) 102L)
        )
      ),
      expr = {
        testthat::expect_s4_class(data_rv(), "teal_data")
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1(), 101L)
        testthat::expect_null(modules_output$module_2())

        session$setInputs(`teal_modules-active_tab` = "module_2")
        testthat::expect_identical(modules_output$module_1(), 101L)
        testthat::expect_identical(modules_output$module_2(), 102L)
      }
    )
  })

  testthat::it("are called once their tab is selected and teal_data_module returns reactive `teal_data`", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              reactive(teal_data(iris = iris))
            })
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) 101L),
          module("module_2", server = function(id, data) 102L)
        )
      ),
      expr = {
        testthat::expect_s4_class(data_rv(), "teal_data")
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1(), 101L)
        testthat::expect_null(modules_output$module_2())

        session$setInputs(`teal_modules-active_tab` = "module_2")
        testthat::expect_identical(modules_output$module_1(), 101L)
        testthat::expect_identical(modules_output$module_2(), 102L)
      }
    )
  })

  testthat::it("are called with data argument being `teal_data`", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data(iris = iris),
        modules = modules(
          module("module_1", server = function(id, data) data)
        )
      ),
      expr = {
        testthat::expect_s4_class(data_rv(), "teal_data")
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_s4_class(modules_output$module_1()(), "teal_data")
      }
    )
  })

  testthat::it("are not called when the teal_data_module doesn't return teal_data", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              reactive("my error")
            })
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) 101L),
          module("module_2", server = function(id, data) 102L)
        )
      ),
      expr = {
        testthat::expect_null(modules_output$module_1())
        testthat::expect_null(data_rv())
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("are not called when the teal_data_module returns validation error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              reactive(validate(need(FALSE, "my error")))
            })
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) 101L),
          module("module_2", server = function(id, data) 102L)
        )
      ),
      expr = {
        testthat::expect_null(modules_output$module_1())
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("are not called when the teal_data_module throw en error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              reactive(validate(need(FALSE, "my error")))
            })
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) 101L),
          module("module_2", server = function(id, data) 102L)
        )
      ),
      expr = {
        testthat::expect_null(modules_output$module_1())
        testthat::expect_null(data_rv())
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("are not called when the teal_data_module returns qenv.error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              reactive(within(teal_data(), stop("my qenv error")))
            })
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) 101L),
          module("module_2", server = function(id, data) 102L)
        )
      ),
      expr = {
        testthat::expect_null(modules_output$module_1())
        testthat::expect_null(data_rv())
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("are receiving reactive data which triggers on change", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              eventReactive(input$dataset, {
                if (input$dataset == "iris") {
                  teal_data(iris = iris)
                } else if (input$dataset == "mtcars") {
                  teal_data(mtcars = mtcars)
                }
              })
            })
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) data),
          module("module_2", server = function(id, data) data)
        )
      ),
      expr = {
        testthat::expect_null(modules_output$module_1())

        session$setInputs(`data-teal_data_module-data-dataset` = "iris", `teal_modules-active_tab` = "module_1")
        testthat::expect_identical(
          ls(teal.code::get_env(modules_output$module_1()())),
          c("iris", "iris_raw")
        )

        # comment: can't trigger reactivity in testServer - the change in a reactive input data
        #          is not propagated to the teal_module(data). Instead we test if the modified data
        #          is sent to another teal_module
        session$setInputs(`data-teal_data_module-data-dataset` = "mtcars", `teal_modules-active_tab` = "module_2")
        session$flushReact()
        testthat::expect_identical(
          ls(teal.code::get_env(modules_output$module_2()())),
          c("mtcars", "mtcars_raw")
        )
      }
    )
  })

  testthat::it("are not called again when data changes", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              eventReactive(input$dataset, {
                if (input$dataset == "iris") {
                  teal_data(iris = iris)
                } else if (input$dataset == "mtcars") {
                  teal_data(mtcars = mtcars)
                }
              })
            })
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) runif(1))
        )
      ),
      expr = {
        testthat::expect_null(modules_output$module_1())
        session$setInputs(
          `data-teal_data_module-data-dataset` = "iris",
          `teal_modules-active_tab` = "module_1"
        )
        out <- modules_output$module_1()
        testthat::expect_true(!is.null(out))
        session$setInputs(`data-teal_data_module-data-dataset` = "mtcars")
        testthat::expect_identical(out, modules_output$module_1())
      }
    )
  })

  testthat::test_that("receives data with datasets == module$datanames")

  testthat::it("is called and receives data if datanames in `teal_data` are not sufficient")

  testthat::test_that("doesn't receive extra data added in a transform")

  testthat::test_that("receives all data when module$datanames = \"all\"")

  testthat::test_that("srv_teal_module.teal_module does not pass data if not in the args explicitly")

  testthat::test_that("srv_teal_module.teal_module passes (deprecated) datasets to the server module")

  testthat::test_that("srv_teal_module.teal_module passes server_args to the ...")

  testthat::test_that("srv_teal_module.teal_module passes filter_panel_api if specified")

  testthat::test_that("srv_teal_module.teal_module passes Reporter if specified")
})

testthat::describe("srv_teal filters", {
  testthat::it("srv_teal: slices_global is initialized with slices specified in filter", {
    init_filter <- teal_slices(
      teal_slice("iris", "Species"),
      teal_slice("mtcars", "cyl")
    )
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(example_module()),
        filter = init_filter
      ),
      expr = {
        testthat::expect_length(setdiff_teal_slices(slices_global(), init_filter), 0)
        testthat::expect_length(setdiff_teal_slices(init_filter, slices_global()), 0)
      }
    )
  })

  testthat::it("srv_teal: attr(slices_global, 'mapping') is resolved for global_filters  when !module_specific", {
    init_filter <- teal_slices(
      teal_slice("iris", "Species"),
      teal_slice("mtcars", "cyl"),
      mapping = list(
        global_filters = c("iris Species", "mtcars cyl")
      )
    )
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(example_module(label = "module-1"), example_module(label = "module-2")),
        filter = init_filter
      ),
      expr = {
        testthat::expect_identical(
          attr(slices_global(), "mapping"),
          list(
            `module-1` = c("iris Species", "mtcars cyl"),
            `module-2` = c("iris Species", "mtcars cyl")
          )
        )
      }
    )
  })

  testthat::it("srv_teal: attr(slices_global, 'mapping') is resolved for global_filters  when module_specific", {
    init_filter <- teal_slices(
      teal_slice("iris", "Species"),
      teal_slice("mtcars", "cyl"),
      mapping = list(global_filters = c("iris Species", "mtcars cyl")),
      module_specific = TRUE
    )
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(example_module(label = "module-1"), example_module(label = "module-2")),
        filter = init_filter
      ),
      expr = {
        testthat::expect_identical(
          attr(slices_global(), "mapping"),
          list(
            `module-1` = c("iris Species", "mtcars cyl"),
            `module-2` = c("iris Species", "mtcars cyl")
          )
        )
      }
    )
  })

  testthat::it("modules receive reactive data with filtered data, raw data and filter code", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal_data(), {
          iris <- iris
          mtcars <- mtcars
        })),
        filter = teal_slices(
          teal_slice(dataname = "iris", varname = "Species", selected = "versicolor"),
          teal_slice(dataname = "mtcars", varname = "cyl", selected = 6)
        ),
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")

        expected_iris <- subset(iris, Species == "versicolor")
        rownames(expected_iris) <- NULL
        testthat::expect_identical(modules_output$module_1()()[["iris"]], expected_iris)
        testthat::expect_identical(modules_output$module_1()()[["iris_raw"]], iris)

        expected_mtcars <- subset(mtcars, cyl == 6)
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], expected_mtcars)
        testthat::expect_identical(modules_output$module_1()()[["mtcars_raw"]], mtcars)

        expected_code <- paste0(
          c(
            "iris <- iris",
            "mtcars <- mtcars",
            "",
            sprintf('stopifnot(rlang::hash(iris) == "%s")', rlang::hash(iris)),
            sprintf('stopifnot(rlang::hash(mtcars) == "%s")', rlang::hash(mtcars)),
            "iris_raw <- iris",
            "mtcars_raw <- mtcars",
            "",
            'iris <- dplyr::filter(iris, Species == "versicolor")',
            "mtcars <- dplyr::filter(mtcars, cyl == 6)"
          ),
          collapse = "\n"
        )
        testthat::expect_identical(teal.code::get_code(modules_output$module_1()()), expected_code)
      }
    )
  })

  testthat::it("modules receive reactive data based on the changes in slices_global", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal_data(), {
          iris <- iris
          mtcars <- mtcars
        })),
        filter = teal_slices(
          teal_slice(dataname = "iris", varname = "Species", selected = "versicolor"),
          teal_slice(dataname = "mtcars", varname = "cyl", selected = 6)
        ),
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        slices_global(teal_slices(teal_slice(dataname = "mtcars", varname = "cyl", selected = "4")))
        session$flushReact()

        testthat::expect_identical(modules_output$module_1()()[["iris"]], iris)
        testthat::expect_identical(modules_output$module_1()()[["iris_raw"]], iris)

        expected_mtcars <- subset(mtcars, cyl == 4)
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], expected_mtcars)
        testthat::expect_identical(modules_output$module_1()()[["mtcars_raw"]], mtcars)

        expected_code <- paste0(
          c(
            "iris <- iris",
            "mtcars <- mtcars",
            "",
            sprintf('stopifnot(rlang::hash(iris) == "%s")', rlang::hash(iris)),
            sprintf('stopifnot(rlang::hash(mtcars) == "%s")', rlang::hash(mtcars)),
            "iris_raw <- iris",
            "mtcars_raw <- mtcars",
            "",
            "mtcars <- dplyr::filter(mtcars, cyl == 4)"
          ),
          collapse = "\n"
        )
        testthat::expect_identical(teal.code::get_code(modules_output$module_1()()), expected_code)
      }
    )
  })

  testthat::it("filter is applied to the teal_module's data only if the tab is selected", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal_data(), {
          iris <- iris
          mtcars <- mtcars
        })),
        modules = modules(
          module("module_1", server = function(id, data) data),
          module("module_2", server = function(id, data) data)
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        session$setInputs(`teal_modules-active_tab` = "module_2")
        slices_global(teal_slices(teal_slice(dataname = "mtcars", varname = "cyl", selected = 6)))
        session$flushReact()

        testthat::expect_identical(modules_output$module_2()()[["mtcars"]], subset(mtcars, cyl == 6))
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], mtcars)

        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], subset(mtcars, cyl == 6))
      }
    )
  })
})

testthat::describe("srv_teal teal_module(s) transformer", {
  testthat::it("evaluates custom qenv call and pass update teal_data to the module", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformers = transform_list[c("iris", "mtcars")]
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["iris"]], head(iris))
        testthat::expect_identical(modules_output$module_1()()[["iris_raw"]], iris)
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], head(mtcars))
        testthat::expect_identical(modules_output$module_1()()[["mtcars_raw"]], mtcars)
      }
    )
  })

  testthat::it("evaluates custom qenv call after filter is applied", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal_data(), {
          iris <- iris
          mtcars <- mtcars
        })),
        filter = teal_slices(
          teal_slice(dataname = "iris", varname = "Species", selected = "versicolor"),
          teal_slice(dataname = "mtcars", varname = "cyl", selected = 6)
        ),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformers = transform_list[c("iris", "mtcars")]
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")

        expected_iris <- subset(iris, Species == "versicolor")
        rownames(expected_iris) <- NULL
        expected_iris <- head(expected_iris)
        testthat::expect_identical(modules_output$module_1()()[["iris"]], expected_iris)
        testthat::expect_identical(modules_output$module_1()()[["iris_raw"]], iris)
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], head(subset(mtcars, cyl == 6)))
        testthat::expect_identical(modules_output$module_1()()[["mtcars_raw"]], mtcars)

        expected_code <- paste(collapse = "\n", c(
          "iris <- iris",
          "mtcars <- mtcars",
          "",
          sprintf('stopifnot(rlang::hash(iris) == "%s")', rlang::hash(iris)),
          sprintf('stopifnot(rlang::hash(mtcars) == "%s")', rlang::hash(mtcars)),
          "iris_raw <- iris",
          "mtcars_raw <- mtcars",
          "",
          'iris <- dplyr::filter(iris, Species == "versicolor")',
          "mtcars <- dplyr::filter(mtcars, cyl == 6)",
          "iris <- head(iris)",
          "mtcars <- head(mtcars)"
        ))
        testthat::expect_identical(
          teal.code::get_code(modules_output$module_1()()),
          expected_code
        )
      }
    )
  })

  testthat::it("is reactive to the filter changes", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal_data(), {
          iris <- iris
          mtcars <- mtcars
        })),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformers = transform_list[c("iris", "mtcars")]
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        slices_global(teal_slices(teal_slice(dataname = "mtcars", varname = "cyl", selected = "4")))
        session$flushReact()

        testthat::expect_identical(modules_output$module_1()()[["iris"]], head(iris))
        testthat::expect_identical(modules_output$module_1()()[["iris_raw"]], iris)
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], head(subset(mtcars, cyl == 4)))
        testthat::expect_identical(modules_output$module_1()()[["mtcars_raw"]], mtcars)

        expected_code <- paste(collapse = "\n", c(
          "iris <- iris",
          "mtcars <- mtcars",
          "",
          sprintf('stopifnot(rlang::hash(iris) == "%s")', rlang::hash(iris)),
          sprintf('stopifnot(rlang::hash(mtcars) == "%s")', rlang::hash(mtcars)),
          "iris_raw <- iris",
          "mtcars_raw <- mtcars",
          "",
          "mtcars <- dplyr::filter(mtcars, cyl == 4)",
          "iris <- head(iris)",
          "mtcars <- head(mtcars)"
        ))
        testthat::expect_identical(
          teal.code::get_code(modules_output$module_1()()),
          expected_code
        )
      }
    )
  })

  testthat::it("fails when transformer doesn't return reactive", {
    testthat::expect_error(
      testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris),
          modules = modules(
            module(
              server = function(id, data) data,
              transformers = list(
                teal_data_module(
                  ui = function(id) NULL,
                  server = function(id, data) "whatever"
                )
              )
            )
          )
        ),
        expr = {
        }
      ),
      "must return a reactive expression"
    )
  })

  testthat::it("continues when transformer throws validation error and returns unchanged data", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformers = list(
              teal_data_module(
                ui = function(id) NULL,
                server = function(id, data) {
                  reactive(validate(need(FALSE, "my error")))
                }
              )
            )
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["iris"]], iris)
        testthat::expect_identical(modules_output$module_1()()[["iris_raw"]], iris)
      }
    )
  })

  testthat::it("continues when transformer throws validation error and returns unchanged data", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformers = list(
              teal_data_module(
                ui = function(id) NULL,
                server = function(id, data) {
                  reactive(validate(need(FALSE, "my error")))
                }
              )
            )
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["iris"]], iris)
        testthat::expect_identical(modules_output$module_1()()[["iris_raw"]], iris)
      }
    )
  })

  testthat::it("continues when transformer throws qenv error and returns unchanged data")

  testthat::it("isn't called when `data` is not teal_data")
  # when reactive returned teal_data_module is not triggered (for example when button isn't clicked)
})

testthat::describe("srv_teal summary table", {
  testthat::it("displays Obs only column if all datasets have no join keys", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal_data(), {
          iris <- iris
          mtcars <- mtcars
        })),
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("iris", "mtcars"),
            Obs = c("150/150", "32/32"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("displays Subjects with count based on foreign key column", {
    data <- teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    join_keys(data) <- join_keys(
      join_key("a", "b", keys = "id")
    )
    datanames(data) <- c("a", "b")

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("a", "b"),
            Obs = c("3/3", "6/6"),
            Subjects = c("", "3/3"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("displays parent's Subjects with count based on primary key", {
    data <- teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    join_keys(data) <- join_keys(
      join_key("a", keys = "id"),
      join_key("b", keys = c("id", "id2"))
    )
    datanames(data) <- c("a", "b")

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("a", "b"),
            Obs = c("3/3", "6/6"),
            Subjects = c("3/3", "6/6"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("displays parent's Subjects with count based on primary and foreign key", {
    data <- teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    join_keys(data) <- join_keys(
      join_key("a", keys = "id"),
      join_key("b", keys = c("id", "id2")),
      join_key("a", "b", keys = "id")
    )
    datanames(data) <- c("a", "b")

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("a", "b"),
            Obs = c("3/3", "6/6"),
            Subjects = c("3/3", "3/3"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("reflects filters and displays subjects by their unique id count", {
    data <- teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    join_keys(data) <- join_keys(
      join_key("a", keys = "id"),
      join_key("b", keys = c("id", "id2")),
      join_key("a", "b", keys = "id")
    )
    datanames(data) <- c("a", "b")

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data)),
        filter = teal_slices(teal_slice("a", "name", selected = "a"))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("a", "b"),
            Obs = c("1/3", "2/6"),
            Subjects = c("1/3", "1/3"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("reflects added filters and displays subjects by their unique id count", {
    data <- teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    join_keys(data) <- join_keys(
      join_key("a", keys = "id"),
      join_key("b", keys = c("id", "id2")),
      join_key("a", "b", keys = "id")
    )
    datanames(data) <- c("a", "b")

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        slices_global(
          teal_slices(teal_slice("a", "name", selected = "a"))
        )
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("a", "b"),
            Obs = c("1/3", "2/6"),
            Subjects = c("1/3", "1/3"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("reflects transforms", {
    testthat::it("displays parent's Subjects with count based on primary key", {
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal_data(iris = iris),
          modules = modules(
            module(
              "module_1",
              server = function(id, data) data,
              transformers = transform_list["iris"]
            )
          )
        ),
        expr = {
          session$setInputs("teal_modules-active_tab" = "module_1")
          session$flushReact()
          testthat::expect_identical(
            module_output_table(output, "module_1"),
            data.frame(
              "Data Name" = c("iris"),
              Obs = c("6/150"),
              check.names = FALSE
            )
          )
        }
      )
    })
  })

  testthat::it("displays only module$datanames", {
    data <- teal_data(iris = iris, mtcars = mtcars)
    datanames(data) <- c("iris", "mtcars")

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data, datanames = "iris"))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("iris"),
            Obs = c("150/150"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("displays subset of module$datanames if not sufficient", {
    data <- teal_data(iris = iris, mtcars = mtcars)
    datanames(data) <- c("iris", "mtcars")

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data, datanames = c("iris", "iris2")))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("iris"),
            Obs = c("150/150"),
            check.names = FALSE
          )
        )
      }
    )
  })
})


# todo: if possible, test if the filters are set globally in slices_global (via srv_module_filter_manager)
