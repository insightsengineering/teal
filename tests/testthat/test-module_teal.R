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
  ),
  add_dataset = teal_data_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          new_data <- within(data(), {
            new_dataset <- data.frame(a = 1:3, b = 4:6)
          })
          teal.data::datanames(new_data) <- c(teal.data::datanames(new_data), "new_dataset")
          new_data
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
        testthat::expect_error(data_rv())
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
        testthat::expect_error(data_rv())
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
        testthat::expect_error(data_rv())
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

  testthat::it("receives data with datasets == module$datanames", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(
          module("module_1", server = function(id, data) data, datanames = c("iris"))
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1()()[["mtcars"]])
        testthat::expect_identical(modules_output$module_1()()[["iris"]], iris)
      }
    )
  })

  testthat::it("is called and receives data if datanames in `teal_data` are not sufficient", {
    data <- teal_data(iris = iris, mtcars = mtcars)
    teal.data::datanames(data) <- "iris"

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(data),
        modules = modules(
          module("module_1", server = function(id, data) data)
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["iris_raw"]], iris)
      }
    )
  })

  testthat::it("doesn't receive extra data added in a transform", {
    testthat::skip("Looks like it does receive extra data?")
    # Added swiss in a transformer and this is added to data()
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformers = list(
              teal_data_module(
                label = "Dummy",
                ui = function(id) div("(does nothing)"),
                server = function(id, data) {
                  moduleServer(id, function(input, output, session) {
                    reactive(within(data(), {
                      swiss <- swiss
                    }))
                  })
                }
              )
            )
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], mtcars)
        testthat::expect_identical(modules_output$module_1()()[["iris"]], iris)
        testthat::expect_identical(modules_output$module_1()()[["swiss"]], swiss)
      }
    )
  })

  testthat::it("receives all data when module$datanames = \"all\"", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(teal_data(iris = iris, mtcars = mtcars, swiss = swiss)),
        modules = modules(
          module("module_1", server = function(id, data) data, datanames = "all")
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], mtcars)
        testthat::expect_identical(modules_output$module_1()()[["iris"]], iris)
        testthat::expect_identical(modules_output$module_1()()[["swiss"]], swiss)
      }
    )
  })

  testthat::it("receives parent data when module$datanames limited to a child data but join keys are provided", {
    parent <- data.frame(id = 1:3, test = letters[1:3])
    child <- data.frame(id = 1:9, parent_id = rep(1:3, each = 3), test2 = letters[1:9])
    data <- teal_data(parent = parent, child = child)
    teal.data::join_keys(data) <- teal.data::join_keys(
      teal.data::join_key("parent", "child", c(id = "parent_id"))
    )

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(data),
        modules = modules(
          module("module_1", server = function(id, data) data, datanames = "child")
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["parent"]], parent)
        testthat::expect_identical(modules_output$module_1()()[["child"]], child)
      }
    )
  })

  testthat::it("srv_teal_module.teal_module does not pass data if not in the args explicitly", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module("module_1", server = function(id, ...) {
            list(...)$data
          })
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        session$flushReact()
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("srv_teal_module.teal_module passes (deprecated) datasets to the server module", {
    testthat::expect_warning(
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris, mtcars = mtcars),
          modules = modules(
            module("module_1", server = function(id, datasets) datasets)
          )
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          testthat::expect_s3_class(modules_output$module_1(), "FilteredData")
        }
      ),
      "`datasets` argument in the server is deprecated and will be removed in the next release"
    )
  })

  testthat::it("srv_teal_module.teal_module passes server_args to the ...", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module(
            "module_1",
            server = function(id, data, ...) {
              data
            },
            server_args = list(x = 1L, y = 2L)
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(
          modules$children$module_1$server_args,
          list(x = 1L, y = 2L)
        )
      }
    )
  })

  testthat::it("srv_teal_module.teal_module passes filter_panel_api if specified", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module("module_1", server = function(id, filter_panel_api) filter_panel_api)
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_s3_class(modules_output$module_1(), "FilterPanelAPI")
      }
    )
  })

  testthat::it("srv_teal_module.teal_module passes Reporter if specified", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module("module_1", server = function(id, reporter) reporter)
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_s3_class(modules_output$module_1(), "Reporter")
      }
    )
  })
})

testthat::describe("srv_teal filters", {
  testthat::it("slices_global is initialized with slices specified in filter", {
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

  testthat::it("attr(slices_global, 'mapping')$global_filters is resolved to modules when !module_specific", {
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

  testthat::it("attr(slices_global, 'mapping')$global_filters is resolved  to modules when module_specific", {
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
        data = reactive(within(teal.data::teal_data(), {
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

  testthat::it("modules receive reactive data based on the changes in existing filter", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), {
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

  testthat::it("modules receive reactive data based on old and added filter", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), mtcars <- mtcars)),
        filter = teal_slices(
          teal_slice(dataname = "mtcars", varname = "cyl", selected = 4)
        ),
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        old_slices <- slices_global()
        new_slices <- teal_slices(teal_slice(dataname = "mtcars", varname = "mpg", selected = c(25, Inf)))
        slices_global(c(old_slices, new_slices))
        session$flushReact()
        expected_mtcars <- subset(mtcars, cyl == 4 & mpg >= 25)
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], expected_mtcars)
        testthat::expect_identical(modules_output$module_1()()[["mtcars_raw"]], mtcars)

        expected_code <- paste0(
          c(
            "mtcars <- mtcars",
            sprintf('stopifnot(rlang::hash(mtcars) == "%s")', rlang::hash(mtcars)),
            "mtcars_raw <- mtcars",
            "",
            "mtcars <- dplyr::filter(mtcars, cyl == 4 & (mpg >= 25 & mpg <= 34))"
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
        data = reactive(within(teal.data::teal_data(), {
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

  testthat::it("added slice to slices_global is applied to module only mapping is set", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), {
          iris <- iris
          mtcars <- mtcars
        })),
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        new_slices <- teal_slices(
          teal_slice(dataname = "mtcars", varname = "cyl", selected = "4"),
          mapping = list()
        )
        slices_global(new_slices)
        session$flushReact()
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], mtcars)

        attr(new_slices, "mapping")$module_1 <- c("mtcars cyl")
        slices_global(new_slices)
        session$flushReact()
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], subset(mtcars, cyl == 4))
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
        data = reactive(within(teal.data::teal_data(), {
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
        data = reactive(within(teal.data::teal_data(), {
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
        data = reactive(within(teal.data::teal_data(), {
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
    data <- teal.data::teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    teal.data::join_keys(data) <- teal.data::join_keys(
      teal.data::join_key("a", "b", keys = "id")
    )
    teal.data::datanames(data) <- c("a", "b")

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
    data <- teal.data::teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    teal.data::join_keys(data) <- teal.data::join_keys(
      teal.data::join_key("a", keys = "id"),
      teal.data::join_key("b", keys = c("id", "id2"))
    )
    teal.data::datanames(data) <- c("a", "b")

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
    data <- teal.data::teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    teal.data::join_keys(data) <- teal.data::join_keys(
      teal.data::join_key("a", keys = "id"),
      teal.data::join_key("b", keys = c("id", "id2")),
      teal.data::join_key("a", "b", keys = "id")
    )
    teal.data::datanames(data) <- c("a", "b")

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
    data <- teal.data::teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    teal.data::join_keys(data) <- teal.data::join_keys(
      teal.data::join_key("a", keys = "id"),
      teal.data::join_key("b", keys = c("id", "id2")),
      teal.data::join_key("a", "b", keys = "id")
    )
    teal.data::datanames(data) <- c("a", "b")

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
    data <- teal.data::teal_data(
      a = data.frame(id = seq(3), name = letters[seq(3)]),
      b = data.frame(id = rep(seq(3), 2), id2 = seq(6), value = letters[seq(6)])
    )
    teal.data::join_keys(data) <- teal.data::join_keys(
      teal.data::join_key("a", keys = "id"),
      teal.data::join_key("b", keys = c("id", "id2")),
      teal.data::join_key("a", "b", keys = "id")
    )
    teal.data::datanames(data) <- c("a", "b")

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

  testthat::it("reflects transform adding new dataset", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          module(
            "module_1",
            server = function(id, data) data,
            transformers = transform_list["add_dataset"]
          )
        )
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_output_table(output, "module_1"),
          data.frame(
            "Data Name" = c("iris", "new_dataset"),
            Obs = c("150/150", "3"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("reflects transform filtering", {
    testthat::it("displays parent's Subjects with count based on primary key", {
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris),
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
    data <- teal.data::teal_data(iris = iris, mtcars = mtcars)
    teal.data::datanames(data) <- c("iris", "mtcars")

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
    data <- teal.data::teal_data(iris = iris, mtcars = mtcars)
    teal.data::datanames(data) <- c("iris", "mtcars")

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

testthat::describe("srv_teal filter manager", {
  testthat::describe("mapping table reflects initial filters state", {
    testthat::it("returns empty mapping table if no filters set", {
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris, mtcars = mtcars),
          modules = modules(module("module_1", server = function(id, data) data))
        ),
        expr = {
          session$setInputs("teal_modules-active_tab" = "module_1")
          session$flushReact()
          testthat::expect_equal(
            mapping_table(),
            data.frame(
              `Global filters` = logical(0),
              row.names = integer(0),
              check.names = FALSE
            )
          )
        }
      )
    })
    testthat::it("returns table with global filters if some filters are set", {
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris, mtcars = mtcars),
          modules = modules(module("module_1", server = function(id, data) data)),
          filter = teal_slices(
            teal_slice("iris", "Species"),
            teal_slice("mtcars", "cyl")
          )
        ),
        expr = {
          session$setInputs("teal_modules-active_tab" = "module_1")
          session$flushReact()
          testthat::expect_identical(
            mapping_table(),
            data.frame(
              `Global filters` = c(TRUE, TRUE),
              row.names = c("iris Species", "mtcars cyl"),
              check.names = FALSE
            )
          )
        }
      )
    })
    testthat::it("returns table reflecting filters active in the modules when module_specific")
    testthat::it("returns table changes when slices global changes")
  })

  testthat::describe("mapping table reflects changes in a module's FilteredData", {
    # use session$userData$module_slices_api[[<module_id>]]$set_filter_state() to interact directly with
    #   module's FilteredData
    testthat::it("addition in module's slice appends a new filter and activates to this module when module_specific")
    testthat::it("removal in the module's slice deactivates a filter to this module only when module_specific")
    testthat::it("change in the module slice adds a new filter to all modules when !module_specific")
    testthat::it("removal in the module's slice deactivates a filter to all modules when !module_specific")
  })
})

testthat::describe("srv_teal snapshot manager", {
  testthat::it("clicking reset button restores initial filters state", {
    initial_slices <- teal_slices(
      teal_slice("iris", "Species"),
      teal_slice("mtcars", "cyl")
    )
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(module("module_1", server = function(id, data) data)),
        filter = initial_slices
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        slices_global(teal_slices())
        session$setInputs("snapshot_manager_panel-module-snapshot_reset" = TRUE)
        testthat::expect_identical(as.list(slices_global()), as.list(initial_slices))
      }
    )
  })
  # WIP more
})
