# comment: srv_teal is exported so the tests here are extensive and cover srv_data as well.
#          testing of srv_data is not needed.

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


# teal_module output --------
testthat::describe("teal_module(s)", {
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

  testthat::it("are called and data is passed even if are datanames in `teal_data` are not sufficient")
})

testthat::describe("srv_teal slices_global(s)", {
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
})

testthat::describe("srv_teal summary table", {
  testthat::it("displays Obs only column if all datasets have no join keys")

  testthat::it("displays Subjects with count based on foreign key column")

  testthat::it("displays parent's Subjects with count based on primary key ")

  testthat::it("reflects filters and displays subjects by their unique id count")

  testthat::it("reflects transforms")

  testthat::it("displays only module$datanames")

  testthat::it("displays subset of module$datanames if not sufficient")
})


# todo: if possible, test if the filters are set globally in slices_global (via srv_module_filter_manager)
