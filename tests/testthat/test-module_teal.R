# comment: srv_teal is exported so the tests here are extensive and cover srv_data as well.
#          testing of srv_data is not needed.
module_summary_table <<- function(output, id) {
  testthat::skip_if_not_installed("rvest")
  table_id <- sprintf("teal_modules-%s-data_summary-table", id)
  html <- output[[table_id]]$html
  as.data.frame(rvest::html_table(rvest::read_html(html), header = TRUE)[[1]])
}

is_slices_equivalent <<- function(x, y, with_attrs = TRUE) {
  x_list <- as.list(x, recursive = TRUE)
  y_list <- as.list(y, recursive = TRUE)
  attributes(x_list) <- NULL
  attributes(y_list) <- NULL
  if (with_attrs) {
    attributes(x_list) <- attributes(x)[c("mapping", "module_specific")]
    attributes(y_list) <- attributes(y)[c("mapping", "module_specific")]
  }
  identical(x_list, y_list)
}

transform_list <<- list(
  fail = teal_transform_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        add_error <- reactiveVal(TRUE)
        observeEvent(input$add_error, add_error(input$add_error))

        reactive({
          if (add_error()) {
            stop("Oh no")
          } else {
            within(data(), iris <- head(iris, n = floor(nrow(iris) / 2)))
          }
        })
      })
    }
  ),
  iris = teal_transform_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        n <- reactiveVal(6)
        observeEvent(input$n, n(input$n))

        reactive({
          within(data(), iris <- head(iris, n = n_input), n_input = n())
        })
      })
    }
  ),
  mtcars = teal_transform_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        n <- reactiveVal(6)
        observeEvent(input$n, n(input$n))

        reactive({
          within(data(), mtcars <- head(mtcars, n = n_input), n_input = n())
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
      "Assertion on 'data' failed: Must inherit from class 'teal_data'/'teal_data_module'/'reactive', but has class 'data.frame'." # nolint: line_length
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
      "Must be a reactive"
    )
  })
})

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
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1(), 101L)
        testthat::expect_null(modules_output$module_2())

        session$setInputs(`teal_modules-active_tab` = "module_2")
        testthat::expect_identical(modules_output$module_1(), 101L)
        testthat::expect_identical(modules_output$module_2(), 102L)
      }
    )
  })

  testthat::it("are called only after teal_data_module is resolved", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) actionButton("submit", "click me"),
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              eventReactive(input$submit, teal_data(iris = iris))
            })
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) 101L)
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        session$flushReact()
        testthat::expect_null(modules_output$module_1())


        session$setInputs("data-teal_data_module-submit" = "1")
        session$flushReact()
        testthat::expect_identical(modules_output$module_1(), 101L)
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
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("are not called when teal_data_module returns validation error", {
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
        testthat::expect_s3_class(data_handled(), "shiny.silent.error")
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("are not called when teal_data_module throws an error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) {
            moduleServer(id, function(input, output, session) {
              reactive(stop("my error"))
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
        testthat::expect_s3_class(data_handled(), "simpleError")
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("are not called when teal_data_module returns qenv.error", {
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
        testthat::expect_s3_class(data_handled(), "qenv.error")
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
        session$setInputs(`data-teal_data_module-dataset` = "iris", `teal_modules-active_tab` = "module_1")
        testthat::expect_setequal(names(modules_output$module_1()()), "iris")
        session$setInputs(`data-teal_data_module-dataset` = "mtcars", `teal_modules-active_tab` = "module_2")
        testthat::expect_setequal(names(modules_output$module_2()()), "mtcars")
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
          `data-teal_data_module-dataset` = "iris",
          `teal_modules-active_tab` = "module_1"
        )
        out <- modules_output$module_1()
        testthat::expect_true(!is.null(out))
        session$setInputs(`data-teal_data_module-dataset` = "mtcars")
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
        testthat::expect_identical(names(modules_output$module_1()()), "iris")
        testthat::expect_identical(modules_output$module_1()()[["iris"]], iris)
      }
    )
  })

  testthat::describe("reserved dataname is being used:", {
    testthat::it("multiple datanames with `all` and `.raw_data`", {
      testthat::skip_if_not_installed("rvest")

      # Shared common code for tests
      td <- within(teal.data::teal_data(), {
        all <- mtcars
        iris <- iris
        .raw_data <- data.frame(
          Species = c("Setosa", "Virginica", "Versicolor"),
          New.Column = c("Setosas are cool", "Virginicas are also cool", "Versicolors are cool too")
        )
      })
      teal.data::join_keys(td) <- teal.data::join_keys(join_key(".raw_data", "iris", "Species"))

      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = td,
          modules = modules(module("module_1", server = function(id, data) data))
        ),
        expr = {
          session$setInputs("teal_modules-active_tab" = "module_1")
          testthat::expect_equal(
            trimws(
              rvest::html_text2(
                rvest::read_html(
                  output[["teal_modules-module_1-validate_datanames-message"]]$html
                )
              )
            ),
            "all and .raw_data are reserved for internal use. Please avoid using them as dataset names."
          )
        }
      )
    })

    testthat::it("single dataname with `all`", {
      testthat::skip_if_not_installed("rvest")

      td <- within(teal.data::teal_data(), {
        all <- mtcars
        iris <- iris
      })

      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = td,
          modules = modules(
            module("module_1", server = function(id, data) data)
          )
        ),
        expr = {
          session$setInputs("teal_modules-active_tab" = "module_1")
          session$flushReact()

          testthat::expect_equal(
            trimws(
              rvest::html_text2(
                rvest::read_html(
                  output[["teal_modules-module_1-validate_datanames-message"]]$html
                )
              )
            ),
            "all is reserved for internal use. Please avoid using it as a dataset name."
          )
        }
      )
    })
  })

  testthat::describe("warnings on missing datanames", {
    testthat::it("warns when dataname is not available", {
      testthat::skip_if_not_installed("rvest")
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal_data(iris = iris),
          modules = modules(
            module("module_1", server = function(id, data) data, datanames = c("iris", "missing"))
          )
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          testthat::expect_equal(
            trimws(
              rvest::html_text2(
                rvest::read_html(
                  output[["teal_modules-module_1-validate_datanames-message"]]$html
                )
              )
            ),
            "Dataset missing is missing. Dataset available in data: iris."
          )
        }
      )
    })

    testthat::it("warns when datanames are not available", {
      testthat::skip_if_not_installed("rvest")
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal_data(mtcars = mtcars, iris = iris),
          modules = modules(
            module("module_1", datanames = c("mtcars", "iris", "missing1", "missing2"))
          )
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")

          testthat::expect_equal(
            trimws(
              rvest::html_text2(
                rvest::read_html(
                  output[["teal_modules-module_1-validate_datanames-message"]]$html
                )
              )
            ),
            "Datasets missing1 and missing2 are missing. Datasets available in data: iris and mtcars."
          )
        }
      )
    })

    testthat::it("warns about empty data when none of module$datanames is available (even if data is not empty)", {
      testthat::skip_if_not_installed("rvest")
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal_data(mtcars = mtcars),
          modules = modules(
            module("module_1", datanames = c("missing1", "missing2"))
          )
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          testthat::expect_equal(
            trimws(
              rvest::html_text2(
                rvest::read_html(
                  output[["teal_modules-module_1-validate_datanames-message"]]$html
                )
              )
            ),
            "Datasets missing1 and missing2 are missing. No datasets are available in data."
          )
        }
      )
    })

    testthat::it("warns about empty data when none of module$datanames is available", {
      testthat::skip_if_not_installed("rvest")
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = reactive(teal_data(mtcars = mtcars)),
          modules = modules(
            module("module_1", datanames = c("missing1", "missing2"))
          )
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          testthat::expect_equal(
            trimws(
              rvest::html_text2(
                rvest::read_html(
                  output[["datanames_warning-message"]]$html
                )
              )
            ),
            "Datasets missing1 and missing2 are missing for module 'module_1'. Dataset available in data: mtcars."
          )
        }
      )
    })
  })

  testthat::it("is called and receives data even if datanames in `teal_data` are not sufficient", {
    data <- teal_data(iris = iris)
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(data),
        modules = modules(
          module("module_1", server = function(id, data) data, datanames = c("iris", "mtcars"))
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(names(modules_output$module_1()()), "iris")
      }
    )
  })

  testthat::it("receives all objects from teal_data when module$datanames = \"all\"", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive({
          td <- teal_data(iris = iris, mtcars = mtcars, swiss = swiss, iris_raw = iris)
          td
        }),
        modules = modules(
          module("module_1", server = function(id, data) data, datanames = "all")
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(
          names(modules_output$module_1()()),
          c("iris", "iris_raw", "mtcars", "swiss")
        )
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

  testthat::it("receives all transformator datasets if module$datanames == 'all'", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive({
          td <- within(teal_data(), {
            iris <- iris
            mtcars <- mtcars
          })
          td
        }),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = list(
              teal_transform_module(
                label = "Dummy",
                server = function(id, data) {
                  moduleServer(id, function(input, output, session) {
                    reactive(within(data(), swiss <- swiss))
                  })
                }
              )
            ),
            datanames = "all"
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(names(modules_output$module_1()()), c("iris", "mtcars", "swiss"))
      }
    )
  })

  testthat::it("receives all datasets if transform$datanames == 'all'", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive({
          td <- within(teal_data(), {
            iris <- iris
            mtcars <- mtcars
          })
          td
        }),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = list(
              teal_transform_module(
                label = "Dummy",
                server = function(id, data) {
                  moduleServer(id, function(input, output, session) {
                    reactive(within(data(), swiss <- swiss))
                  })
                }
              )
            ),
            datanames = "all"
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(names(modules_output$module_1()()), c("iris", "mtcars", "swiss"))
      }
    )
  })

  testthat::it("receives all raw datasets based on module$datanames", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive({
          td <- within(teal_data(), {
            iris <- iris
            mtcars <- mtcars
            swiss <- swiss
          })
          td
        }),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            datanames = c("iris", "swiss")
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_setequal(names(modules_output$module_1()()[[".raw_data"]]), c("iris", "swiss"))
      }
    )
  })

  testthat::it("combines datanames from transform/module $datanames", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(teal_data(iris = iris, mtcars = mtcars, not_included = data.frame())),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = list(
              teal_transform_module(
                label = "Dummy",
                ui = function(id) div("(does nothing)"),
                server = function(id, data) {
                  moduleServer(id, function(input, output, session) {
                    reactive(within(data(), swiss <- swiss))
                  })
                },
                datanames = "swiss"
              )
            ),
            datanames = c("iris", "mtcars")
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(names(modules_output$module_1()()), c("iris", "mtcars", "swiss"))
      }
    )
  })

  testthat::it("does not receive transformator datasets not specified in transform$datanames nor modue$datanames", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive({
          td <- within(teal_data(), {
            iris <- iris
            mtcars <- mtcars
          })
          td
        }),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = list(
              teal_transform_module(
                label = "Dummy",
                server = function(id, data) {
                  moduleServer(id, function(input, output, session) {
                    reactive(within(data(), swiss <- swiss))
                  })
                },
                datanames = character(0)
              )
            ),
            datanames = c("iris", "mtcars")
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(names(modules_output$module_1()()), c("iris", "mtcars"))
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

  testthat::it("srv_teal_module.teal_module passes quoted arguments to the teal_module$server call", {
    tm_query <- function(query) {
      module(
        "module_1",
        server = function(id, data, query) {
          moduleServer(id, function(input, output, session) {
            reactive(q <- eval_code(data(), query))
          })
        },
        server_args = list(query = query)
      )
    }
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(a_dataset = iris),
        modules = modules(tm_query(quote(a_dataset <- subset(a_dataset, Species == "setosa"))))
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        session$flushReact()

        testthat::expect_setequal(
          "setosa",
          unique(modules_output$module_1()()[["a_dataset"]]$Species)
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

  testthat::it("does not receive report_previewer when none of the modules contain reporter argument", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module("module_1", server = function(id) {}),
          module("module_2", server = function(id) {})
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "report_previewer")
        testthat::expect_setequal(names(modules_output), c("module_1", "module_2"))
      }
    )
  })

  testthat::it("receives one report_previewer module when any module contains reporter argument", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module("module_1", server = function(id, reporter) {}),
          module("module_2", server = function(id) {})
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "report_previewer")
        testthat::expect_setequal(names(modules_output), c("module_1", "module_2", "report_previewer"))
      }
    )
  })
})

testthat::describe("srv_teal filters", {
  testthat::describe("slicesGlobal", {
    testthat::it("is set to initial filters when !module_specific", {
      init_filter <- teal_slices(
        teal_slice("iris", "Species"),
        teal_slice("mtcars", "cyl"),
        mapping = list(
          global_filters = c("iris Species", "mtcars cyl")
        ),
        module_specific = FALSE
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
          testthat::expect_identical(slices_global$all_slices(), init_filter)
        }
      )
    })
    testthat::it("is set to initial filters with resolved attr(, 'mapping')$<modules label> when `module_specific`", {
      init_filter <- teal_slices(
        teal_slice("iris", "Species"),
        teal_slice("mtcars", "cyl"),
        module_specific = TRUE,
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
          setdiff_teal_slices <- getFromNamespace("setdiff_teal_slices", "teal.slice")
          testthat::expect_length(setdiff_teal_slices(slices_global$all_slices(), init_filter), 0)
          testthat::expect_identical(
            attr(slices_global$all_slices(), "mapping"),
            list(
              `module-1` = c("iris Species", "mtcars cyl"),
              `module-2` = c("iris Species", "mtcars cyl")
            )
          )
        }
      )
    })
    testthat::it("slices in slicesGlobal and in FilteredData refer to the same object", {
      init_filter <- teal_slices(
        teal_slice("iris", "Species"),
        teal_slice("mtcars", "cyl"),
        module_specific = TRUE,
        mapping = list(
          global_filters = c("iris Species", "mtcars cyl")
        )
      )
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris, mtcars = mtcars),
          modules = modules(example_module(label = "module_1"), example_module(label = "module_2")),
          filter = init_filter
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          testthat::expect_true(identical(
            slices_global$all_slices()[[1]],
            slices_global$module_slices_api[["module_1"]]$get_filter_state()[[1]]
          ))
          testthat::expect_true(identical(
            slices_global$all_slices()[[1]],
            slices_global$module_slices_api[["module_2"]]$get_filter_state()[[1]]
          ))
        }
      )
    })
    testthat::it("appends new slice and activates in $global_filters when added in a module if !module_specific", {
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
          ),
          filter = teal_slices(module_specific = FALSE)
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          session$setInputs(`teal_modules-module_2-filter_panel-filters-iris-iris-filter-var_to_add` = "Species")
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = teal_slices(
              teal_slice("iris", "Species", choices = unique(iris$Species), selected = unique(iris$Species)),
              mapping = list(global_filters = "iris Species"),
              module_specific = FALSE
            )
          ))
        }
      )
    })
    testthat::it("deactivates in $global_filters when removed from module if !module_specific", {
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
          ),
          filter = teal_slices(
            teal_slice("iris", varname = "Species", selected = "versicolor"),
            module_specific = FALSE
          )
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          session$setInputs(`teal_modules-module_2-filter_panel-filters-iris-filter-iris_Species-remove` = "Species")
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = teal_slices(
              teal_slice("iris", "Species", choices = unique(iris$Species), selected = "versicolor"),
              mapping = list(global_filters = character(0)),
              module_specific = FALSE
            )
          ))
        }
      )
    })
    testthat::it("appends new slice and activates in $<module> when added in a module if module_specific", {
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
          ),
          filter = teal_slices(module_specific = TRUE)
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          session$setInputs(`teal_modules-module_2-filter_panel-filters-iris-iris-filter-var_to_add` = "Species")
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = teal_slices(
              teal_slice("iris", "Species", choices = unique(iris$Species), selected = unique(iris$Species)),
              mapping = list(module_1 = character(0), module_2 = "iris Species"),
              module_specific = TRUE
            )
          ))
        }
      )
    })
    testthat::it("appends added 'duplicated' slice and makes new-slice$id unique", {
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
          ),
          filter = teal_slices(
            teal_slice("iris", "Species", choices = unique(iris$Species), selected = unique(iris$Species)),
            mapping = list(global_filters = character(0))
          )
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-module_1-filter_panel-filters-iris-iris-filter-var_to_add` = "Species")
          session$flushReact()
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = teal_slices(
              teal_slice("iris", "Species", choices = unique(iris$Species), selected = unique(iris$Species)),
              teal_slice("iris", "Species",
                choices = unique(iris$Species), selected = unique(iris$Species),
                id = "iris Species_1"
              ),
              mapping = list(global_filters = "iris Species_1"),
              module_specific = FALSE
            )
          ))
        }
      )
    })
    testthat::it("deactivates in $<module> when removed from module if module_specific", {
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
          ),
          filter = teal_slices(
            teal_slice("iris", varname = "Species", selected = "versicolor"),
            mapping = list(global_filters = "iris Species"),
            module_specific = TRUE
          )
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          session$setInputs(`teal_modules-module_2-filter_panel-filters-iris-filter-iris_Species-remove` = "Species")
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = teal_slices(
              teal_slice("iris", "Species", choices = unique(iris$Species), selected = "versicolor"),
              mapping = list(module_1 = "iris Species", module_2 = character(0)),
              module_specific = TRUE
            )
          ))
        }
      )
    })
    testthat::it("auto-resolves to mapping$<m> when setting slices with mapping$global_filters in module_specific ", {
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
          ),
          filter = teal_slices(module_specific = TRUE)
        ),
        expr = {
          testthat::skip("need a fix in a .slicesGlobal")
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          slices_global$slices_set(teal_slices(
            teal_slice("iris", "Species"),
            mapping = list(global_filters = "iris Species")
          ))
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = teal_slices(
              teal_slice("iris", "Species", choices = unique(iris$Species), selected = unique(iris$Species)),
              mapping = list(module_1 = "iris Species", module_2 = "iris Species"),
              module_specific = TRUE
            )
          ))
        }
      )
    })
    testthat::it("sets filters from mapping$<mod> to all modules' FilteredData when !module_specific", {
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
          ),
          filter = teal_slices(module_specific = FALSE)
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          slices_global$slices_append(teal_slices(teal_slice("iris", "Species", selected = "versicolor")))
          slices_global$slices_active(list(global_filter = "iris Species"))
          session$flushReact()
          expected_slices <- slices_global$all_slices()

          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = slices_global$module_slices_api[["global_filters"]]$get_filter_state(),
            with_attrs = FALSE
          ))
        }
      )
    })
    testthat::it("sets filters from mapping$<mod> to module's FilteredData when module_specific", {
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
          ),
          filter = teal_slices(module_specific = TRUE)
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          slices_global$slices_append(teal_slices(teal_slice("iris", "Species", selected = "versicolor")))
          slices_global$slices_active(list(module_1 = "iris Species"))
          session$flushReact()
          expected_slices <- slices_global$all_slices()
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = slices_global$module_slices_api[["module_1"]]$get_filter_state(),
            with_attrs = FALSE
          ))
          testthat::expect_true(is_slices_equivalent(
            x = teal_slices(),
            y = slices_global$module_slices_api[["module_2"]]$get_filter_state(),
            with_attrs = FALSE
          ))
        }
      )
    })
    testthat::it("sets filters from mapping$global_filters to all modules' FilteredData when module_specific", {
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
          ),
          filter = teal_slices(module_specific = TRUE)
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          session$setInputs(`teal_modules-active_tab` = "module_2")
          slices_global$slices_append(teal_slices(teal_slice("iris", "Species", selected = "versicolor")))
          slices_global$slices_active(list(global_filters = "iris Species"))
          session$flushReact()
          expected_slices <- slices_global$all_slices()
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = slices_global$module_slices_api[["module_1"]]$get_filter_state(),
            with_attrs = FALSE
          ))
          testthat::expect_true(is_slices_equivalent(
            x = slices_global$all_slices(),
            y = slices_global$module_slices_api[["module_2"]]$get_filter_state(),
            with_attrs = FALSE
          ))
        }
      )
    })
    testthat::it("change in the slicesGlobal causes module's data filtering", {
      existing_filters <- teal_slices(
        teal_slice(dataname = "iris", varname = "Species", selected = "versicolor"),
        teal_slice(dataname = "mtcars", varname = "cyl", selected = 6)
      )
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = reactive(within(teal.data::teal_data(), {
            iris <- iris
            mtcars <- mtcars
          })),
          filter = existing_filters,
          modules = modules(module("module_1", server = function(id, data) data))
        ),
        expr = {
          session$setInputs(`teal_modules-active_tab` = "module_1")
          slices_global$slices_set(
            teal_slices(
              teal_slice("mtcars", varname = "cyl", selected = "4")
            )
          )
          session$flushReact()
          # iris is not active
          testthat::expect_identical(modules_output$module_1()()[["iris"]], iris)
          # mtcars has been modified
          expected_mtcars <- subset(mtcars, cyl == 4)
          testthat::expect_identical(modules_output$module_1()()[["mtcars"]], expected_mtcars)
          expected_code <- paste0(
            c(
              "iris <- iris",
              "mtcars <- mtcars",
              sprintf('stopifnot(rlang::hash(iris) == "%s") # @linksto iris', rlang::hash(iris)),
              sprintf('stopifnot(rlang::hash(mtcars) == "%s") # @linksto mtcars', rlang::hash(mtcars)),
              ".raw_data <- list2env(list(iris = iris, mtcars = mtcars))",
              "lockEnvironment(.raw_data) # @linksto .raw_data",
              "mtcars <- dplyr::filter(mtcars, cyl == 4)"
            ),
            collapse = "\n"
          )
          testthat::expect_identical(teal.code::get_code(modules_output$module_1()()), expected_code)
        }
      )
    })
  })

  testthat::describe("mapping table", {
    testthat::it("returns no rows if no filters set", {
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
    testthat::it("returns global filters with active=true, inactive=false, unavailable=na", {
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris, mtcars = mtcars),
          modules = modules(
            module("module_1", server = function(id, data) data),
            module("module_2", server = function(id, data) data)
          ),
          filter = teal_slices(
            teal_slice("iris", "Species"),
            teal_slice("mtcars", "cyl"),
            teal_slice("unknown", "unavailable"),
            mapping = list(global_filters = "iris Species")
          )
        ),
        expr = {
          testthat::expect_warning(
            session$setInputs("teal_modules-active_tab" = "module_1"),
            "Filter 'unknown unavailable' refers to dataname not available in 'data'"
          )
          session$flushReact()
          testthat::expect_identical(
            mapping_table(),
            data.frame(
              `Global filters` = c(TRUE, FALSE, NA),
              row.names = c("iris Species", "mtcars cyl", "unknown unavailable"),
              check.names = FALSE
            )
          )
        }
      )
    })

    testthat::it("returns column per module with active=true, inactive=false, unavailable=na", {
      shiny::testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris, mtcars = mtcars),
          modules = modules(
            module("module_1", server = function(id, data) data),
            module("module_2", server = function(id, data) data)
          ),
          filter = teal_slices(
            teal_slice("iris", "Species"),
            teal_slice("mtcars", "cyl"),
            teal_slice("unknown", "unavailable"),
            module_specific = TRUE,
            mapping = list(module_1 = "iris Species", module_2 = "mtcars cyl")
          )
        ),
        expr = {
          testthat::expect_warning(
            session$setInputs("teal_modules-active_tab" = "module_1"),
            "Filter 'unknown unavailable' refers to dataname not available in 'data'"
          )
          session$flushReact()
          testthat::expect_identical(
            mapping_table(),
            data.frame(
              module_1 = c(TRUE, FALSE, NA),
              module_2 = c(FALSE, TRUE, NA),
              row.names = c("iris Species", "mtcars cyl", "unknown unavailable"),
              check.names = FALSE
            )
          )
        }
      )
    })

    testthat::it("what happens when module$label is duplicated (when nested modules)", {
      testthat::skip("todo")
    })
  })
})

testthat::describe("srv_teal data reload", {
  testthat::it("sets back the same active filters in each module", {
    testthat::skip("todo")
  })
  testthat::it("doesn't fail when teal_data has no datasets", {
    testthat::skip("todo")
  })
})

testthat::describe("srv_teal teal_module(s) transformator", {
  testthat::it("evaluates custom qenv call and pass updated teal_data to the module", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = transform_list[c("iris", "mtcars")]
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_identical(modules_output$module_1()()[["iris"]], head(iris))
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], head(mtcars))
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
            transformators = transform_list[c("iris", "mtcars")]
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        expected_iris <- subset(iris, Species == "versicolor")
        rownames(expected_iris) <- NULL
        expected_iris <- head(expected_iris)
        testthat::expect_identical(modules_output$module_1()()[["iris"]], expected_iris)
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], head(subset(mtcars, cyl == 6)))
        expected_code <- paste(collapse = "\n", c(
          "iris <- iris",
          "mtcars <- mtcars",
          sprintf('stopifnot(rlang::hash(iris) == "%s") # @linksto iris', rlang::hash(iris)),
          sprintf('stopifnot(rlang::hash(mtcars) == "%s") # @linksto mtcars', rlang::hash(mtcars)),
          ".raw_data <- list2env(list(iris = iris, mtcars = mtcars))",
          "lockEnvironment(.raw_data) # @linksto .raw_data",
          'iris <- dplyr::filter(iris, Species == "versicolor")',
          "mtcars <- dplyr::filter(mtcars, cyl == 6)",
          "iris <- head(iris, n = 6)",
          "mtcars <- head(mtcars, n = 6)"
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
            transformators = transform_list[c("iris", "mtcars")]
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        slices_global$slices_set(
          teal_slices(teal_slice(dataname = "mtcars", varname = "cyl", selected = "4"))
        )
        session$flushReact()

        testthat::expect_identical(modules_output$module_1()()[["iris"]], head(iris))
        testthat::expect_identical(modules_output$module_1()()[["mtcars"]], head(subset(mtcars, cyl == 4)))
        expected_code <- paste(collapse = "\n", c(
          "iris <- iris",
          "mtcars <- mtcars",
          sprintf('stopifnot(rlang::hash(iris) == "%s") # @linksto iris', rlang::hash(iris)),
          sprintf('stopifnot(rlang::hash(mtcars) == "%s") # @linksto mtcars', rlang::hash(mtcars)),
          ".raw_data <- list2env(list(iris = iris, mtcars = mtcars))",
          "lockEnvironment(.raw_data) # @linksto .raw_data",
          "mtcars <- dplyr::filter(mtcars, cyl == 4)",
          "iris <- head(iris, n = 6)",
          "mtcars <- head(mtcars, n = 6)"
        ))
        testthat::expect_identical(
          teal.code::get_code(modules_output$module_1()()),
          expected_code
        )
      }
    )
  })

  testthat::it("receives all possible objects while those not specified in module$datanames are unfiltered", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), {
          iris <- iris
          mtcars <- mtcars
        })),
        filter = teal_slices(
          teal_slice(dataname = "mtcars", varname = "cyl", selected = "4"),
          teal_slice(dataname = "iris", varname = "Species", selected = "versicolor")
        ),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            datanames = c("iris", "data_from_transform"),
            transformators = list(
              teal_transform_module(
                ui = function(id) NULL,
                server = function(id, data) {
                  moduleServer(id, function(input, output, session) {
                    reactive({
                      within(data(), data_from_transform <- list(iris = iris, mtcars = mtcars))
                    })
                  })
                },
                datanames = character(0)
              )
            )
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        data_from_transform <- modules_output$module_1()()[["data_from_transform"]]
        testthat::expect_identical(data_from_transform$mtcars, mtcars)
        expected_iris <- iris[iris$Species == "versicolor", ]
        rownames(expected_iris) <- NULL
        testthat::expect_identical(data_from_transform$iris, expected_iris)
      }
    )
  })

  testthat::it("throws a warning when transformator returns reactive.event", {
    testthat::expect_warning(
      testServer(
        app = srv_teal,
        args = list(
          id = "test",
          data = teal.data::teal_data(iris = iris),
          modules = modules(
            module(
              server = function(id, data) data,
              transformators = list(
                teal_transform_module(
                  ui = function(id) textInput("a", "an input"),
                  server = function(id, data) eventReactive(input$a, data())
                )
              )
            )
          )
        ),
        expr = {
          session$setInputs("teal_modules-active_tab" = "module")
          session$flushReact()
        }
      ),
      "Using eventReactive in teal_transform module server code should be avoided"
    )
  })

  testthat::it("fails when transformator doesn't return reactive", {
    testthat::expect_warning(
      # error decorator is mocked to avoid showing the trace error during the
      # test.
      # This tests works without the mocking, but it's more verbose.
      testthat::with_mocked_bindings(
        testServer(
          app = srv_teal,
          args = list(
            id = "test",
            data = teal.data::teal_data(iris = iris),
            modules = modules(
              module(
                server = function(id, data) data,
                transformators = list(
                  teal_transform_module(
                    ui = function(id) NULL,
                    server = function(id, data) "whatever"
                  )
                )
              )
            )
          ),
          expr = {
            session$setInputs("teal_modules-active_tab" = "module")
            session$flushReact()
          }
        ),
        decorate_err_msg = function(x, ...) {
          testthat::expect_error(x, "Must be a reactive")
          warning(tryCatch(x, error = function(e) e$message))
        },
      ),
      "Must be a reactive"
    )
  })

  testthat::it("pauses when transformator throws validation error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = list(
              teal_transform_module(
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
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("pauses when transformator throws validation error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = list(
              teal_transform_module(
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
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("pauses when transformator throws qenv error", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = list(
              teal_transform_module(
                ui = function(id) NULL,
                server = function(id, data) {
                  reactive(within(data(), stop("my error")))
                }
              )
            )
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("isn't called when `data` is not teal_data", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          module(
            label = "module_1",
            server = function(id, data) data,
            transformators = list(
              teal_transform_module(
                ui = function(id) NULL,
                server = function(id, data) {
                  reactive(data.frame())
                }
              )
            )
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "module_1")
        testthat::expect_null(modules_output$module_1())
      }
    )
  })

  testthat::it("changes module output for a module with a static decorator", {
    output_decorator <- teal_transform_module(
      label = "output_decorator",
      server = make_teal_transform_server(expression(object <- rev(object)))
    )

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(object = iris),
        modules = modules(example_module("mod1", decorators = list(output_decorator)))
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "mod1")
        session$setInputs(`teal_modules-mod1-module-dataname` = "object")
        session$flushReact()
        testthat::expect_identical(
          modules_output$mod1()()[["object"]],
          rev(iris)
        )
      }
    )
  })


  testthat::it("changes module output for a module with a decorator that is a function of an object name", {
    decorator_name <- function(output_name, label) {
      teal_transform_module(
        label = label,
        server = function(id, data) {
          moduleServer(id, function(input, output, session) {
            reactive({
              within(
                data(),
                output_name <- paste0(output_name, " lorem ipsum"),
                text = input$text,
                output_name = as.name(output_name)
              )
            })
          })
        }
      )
    }

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(x1 = "ABC"),
        modules = modules(
          example_module(
            "mod1",
            decorators = list(decorator_name(output_name = "object", label = "decorator_name"))
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "mod1")
        session$setInputs(`teal_modules-mod1-module-dataname` = "x1")
        session$flushReact()

        testthat::expect_identical(modules_output$mod1()()[["object"]], "ABC lorem ipsum")
      }
    )
  })

  testthat::it("changes module output for a module with an interactive decorator", {
    decorator_name <- function(output_name, label) {
      teal_transform_module(
        label = label,
        server = function(id, data) {
          moduleServer(id, function(input, output, session) {
            reactive({
              req(data(), input$text)
              within(
                data(),
                output_name <- paste0(output_name, " ", text),
                text = input$text,
                output_name = as.name(output_name)
              )
            })
          })
        }
      )
    }

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(x1 = "ABC"),
        modules = modules(
          example_module(
            "mod1",
            decorators = list(decorator_name(output_name = "object", label = "decorator_name"))
          )
        )
      ),
      expr = {
        session$setInputs(`teal_modules-active_tab` = "mod1")
        session$setInputs(`teal_modules-mod1-module-dataname` = "x1")
        session$setInputs(`teal_modules-mod1-module-decorate-transform_1-transform-text` = "lorem ipsum dolor")
        session$flushReact()

        testthat::expect_identical(modules_output$mod1()()[["object"]], "ABC lorem ipsum dolor")
      }
    )
  })
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
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
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

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
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

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
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

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
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
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
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

    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        slices_global$slices_set(teal_slices(teal_slice("a", "name", selected = "a")))
        session$flushReact()
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
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

  testthat::it("reflects transformator adding new dataset if specified in module", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          module(
            "module_1",
            server = function(id, data) data,
            transformators = teal_transform_module(
              datanames = character(0),
              server = function(id, data) {
                moduleServer(id, function(input, output, session) {
                  reactive({
                    within(data(), new_dataset <- data.frame(x = 1:3))
                  })
                })
              }
            ),
            datanames = c("iris", "new_dataset")
          )
        )
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
          data.frame(
            "Data Name" = c("iris", "new_dataset"),
            Obs = c("150/150", "3"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("reflects transformator filtering", {
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
              transformators = transform_list["iris"]
            )
          )
        ),
        expr = {
          session$setInputs("teal_modules-active_tab" = "module_1")
          testthat::expect_identical(
            module_summary_table(output, "module_1"),
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
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data, datanames = "iris"))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
          data.frame(
            "Data Name" = c("iris"),
            Obs = c("150/150"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("displays parent before child when join_keys are provided", {
    data <- teal.data::teal_data(
      parent = mtcars,
      child = data.frame(am = c(0, 1), test = c("a", "b"))
    )

    teal.data::join_keys(data) <- teal.data::join_keys(
      teal.data::join_key("parent", "child", keys = c("am"))
    )
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
          module_summary_table(output, "module_1")[["Data Name"]],
          c("parent", "child")
        )
      }
    )
  })

  testthat::it("displays subset of module$datanames if not sufficient", {
    data <- teal.data::teal_data(iris = iris, mtcars = mtcars)
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
          module_summary_table(output, "module_1"),
          data.frame(
            "Data Name" = c("iris"),
            Obs = c("150/150"),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("summary table displays MAE dataset added in transformators", {
    data <- within(teal.data::teal_data(), {
      iris <- iris
      mtcars <- mtcars
      foo <- identity
    })
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data, datanames = "all", transformators = list(
          teal_transform_module(
            server = function(id, data) {
              reactive({
                within(data(), {
                  withr::with_package("MultiAssayExperiment", {
                    data("miniACC", package = "MultiAssayExperiment", envir = environment())
                  })
                })
              })
            }
          )
        )))
      ),
      expr = {
        # throws warning as data("miniACC") hasn't been detected as miniACC dependency
        suppressWarnings(session$setInputs("teal_modules-active_tab" = "module_1"))
        testthat::expect_equal(
          module_summary_table(output, "module_1"),
          data.frame(
            "Data Name" = c(
              "iris", "miniACC", "- RNASeq2GeneNorm", "- gistict",
              "- RPPAArray", "- Mutations", "- miRNASeqGene", "mtcars"
            ),
            Obs = c("150/150", "", "198", "198", "33", "97", "471", "32/32"),
            Subjects = c(NA_integer_, 92, 79, 90, 46, 90, 80, NA_integer_),
            check.names = FALSE
          )
        )
      }
    )
  })

  testthat::it("displays unsupported datasets", {
    data <- within(teal.data::teal_data(), {
      iris <- iris
      mtcars <- mtcars
      foo <- identity
    })
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = data,
        modules = modules(module("module_1", server = function(id, data) data, datanames = "all"))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()
        testthat::expect_identical(
          module_summary_table(output, "module_1"),
          data.frame(
            "Data Name" = c("iris", "mtcars"),
            Obs = c("150/150", "32/32"),
            check.names = FALSE
          )
        )
      }
    )
  })
})

testthat::describe("srv_teal snapshot manager", {
  testthat::it("clicking reset button restores initial filters state when !module_specific", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module("module_1", server = function(id, data) data),
          module("module_2", server = function(id, data) data)
        ),
        filter = teal_slices(
          teal_slice("iris", "Species"),
          teal_slice("mtcars", "cyl"),
          module_specific = FALSE
        )
      ),
      expr = {
        initial_slices <- slices_global$all_slices()
        session$setInputs("teal_modules-active_tab" = "module_1")
        slices_global$slices_set(teal_slices())
        session$flushReact()
        session$setInputs("snapshot_manager_panel-module-snapshot_reset" = TRUE)
        session$flushReact()
        testthat::expect_true(
          is_slices_equivalent(
            slices_global$all_slices(),
            initial_slices
          )
        )
        testthat::expect_true(
          is_slices_equivalent(
            slices_global$module_slices_api[["global_filters"]]$get_filter_state(),
            initial_slices,
            with_attrs = FALSE
          )
        )
      }
    )
  })

  testthat::it("clicking reset button restores initial filters with respect to mapping state when module_specific", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(iris = iris, mtcars = mtcars),
        modules = modules(
          module("module_1", server = function(id, data) data),
          module("module_2", server = function(id, data) data)
        ),
        filter = teal_slices(
          teal_slice("iris", "Species"),
          teal_slice("mtcars", "cyl"),
          mapping = list(module_1 = "iris Species", module_2 = "mtcars cyl"),
          module_specific = TRUE
        )
      ),
      expr = {
        initial_slices <- slices_global$all_slices()
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$setInputs("teal_modules-active_tab" = "module_2")
        slices_global$slices_set(teal_slices())
        session$flushReact()
        session$setInputs("snapshot_manager_panel-module-snapshot_reset" = TRUE)
        session$flushReact()
        testthat::expect_true(
          is_slices_equivalent(
            slices_global$all_slices(),
            initial_slices
          )
        )
        testthat::expect_true(
          is_slices_equivalent(
            slices_global$module_slices_api[["module_1"]]$get_filter_state(),
            initial_slices[1],
            with_attrs = FALSE
          )
        )
        testthat::expect_true(
          is_slices_equivalent(
            slices_global$module_slices_api[["module_2"]]$get_filter_state(),
            initial_slices[2],
            with_attrs = FALSE
          )
        )
      }
    )
  })
})

testthat::describe("Datanames with special symbols", {
  testthat::it("are detected as datanames when defined as 'all'", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = teal.data::teal_data(
          iris = iris,
          `%a_pipe%` = function(lhs, rhs) paste(lhs, rhs)
        ),
        modules = modules(module("module_1", server = function(id, data) data)),
        filter = teal_slices(
          module_specific = TRUE
        )
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()

        testthat::expect_setequal(
          names(modules_output$module_1()()),
          c("iris", "%a_pipe%")
        )
      }
    )
  })

  testthat::it("are present in datanames when used in pre-processing code", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = within(
          teal.data::teal_data(),
          {
            iris <- iris
            mtcars <- mtcars
            `_a variable with spaces_` <- "new_column" # nolint: object_name.
            iris <- cbind(iris, data.frame(`_a variable with spaces_`))
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) data, datanames = c("iris", "_a variable with spaces_"))
        )
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()

        testthat::expect_setequal(
          names(modules_output$module_1()()),
          c("iris", "_a variable with spaces_")
        )
      }
    )
  })

  testthat::it("(when used as non-native pipe) are present in datanames in the pre-processing code", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = within(
          teal.data::teal_data(),
          {
            iris <- iris
            mtcars <- mtcars
            `%cbind%` <- function(lhs, rhs) cbind(lhs, rhs)
            iris <- iris %cbind% data.frame("new_column")
          }
        ),
        modules = modules(
          module("module_1", server = function(id, data) data, datanames = c("iris"))
        ),
        filter = teal_slices(
          module_specific = TRUE
        )
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()

        testthat::expect_contains(
          strsplit(
            x = teal.code::get_code(modules_output$module_1()()),
            split = "\n"
          )[[1]],
          c(
            "`%cbind%` <- function(lhs, rhs) cbind(lhs, rhs)",
            ".raw_data <- list2env(list(iris = iris))"
          )
        )
      }
    )
  })
})

testthat::describe("teal.data code with a function defined", {
  testthat::it("is fully reproducible", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), {
          fun <- function(x) {
            y <- x + 1
            y + 3
          }
        })),
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()

        # Need to evaluate characters to preserve indentation
        local_env <- new.env(parent = .GlobalEnv)
        dat <- modules_output$module_1()()

        eval(
          parse(text = teal.code::get_code(dat)),
          envir = local_env
        )

        testthat::expect_identical(local_env$fun(1), 5)
        testthat::expect_identical(local_env$fun(1), dat[["fun"]](1))
      }
    )
  })

  testthat::it("has the correct code (with hash)", {
    shiny::testServer(
      app = srv_teal,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), {
          fun <- function(x) {
            y <- x + 1
            y + 3
          }
        })),
        modules = modules(module("module_1", server = function(id, data) data))
      ),
      expr = {
        session$setInputs("teal_modules-active_tab" = "module_1")
        session$flushReact()

        # Need to evaluate characters to preserve indentation
        local_env <- new.env(parent = .GlobalEnv)
        eval(
          parse(
            text = paste(
              sep = "\n",
              "fun <- function(x) {",
              "    y <- x + 1",
              "    y + 3",
              "}"
            )
          ),
          envir = local_env
        )
        local(hash <- rlang::hash(deparse1(fun)), envir = local_env)
        testthat::expect_setequal(
          trimws(strsplit(
            x = teal.code::get_code(modules_output$module_1()()),
            split = "\n"
          )[[1]]),
          c(
            "fun <- function(x) {",
            "y <- x + 1",
            "y + 3",
            "}",
            sprintf("stopifnot(rlang::hash(deparse1(fun)) == \"%s\") # @linksto fun", local_env$hash),
            ".raw_data <- list2env(list(fun = fun))",
            "lockEnvironment(.raw_data) # @linksto .raw_data"
          )
        )
      }
    )
  })
})
