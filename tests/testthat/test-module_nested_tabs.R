teal_data <- teal.data::teal_data()
teal_data <- within(teal_data, iris <- head(iris))
datanames(teal_data) <- "iris"
filtered_data <- teal_data_to_filtered_data(teal_data)
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

test_module <- module(
  label = "test1",
  ui = function(id, ...) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("1")),
  datanames = NULL
)

# arguments -------
testthat::test_that("srv_teal_module accepts data being a reactive", {
  testthat::expect_no_error(
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal_data),
        modules = modules(test_module)
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module throws error if data is not a reactive", {
  testthat::expect_error(
    srv_teal_module("id", data_rv = teal_data, modules = modules(test_module)),
    "Must inherit from class 'reactive'"
  )
})

testthat::test_that("srv_teal_module accepts modules being a teal_modules", {
  testthat::expect_no_error(
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal_data),
        modules = modules(test_module)
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module throws error if modules are not teal_modules", {
  testthat::expect_error(
    srv_teal_module("id", data_rv = reactive(teal_data), modules = list(test_module)),
    "Must inherit from class 'teal_modules'/'teal_module'"
  )
})

testthat::test_that("srv_teal_module accepts datasets being a reactive", {
  testthat::expect_no_error(
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal_data),
        modules = modules(test_module),
        datasets = reactive(filtered_data)
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module accepts datasets being a NULL", {
  testthat::expect_no_error(
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal_data),
        modules = modules(test_module),
        datasets = NULL
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module throws error if datasets are not reactive or null", {
  testthat::expect_error(
    srv_teal_module("id", data_rv = reactive(teal_data), modules = modules(test_module), datasets = filtered_data),
    "Must inherit from class 'reactive'"
  )
})

testthat::test_that("srv_teal_module accepts slices_global being a reactiveVal object", {
  testthat::expect_no_error(
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal_data),
        modules = modules(test_module),
        slices_global = reactiveVal(teal_slices())
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module accepts reporter as a Reporter object", {
  testthat::expect_no_error(
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal_data),
        modules = modules(test_module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module throws error if reporter is not inherited from class Reporter", {
  testthat::expect_error(
    srv_teal_module("id", data_rv = reactive(teal_data), modules = modules(test_module), reporter = list()),
    "Must inherit from class 'Reporter'"
  )
})

# module --------
testthat::test_that("srv_teal_module: teal_module is called when data is triggered", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) -99L)
    ),
    expr = {
      testthat::expect_null(module_out())
      session$flushReact()
      testthat::expect_identical(trigger_data(), 1L)
      testthat::expect_identical(module_out(), -99L)
    }
  )
})

testthat::test_that("srv_teal_module: teal_module doesn't start when input data is not truthy", {
  trigger_external <- reactiveVal(NULL)
  data_in <- reactive({
    if (is.null(trigger_external())) {
      validate(need(FALSE, "oh no!"))
    } else {
      teal.data::teal_data(iris = iris)
    }
  })
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = data_in,
      modules = module(server = function(id, data) -99L)
    ),
    expr = {
      testthat::expect_error(data_rv(), "oh no!")
      testthat::expect_error(filtered_teal_data())
      testthat::expect_error(transformed_teal_data())
      testthat::expect_null(module_out())

      trigger_external(1)
      session$flushReact()
      testthat::expect_identical(module_out(), -99L)
    }
  )
})

testthat::test_that("srv_teal_module: teal_module starts even if are datanames not sufficient", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) -99L, dataname = c("iris", "dontexist"))
    ),
    expr = {
      testthat::expect_null(module_out())
      session$flushReact()
      testthat::expect_identical(trigger_data(), 1L)
      testthat::expect_identical(module_out(), -99L)
    }
  )
})

testthat::test_that("srv_teal_module: data retrigger doesn't call teal_module again", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) runif(1))
    ),
    expr = {
      session$flushReact()
      number_1 <- module_out()
      testthat::expect_true(obs_module$.destroyed)

      trigger_data(2L)
      session$flushReact()
      number_2 <- module_out()
      testthat::expect_identical(number_1, number_2)
    }
  )
})

testthat::test_that("srv_teal_module: teal_module gets data being a reactive teal_data", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris)),
      modules = module(server = function(id, data) data)
    ),
    expr = {
      session$flushReact()
      out <- module_out()
      testthat::expect_true(is(out, "reactive"))
      testthat::expect_s4_class(out(), "teal_data")
    }
  )
})

testthat::test_that("srv_teal_module: app fails if transformer doesn't return reactive", {
  testthat::expect_error(
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris)),
        modules = module(
          server = function(id, data) data,
          transformers = list(
            teal_data_module(
              ui = function(id) NULL,
              server = function(id, data) "whatever"
            )
          )
        )
      ),
      expr = {
        NULL
      }
    )
  )
})

testthat::test_that("srv_teal_module: teal_module gets data even if any transform fails", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris)),
      modules = module(
        server = function(id, data) data,
        transformers = transform_list
      )
    ),
    expr = {
      datasets()$set_filter_state(teal_slices(teal_slice("iris", "Species", selected = "virginica")))
      session$flushReact()
      out <- module_out()
      testthat::expect_identical(out()[["iris"]], dplyr::filter(iris, Species == "virginica") %>% head(6))
      testthat::expect_identical(out()[["mtcars"]], head(mtcars, 6))
    }
  )
})

testthat::test_that("srv_teal_module: teal_module gets data with datasets == module$datanames", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars, data3 = data.frame(x = 1))),
      modules = module(server = function(id, data) data, datanames = c("iris", "data3"))
    ),
    expr = {
      session$flushReact()
      out <- module_out()
      testthat::expect_identical(teal.data::datanames(out()), c("iris", "data3"))
      testthat::expect_identical(ls(out()@env), c("data3", "data3_raw", "iris", "iris_raw"))
    }
  )
})

testthat::test_that("srv_teal_module: teal_module doesn't get extra data added in a transform", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(
        server = function(id, data) data,
        datanames = c("iris", "data3"),
        transformers = list(
          teal_data_module(
            ui = function(id) NULL,
            server = function(id, data) {
              moduleServer(id, function(input, output, session) {
                reactive({
                  within(data(), {
                    data3 <- data.frame(x = 1)
                    data4 <- data.frame(y = 2)
                  })
                })
              })
            }
          )
        )
      )
    ),
    expr = {
      session$flushReact()
      out <- module_out()
      testthat::expect_identical(teal.data::datanames(out()), c("iris", "data3"))
      testthat::expect_identical(ls(out()@env), c("data3", "iris", "iris_raw"))
    }
  )
})

testthat::test_that("srv_teal_module: teal_module gets all data when module$datanames = \"all\"", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars, data3 = data.frame(x = 1))),
      modules = module(server = function(id, data) data, datanames = "all")
    ),
    expr = {
      session$flushReact()
      out <- module_out()
      testthat::expect_identical(teal.data::datanames(out()), c("iris", "mtcars", "data3"))
      testthat::expect_identical(ls(out()@env), c("data3", "data3_raw", "iris", "iris_raw", "mtcars", "mtcars_raw"))
    }
  )
})

testthat::test_that("srv_teal_module: teal_module gets data containing code (preprocessing, hashes, raw data)", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(
        within(
          teal.data::teal_data(),
          {
            iris <- iris
            mtcars <- mtcars
            data3 <- data.frame(x = 1)
          }
        )
      ),
      modules = module(server = function(id, data) data)
    ),
    expr = {
      session$flushReact()
      out <- module_out()
      hashes <- lapply(c("data3", "iris", "mtcars"), function(dataname) {
        sprintf(
          "stopifnot(rlang::hash(%s) == \"%s\")",
          dataname,
          rlang::hash(out()[[sprintf("%s_raw", dataname)]])
        )
      })
      testthat::expect_identical(
        teal.data::get_code(out()),
        paste(
          c(
            "iris <- iris",
            "mtcars <- mtcars",
            "data3 <- data.frame(x = 1)",
            "",
            hashes,
            "data3_raw <- data3",
            "iris_raw <- iris",
            "mtcars_raw <- mtcars"
          ),
          collapse = "\n"
        )
      )
    }
  )
})

testthat::test_that("srv_teal_module: teal_module receives filtered data with filter code", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(
        within(
          teal.data::teal_data(),
          {
            iris <- iris
            mtcars <- mtcars
            data3 <- data.frame(x = 1)
          }
        )
      ),
      modules = module(server = function(id, data) data)
    ),
    expr = {
      datasets()$set_filter_state(teal_slices(teal_slice("iris", "Species", selected = "virginica")))
      datasets()$set_filter_state(teal_slices(teal_slice("mtcars", "cyl", selected = "6")))
      session$flushReact()
      out <- module_out()
      testthat::expect_identical(out()[["iris"]], dplyr::filter(iris, Species == "virginica"))
      testthat::expect_identical(out()[["mtcars"]], dplyr::filter(mtcars, cyl == 6))
      hashes <- lapply(c("data3", "iris", "mtcars"), function(dataname) {
        sprintf(
          "stopifnot(rlang::hash(%s) == \"%s\")",
          dataname,
          rlang::hash(out()[[sprintf("%s_raw", dataname)]])
        )
      })
      testthat::expect_identical(
        teal.data::get_code(out()),
        paste(
          c(
            "iris <- iris",
            "mtcars <- mtcars",
            "data3 <- data.frame(x = 1)",
            "",
            hashes,
            "data3_raw <- data3",
            "iris_raw <- iris",
            "mtcars_raw <- mtcars",
            "\niris <- dplyr::filter(iris, Species == \"virginica\")",
            "mtcars <- dplyr::filter(mtcars, cyl == 6)"
          ),
          collapse = "\n"
        )
      )
    }
  )
})

testthat::test_that("srv_teal_module: teal_module receives transformed data with transform code", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(
        within(
          teal.data::teal_data(),
          {
            iris <- iris
            mtcars <- mtcars
            data3 <- data.frame(x = 1)
          }
        )
      ),
      modules = module(server = function(id, data) data, transformers = transform_list)
    ),
    expr = {
      datasets()$set_filter_state(teal_slices(teal_slice("iris", "Species", selected = "virginica")))
      datasets()$set_filter_state(teal_slices(teal_slice("mtcars", "cyl", selected = "6")))
      session$flushReact()
      out <- module_out()
      testthat::expect_identical(out()[["iris"]], dplyr::filter(iris, Species == "virginica") %>% head(6))
      testthat::expect_identical(out()[["mtcars"]], dplyr::filter(mtcars, cyl == 6) %>% head(6))
      hashes <- lapply(c("data3", "iris", "mtcars"), function(dataname) {
        sprintf(
          "stopifnot(rlang::hash(%s) == \"%s\")",
          dataname,
          rlang::hash(out()[[sprintf("%s_raw", dataname)]])
        )
      })
      testthat::expect_identical(
        teal.data::get_code(out()),
        paste(
          c(
            "iris <- iris",
            "mtcars <- mtcars",
            "data3 <- data.frame(x = 1)",
            "",
            hashes,
            "data3_raw <- data3",
            "iris_raw <- iris",
            "mtcars_raw <- mtcars",
            "\niris <- dplyr::filter(iris, Species == \"virginica\")",
            "mtcars <- dplyr::filter(mtcars, cyl == 6)",
            "iris <- head(iris)",
            "mtcars <- head(mtcars)"
          ),
          collapse = "\n"
        )
      )
    }
  )
})

testthat::test_that("srv_teal_module.teal_module does not pass data if not in the args explicitly", {
  module <- module(server = function(id, ...) {
    moduleServer(id, function(input, output, session) {
      testthat::expect_null(list(...)$data)
    })
  })

  shiny::testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = modules(module)
    ),
    expr = {
      session$flushReact()
    }
  )
})

testthat::test_that("srv_teal_module.teal_module passes (deprecated) datasets to the server module", {
  module <- lifecycle::expect_deprecated(
    module(server = function(id, datasets) {
      moduleServer(id, function(input, output, session) datasets)
    })
  )

  testthat::expect_warning(
    shiny::testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
        modules = module
      ),
      expr = {
        session$flushReact()
        testthat::expect_s3_class(module_out(), "FilteredData")
      }
    ),
    "datasets argument is not reactive and therefore it won't be updated when data is refreshed."
  )
})

testthat::test_that("srv_teal_module.teal_module passes server_args to the ...", {
  server_args <- list(a = 1, b = 2)
  module <- module(server_args = server_args, server = function(id, ...) {
    moduleServer(id, function(input, output, session) list(...))
  })

  shiny::testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module
    ),
    expr = {
      session$flushReact()
      testthat::expect_identical(module_out(), server_args)
    }
  )
})

testthat::test_that("srv_teal_module.teal_module passes filter_panel_api if specified", {
  shiny::testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, filter_panel_api) filter_panel_api)
    ),
    expr = {
      session$flushReact()
      testthat::expect_s3_class(module_out(), "FilterPanelAPI")
    }
  )
})

testthat::test_that("srv_teal_module.teal_module passes Reporter if specified", {
  shiny::testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, reporter) reporter)
    ),
    expr = {
      session$flushReact()
      testthat::expect_s3_class(module_out(), "Reporter")
    }
  )
})

testthat::test_that("srv_teal_module: summary table displays Obs only column if all datasets have no join keys", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) data)
    ),
    expr = {
      session$flushReact()
      testthat::expect_identical(
        summary_table(),
        data.frame(
          `Data Name` = c("iris", "mtcars"),
          Obs = c("150/150", "32/32"),
          check.names = FALSE
        )
      )
    }
  )
})

testthat::test_that("srv_teal_module: summary table displays Subjects with count based on foreign key column", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive({
        data <- within(teal.data::teal_data(), {
          df_parent <- data.frame(id = 1:10, x = letters[1:10])
          df_child <- data.frame(id = 1:20, parent_id = rep(1:10, each = 2), y = letters[1:20])
          mtcars <- mtcars
        })
        teal.data::join_keys(data) <- teal.data::join_keys(
          teal.data::join_key("df_parent", "df_child", keys = c(id = "parent_id")),
          teal.data::join_key("df_parent", keys = c(id = "id")),
          teal.data::join_key("df_child", keys = "id")
        )
        data
      }),
      modules = module(server = function(id, data) data)
    ),
    expr = {
      session$flushReact()
      # todo: datanames in a summary should be presented in topological order
      testthat::expect_identical(
        summary_table(),
        data.frame(
          `Data Name` = c("df_parent", "df_child", "mtcars"),
          Obs = c("10/10", "20/20", "32/32"),
          Subjects = c("10/10", "10/10", ""),
          check.names = FALSE
        )
      )
    }
  )
})

testthat::test_that("srv_teal_module: summary table displays parent's Subjects with count based on primary key ", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive({
        data <- within(teal.data::teal_data(), {
          df_parent <- data.frame(id = 1:10, x = letters[1:10])
          mtcars <- mtcars
        })
        teal.data::join_keys(data) <- teal.data::join_keys(
          teal.data::join_key("df_parent", "df_child", keys = c(id = "parent_id")),
          teal.data::join_key("df_parent", keys = c(id = "id"))
        )
        data
      }),
      modules = module(server = function(id, data) data)
    ),
    expr = {
      session$flushReact()
      testthat::expect_identical(
        summary_table(),
        data.frame(
          `Data Name` = c("df_parent", "mtcars"),
          Obs = c("10/10", "32/32"),
          Subjects = c("10/10", ""),
          check.names = FALSE
        )
      )
    }
  )
})

testthat::test_that("srv_teal_module: summary table reflects filters and displays subjects by their unique id count", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive({
        data <- teal.data::teal_data(iris = iris, mtcars = mtcars)
        data
      }),
      modules = module(server = function(id, data) data)
    ),
    expr = {
      datasets()$set_filter_state(teal_slices(teal_slice("iris", "Species", selected = "versicolor")))
      datasets()$set_filter_state(teal_slices(teal_slice("mtcars", "cyl", selected = "6")))
      session$flushReact()
      testthat::expect_identical(
        summary_table(),
        data.frame(
          `Data Name` = c("iris", "mtcars"),
          Obs = c("50/150", "7/32"),
          check.names = FALSE
        )
      )
    }
  )
})

testthat::test_that("srv_teal_module: summary table reflects transforms", {

})

testthat::test_that("srv_teal_module: summary table displays only module$datanames", {

})

testthat::test_that("srv_teal_module: summary table displays subset of module$datanames if not sufficient", {

})

# todo: check join keys in data() passed to the teal_module
