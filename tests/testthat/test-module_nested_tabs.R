teal_data <- teal.data::teal_data()
teal_data <- within(teal_data, iris <- head(iris))
datanames(teal_data) <- "iris"
filtered_data <- teal_data_to_filtered_data(teal_data)

test_module1 <- module(
  label = "test1",
  ui = function(id, ...) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("1")),
  datanames = NULL
)
test_module2 <- module(
  label = "test2",
  ui = function(id) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("2")),
  datanames = NULL
)
test_module3 <- module(
  label = "test3",
  ui = function(id) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("3")),
  datanames = NULL
)
test_module4 <- module(
  label = "test4",
  ui = function(id) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("4")),
  datanames = NULL
)
test_module_wdata <- function(datanames) {
  module(
    label = "with_data",
    ui = function(id) NULL,
    server = function(id, data) moduleServer(id, function(input, output, session) message("module with data")),
    datanames = datanames
  )
}

get_example_filtered_data <- function() {
  td <- teal.data::teal_data()
  td <- within(td, d1 <- data.frame(id = 1:5, pk = c(2, 3, 2, 1, 4), val = 1:5))
  td <- within(td, d2 <- data.frame(id = 1:5, value = 1:5))
  datanames(td) <- c("d1", "d2")
  teal.data::join_keys(td) <- teal.data::join_keys(teal.data::join_key("d1", "d2", c("pk" = "id")))
  teal_data_to_filtered_data(td)
}

# arguments -------
testthat::test_that("srv_teal_module accepts data being a reactive", {
  testthat::expect_no_error(
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal_data),
        modules = modules(test_module1)
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module throws error if data is not a reactive", {
  testthat::expect_error(
    srv_teal_module("id", data_rv = teal_data, modules = modules(test_module1)),
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
        modules = modules(test_module1)
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module throws error if modules are not teal_modules", {
  testthat::expect_error(
    srv_teal_module("id", data_rv = reactive(teal_data), modules = list(test_module1)),
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
        modules = modules(test_module1),
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
        modules = modules(test_module1),
        datasets = NULL
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module throws error if datasets are not reactive or null", {
  testthat::expect_error(
    srv_teal_module("id", data_rv = reactive(teal_data), modules = modules(test_module1), datasets = filtered_data),
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
        modules = modules(test_module1),
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
        modules = modules(test_module1),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module throws error if reporter is not inherited from class Reporter", {
  testthat::expect_error(
    srv_teal_module("id", data_rv = reactive(teal_data), modules = modules(test_module1), reporter = list()),
    "Must inherit from class 'Reporter'"
  )
})

# server -------
testthat::test_that(
  "srv_teal_module: datasets() is truthy and returns FilteredData only when data_rv() is truthy and returns teal_data",
  {
    testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactiveVal(NULL),
        modules = module(server = function(id, data) NULL, datanames = "iris")
      ),
      expr = {
        testthat::expect_null(data_rv())
        testthat::expect_error(datasets())

        data_rv("truthy but not teal_data")
        testthat::expect_error(datasets())

        data_rv(teal.data::teal_data(iris = iris))
        testthat::expect_s3_class(datasets(), "FilteredData")
      }
    )
  }
)

testthat::test_that("srv_teal_module: trigger_data remains NULL when datasets is not truthy", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactiveVal(teal.data::teal_data(iris = iris)),
      datasets = reactiveVal(NULL),
      modules = module(server = function(id, data) NULL, datanames = "iris")
    ),
    expr = {
      session$flushReact() # to trigger "visual observer" (renderUI)
      testthat::expect_null(trigger_data())
    }
  )
})

testthat::test_that("srv_teal_module: filtered_teal_data is created only when trigger_data changes from NULL", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) NULL, datanames = "iris")
    ),
    expr = {
      testthat::expect_null(trigger_data())
      testthat::expect_error(filtered_teal_data())
      session$flushReact() # to trigger "visual observer" (renderUI)
      testthat::expect_identical(trigger_data(), 1L)
      testthat::expect_s4_class(filtered_teal_data(), "teal_data")
    }
  )
})

testthat::test_that("srv_teal_module: data objects contain datanames = modules$datanames", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) NULL, datanames = "iris")
    ),
    expr = {
      session$flushReact() # to trigger "visual observer" (renderUI)
      testthat::expect_identical(active_datanames(), "iris")
      testthat::expect_identical(datasets()$datanames(), "iris")
      testthat::expect_identical(teal.data::datanames(filtered_teal_data()), "iris")
      testthat::expect_identical(ls(filtered_teal_data()@env), c("iris", "iris_raw"))
    }
  )
})

testthat::test_that("srv_teal_module: data code contains proprocessing, hashes and raw_data when data unfiltered", {
  teal_data_obj <- within(teal.data::teal_data(), {
    iris <- iris
    mtcars <- mtcars
  })

  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal_data_obj),
      modules = module(label = "test", server = function(id, data) NULL)
    ),
    expr = {
      session$flushReact() # to trigger "visual observer" (renderUI)
      rcode <- teal.code::get_code(filtered_teal_data())
      hash_calls <- lapply(c("iris", "mtcars"), function(dataname) {
        sprintf(
          "stopifnot(rlang::hash(%s) == \"%s\")",
          dataname,
          rlang::hash(filtered_teal_data()[[dataname]])
        )
      })
      testthat::expect_identical(
        rcode,
        paste(
          c(
            "iris <- iris",
            "mtcars <- mtcars",
            "",
            hash_calls,
            "iris_raw <- iris",
            "mtcars_raw <- mtcars"
          ),
          collapse = "\n"
        )
      )
    }
  )
})

testthat::test_that("srv_teal_module: active_datanames is resolved when modules$datanames = all or NULL", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) NULL, datanames = "all")
    ),
    expr = {
      session$flushReact() # to trigger "visual observer" (renderUI)
      testthat::expect_identical(active_datanames(), c("iris", "mtcars"))
      testthat::expect_identical(datasets()$datanames(), c("iris", "mtcars"))
      testthat::expect_identical(teal.data::datanames(filtered_teal_data()), c("iris", "mtcars"))
      testthat::expect_identical(ls(filtered_teal_data()@env), c("iris", "iris_raw", "mtcars", "mtcars_raw"))
    }
  )

  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) NULL, datanames = NULL)
    ),
    expr = {
      session$flushReact() # to trigger "visual observer" (renderUI)
      testthat::expect_identical(active_datanames(), c("iris", "mtcars"))
      testthat::expect_identical(datasets()$datanames(), c("iris", "mtcars"))
      testthat::expect_identical(teal.data::datanames(filtered_teal_data()), c("iris", "mtcars"))
      testthat::expect_identical(ls(filtered_teal_data()@env), c("iris", "iris_raw", "mtcars", "mtcars_raw"))
    }
  )
})

testthat::test_that("srv_teal_module: is called only once when trigger_data changes. Retrigger doesn't call module", {
  testServer(
    app = srv_teal_module,
    args = list(
      id = "test",
      data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
      modules = module(server = function(id, data) runif(1))
    ),
    expr = {
      testthat::expect_null(module_out())
      session$flushReact()
      number_1 <- module_out()
      testthat::expect_true(is.numeric(number_1))
      testthat::expect_true(obs_module$.destroyed)

      trigger_data(2L)
      session$flushReact()
      number_2 <- module_out()
      testthat::expect_identical(number_1, number_2)
    }
  )
})

out <- shiny::testServer(
  app = srv_teal_module,
  args = list(
    id = "test",
    data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
    modules = modules(
      modules(label = "tab1", test_module1, test_module2),
      modules(label = "tab2", test_module3, test_module4)
    ),
    reporter = teal.reporter::Reporter$new()
  ),
  expr = {
    testthat::test_that("modules_reactive is a list of reactives", {
      testthat::expect_is(modules_reactive, "list")
      testthat::expect_is(modules_reactive$tab1, "reactive")
      testthat::expect_is(modules_reactive$tab2, "reactive")
    })

    testthat::test_that("modules_reactive returns modules according to selection in the nested tabs", {
      session$setInputs(`tab1-active_tab` = "test2") # active tab in tab1
      session$setInputs(`tab2-active_tab` = "test3") # active tab in tab2
      nested_active_modules <- lapply(modules_reactive, function(child) child())
      testthat::expect_identical(nested_active_modules, list(tab1 = test_module2, tab2 = test_module3))

      session$setInputs(`tab1-active_tab` = "test1") # active tab in tab1
      session$setInputs(`tab2-active_tab` = "test4") # active tab in tab2
      nested_active_modules <- lapply(modules_reactive, function(child) child())
      testthat::expect_identical(nested_active_modules, list(tab1 = test_module1, tab2 = test_module4))
    })

    testthat::test_that("Change of this tab returns active module from this tab", {
      session$setInputs(`active_tab` = "tab1")
      testthat::expect_identical(get_active_module(), test_module1)

      session$setInputs(`active_tab` = "tab2")
      testthat::expect_identical(get_active_module(), test_module4)
    })
  }
)

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
      modules = modules(module),
      reporter = teal.reporter::Reporter$new()
    ),
    expr = {
      session$flushReact()
    }
  )
})

testthat::test_that("srv_teal_module.teal_module does pass data if in the args explicitly", {
  module <- module(
    server = function(id, data, ...) {
      moduleServer(id, function(input, output, session) {
        checkmate::assert_class(data, "reactive")
        checkmate::assert_class(data(), "teal_data")
      })
    },
    datanames = NULL
  )
  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = {
        session$setInputs()
      }
    )
  )
})

testthat::test_that("srv_teal_module.teal_module passes data to the server module", {
  module <- module(datanames = NULL, server = function(id, data) {
    moduleServer(id, function(input, output, session) checkmate::assert_list(data, "reactive"))
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module.teal_module passes (deprecated) datasets to the server module", {
  module <- lifecycle::expect_deprecated(
    module(server = function(id, datasets) {
      moduleServer(id, function(input, output, session) checkmate::assert_class(datasets, "FilteredData"))
    })
  )

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_teal_module.teal_module passes server_args to the ...", {
  server_args <- list(a = 1, b = 2)
  module <- module(server_args = server_args, server = function(id, ...) {
    moduleServer(id, function(input, output, session) stopifnot(identical(list(...), server_args)))
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})

fp_api <- teal.slice:::FilterPanelAPI$new(filtered_data)
testthat::test_that("srv_teal_module.teal_module doesn't pass filter_panel_api if not in the args explicitly", {
  module <- module(server = function(id, ...) {
    moduleServer(id, function(input, output, session) {
      checkmate::assert_false(
        tryCatch(
          checkmate::test_class(filter_panel_api, "FilterPanelAPI"),
          error = function(cond) FALSE
        )
      )
    })
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = {
        session$setInputs()
      }
    )
  )
})

testthat::test_that("srv_teal_module.teal_module passes filter_panel_api when passed in the args explicitly", {
  module <- module(server = function(id, filter_panel_api = fp_api, ...) {
    moduleServer(id, function(input, output, session) {
      checkmate::assert_class(filter_panel_api, "FilterPanelAPI")
    })
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = {
        session$setInputs()
      }
    )
  )
})

testthat::test_that("srv_teal_module.teal_module passes filter_panel_api to the server module", {
  module <- module(server = function(id, filter_panel_api) {
    moduleServer(id, function(input, output, session) checkmate::assert_class(filter_panel_api, "FilterPanelAPI"))
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_module,
      args = list(
        id = "test",
        data_rv = reactive(teal.data::teal_data(iris = iris, mtcars = mtcars)),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})
