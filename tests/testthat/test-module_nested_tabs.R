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


testthat::test_that("srv_nested_tabs throws error if reporter is not inherited from class Reporter", {
  testthat::expect_error(
    srv_nested_tabs(id, datasets = filtered_data, modules = modules(test_module1), reporter = list()),
    "Must inherit from class 'Reporter'"
  )
})

# server -------
testthat::test_that("passed shiny module is initialized only when the UI is triggered", {
  # module not initialized
  testthat::expect_silent(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(test1 = filtered_data),
        modules = modules(test_module1),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )

  # module initialized
  testthat::expect_message(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(test1 = filtered_data),
        modules = modules(test_module1),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = {
        session$setInputs()
      }
    ),
    "1"
  )
})

testthat::test_that("nested teal-modules are initialized when the UI is triggered", {
  # modules not initialized
  testthat::expect_silent(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(
          tab1 = list(test1 = filtered_data, test2 = filtered_data),
          tab2 = list(test3 = filtered_data, test4 = filtered_data)
        ),
        modules = modules(
          modules(label = "tab1", test_module1, test_module2),
          modules(label = "tab2", test_module3, test_module4)
        ),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )

  # modules initialized
  out <- testthat::capture_messages(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(
          tab1 = list(test1 = filtered_data, test2 = filtered_data),
          tab2 = list(test3 = filtered_data, test4 = filtered_data)
        ),
        modules = modules(
          modules(label = "tab1", test_module1, test_module2),
          modules(label = "tab2", test_module3, test_module4)
        ),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = {
        session$setInputs()
      }
    )
  )
  testthat::expect_identical(out, c("1\n", "2\n", "3\n", "4\n"))
})

out <- shiny::testServer(
  app = srv_nested_tabs,
  args = list(
    id = "test",
    datasets = list(
      tab1 = list(test1 = filtered_data, test2 = filtered_data),
      tab2 = list(test3 = filtered_data, test4 = filtered_data)
    ),
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

testthat::test_that("srv_nested_tabs.teal_module does not pass data if not in the args explicitly", {
  module <- module(server = function(id, ...) {
    moduleServer(id, function(input, output, session) {
      testthat::expect_null(list(...)$data)
    })
  })

  shiny::testServer(
    app = srv_nested_tabs,
    args = list(
      id = "test",
      datasets = list(module = filtered_data),
      modules = modules(module),
      reporter = teal.reporter::Reporter$new()
    ),
    expr = {
      session$setInputs()
    }
  )
})

testthat::test_that("srv_nested_tabs.teal_module does pass data if in the args explicitly", {
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
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(module = filtered_data),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = {
        session$setInputs()
      }
    )
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes data to the server module", {
  module <- module(datanames = NULL, server = function(id, data) {
    moduleServer(id, function(input, output, session) checkmate::assert_list(data, "reactive"))
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(module = filtered_data),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes (deprecated) datasets to the server module", {
  module <- lifecycle::expect_deprecated(
    module(server = function(id, datasets) {
      moduleServer(id, function(input, output, session) checkmate::assert_class(datasets, "FilteredData"))
    })
  )

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(module = filtered_data),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes server_args to the ...", {
  server_args <- list(a = 1, b = 2)
  module <- module(server_args = server_args, server = function(id, ...) {
    moduleServer(id, function(input, output, session) stopifnot(identical(list(...), server_args)))
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(module = filtered_data),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})

fp_api <- teal.slice:::FilterPanelAPI$new(filtered_data)
testthat::test_that("srv_nested_tabs.teal_module doesn't pass filter_panel_api if not in the args explicitly", {
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
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(module = filtered_data),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = {
        session$setInputs()
      }
    )
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes filter_panel_api when passed in the args explicitly", {
  module <- module(server = function(id, filter_panel_api = fp_api, ...) {
    moduleServer(id, function(input, output, session) {
      checkmate::assert_class(filter_panel_api, "FilterPanelAPI")
    })
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(module = filtered_data),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = {
        session$setInputs()
      }
    )
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes filter_panel_api to the server module", {
  module <- module(server = function(id, filter_panel_api) {
    moduleServer(id, function(input, output, session) checkmate::assert_class(filter_panel_api, "FilterPanelAPI"))
  })

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = list(module = filtered_data),
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
})


testthat::test_that(".datasets_to_data returns data which is filtered", {
  datasets <- get_example_filtered_data()
  datasets$set_filter_state(
    teal.slice::teal_slices(
      teal.slice::teal_slice(dataname = "d1", varname = "val", selected = c(1, 2))
    )
  )
  module <- test_module_wdata(datanames = c("d1", "d2"))
  data <- shiny::isolate(.datasets_to_data(module, datasets))

  d1_filtered <- data[["d1"]]
  testthat::expect_equal(d1_filtered, data.frame(id = 1:2, pk = 2:3, val = 1:2))
  d2_filtered <- data[["d2"]]
  testthat::expect_equal(d2_filtered, data.frame(id = 2:3, value = 2:3))
})


testthat::test_that(".datasets_to_data returns only data requested by modules$datanames", {
  datasets <- get_example_filtered_data()
  module <- test_module_wdata(datanames = "d1")
  data <- shiny::isolate(.datasets_to_data(module, datasets))
  testthat::expect_equal(datanames(data), "d1")
})

testthat::test_that(".datasets_to_data returns teal_data object", {
  datasets <- get_example_filtered_data()
  module <- test_module_wdata(datanames = c("d1", "d2"))
  data <- shiny::isolate(.datasets_to_data(module, datasets))

  testthat::expect_s4_class(data, "teal_data")

  # join_keys
  testthat::expect_equal(
    join_keys(data),
    teal.data::join_keys(teal.data::join_key("d1", "d2", c("pk" = "id")))
  )

  # code
  testthat::expect_equal(
    teal.code::get_code(data),
    paste(
      c(
        get_rcode_str_install(),
        get_rcode_libraries(),
        "d1 <- data.frame(id = 1:5, pk = c(2, 3, 2, 1, 4), val = 1:5)",
        "d2 <- data.frame(id = 1:5, value = 1:5)",
        "",
        "stopifnot(rlang::hash(d1) == \"f6f90d2c133ca4abdeb2f7a7d85b731e\")",
        "stopifnot(rlang::hash(d2) == \"6e30be195b7d914a1311672c3ebf4e4f\")",
        "",
        "d2 <- dplyr::inner_join(x = d2, y = d1[, c(\"pk\"), drop = FALSE], by = c(id = \"pk\"))",
        ""
      ),
      collapse = "\n"
    )
  )
})

testthat::test_that("calculate_hashes takes a FilteredData and vector of datanames as input", {
  adsl <- data.frame(STUDYID = 1, USUBJID = 1)
  adae <- data.frame(STUDYID = 1, USUBJID = 1, ASTDTM = 1, AETERM = 1, AESEQ = 1)
  adtte <- data.frame(STUDYID = 1, USUBJID = 1, PARAMCD = 1)

  datasets <- teal.slice::init_filtered_data(
    list(
      ADSL = list(dataset = adsl),
      ADAE = list(dataset = adae),
      ADTTE = list(dataset = adtte)
    )
  )

  testthat::expect_no_error(calculate_hashes(datanames = c("ADSL", "ADAE", "ADTTE"), datasets = datasets))
})

testthat::test_that("calculate_hashes returns a named list", {
  adsl <- data.frame(STUDYID = 1, USUBJID = 1)
  adae <- data.frame(STUDYID = 1, USUBJID = 1, ASTDTM = 1, AETERM = 1, AESEQ = 1)
  adtte <- data.frame(STUDYID = 1, USUBJID = 1, PARAMCD = 1)

  datasets <- teal.slice::init_filtered_data(
    list(
      ADSL = list(dataset = adsl),
      ADAE = list(dataset = adae),
      ADTTE = list(dataset = adtte)
    )
  )

  hashes <- calculate_hashes(datanames = c("ADSL", "ADAE", "ADTTE"), datasets = datasets)
  testthat::expect_identical(
    hashes,
    list(
      "ADSL" = "e89f5271357822c78dd5cfddb60c0a95",
      "ADAE" = "f71b576ecfd23075f7285841327515e0",
      "ADTTE" = "c68c01c86b946a3dfe05150da040aa2a"
    )
  )
  testthat::expect_is(hashes, "list")
  testthat::expect_named(hashes)
})

testthat::test_that("calculate_hashes returns the hash of the non Filtered dataset", {
  datasets <- teal.slice::init_filtered_data(
    list(iris = list(dataset = iris))
  )

  fs <- teal.slice::teal_slices(
    teal.slice::teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4)),
    teal.slice::teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"))
  )

  shiny::isolate(datasets$set_filter_state(state = fs))

  hashes <- calculate_hashes(datanames = c("iris"), datasets = datasets)
  testthat::expect_identical(hashes, list("iris" = "34844aba7bde36f5a34f6d8e39803508"))
  testthat::expect_false(hashes == rlang::hash(shiny::isolate(datasets$get_data("iris", filtered = TRUE))))
})
