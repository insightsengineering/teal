filtered_data <- teal.slice::init_filtered_data(
  list(iris = list(dataset = head(iris)))
)

test_module1 <- module(
  label = "test1",
  ui = function(id, ...) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("1")),
  filters = NULL
)
test_module2 <- module(
  label = "test2",
  ui = function(id) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("2")),
  filters = NULL
)
test_module3 <- module(
  label = "test3",
  ui = function(id) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("3")),
  filters = NULL
)
test_module4 <- module(
  label = "test4",
  ui = function(id) NULL,
  server = function(id) moduleServer(id, function(input, output, session) message("4")),
  filters = NULL
)

testthat::test_that("srv_nested_tabs throws error if reporter is not inherited from class Reporter", {
  testthat::expect_error(
    srv_nested_tabs(id, datasets = filtered_data, modules = modules(test_module1), reporter = list()),
    "inherits\\(reporter, \"Reporter\"\\) is not TRUE"
  )
})

# server -------
testthat::test_that("passed shiny module is initialized", {
  testthat::expect_message(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(test_module1),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    ),
    "1"
  )
})

testthat::test_that("nested teal-modules are initialized", {
  out <- testthat::capture_messages(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(
          modules(label = "tab1", test_module1, test_module2),
          modules(label = "tab2", test_module3, test_module4)
        ),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    )
  )
  testthat::expect_identical(out, c("1\n", "2\n", "3\n", "4\n"))
})

out <- shiny::testServer(
  app = srv_nested_tabs,
  args = list(
    id = "test",
    datasets = filtered_data,
    modules = modules(
      modules(label = "tab1", test_module1, test_module2),
      modules(label = "tab2", test_module3, test_module4)
    ),
    reporter = teal.reporter::Reporter$new()
  ),
  expr = {
    # to adjust input modules to the active modules (server_args is dropped when NULL)
    test_module1$server_args <- NULL
    test_module2$server_args <- NULL
    test_module3$server_args <- NULL
    test_module4$server_args <- NULL

    testthat::test_that("modules_reactive is a list of reactives", {
      expect_is(modules_reactive, "list")
      expect_is(modules_reactive$tab1, "reactive")
      expect_is(modules_reactive$tab2, "reactive")
    })

    testthat::test_that("modules_reactive returns modules according to selection in the nested tabs", {
      session$setInputs(`tab1-active_tab` = "test2") # active tab in tab1
      session$setInputs(`tab2-active_tab` = "test3") # active tab in tab2
      nested_active_modules <- lapply(modules_reactive, function(child) child())
      expect_identical(nested_active_modules, list(tab1 = test_module2, tab2 = test_module3))

      session$setInputs(`tab1-active_tab` = "test1") # active tab in tab1
      session$setInputs(`tab2-active_tab` = "test4") # active tab in tab2
      nested_active_modules <- lapply(modules_reactive, function(child) child())
      expect_identical(nested_active_modules, list(tab1 = test_module1, tab2 = test_module4))
    })

    testthat::test_that("Change of this tab returns active module from this tab", {
      session$setInputs(`active_tab` = "tab1")
      expect_identical(get_active_module(), test_module1)

      session$setInputs(`active_tab` = "tab2")
      expect_identical(get_active_module(), test_module4)
    })
  }
)

testthat::test_that("srv_nested_tabs.teal_module doesn't pass data if not in the args explicitly", {
  module <- module(server = function(id, ...) {
    moduleServer(id, function(input, output, session) checkmate::assert_list(data, "reactive"))
  })

  testthat::expect_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    ),
    "Assertion on 'data' failed"
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes data to the server module", {
  module <- module(server = function(id, data) {
    moduleServer(id, function(input, output, session) checkmate::assert_list(data, "reactive"))
  })

  testthat::expect_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    ),
    NA
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes datasets to the server module", {
  module <- module(server = function(id, datasets) {
    moduleServer(id, function(input, output, session) checkmate::assert_class(datasets, "FilteredData"))
  })

  testthat::expect_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    ),
    NA
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes server_args to the ...", {
  server_args <- list(a = 1, b = 2)
  module <- module(server_args = server_args, server = function(id, ...) {
    moduleServer(id, function(input, output, session) stopifnot(identical(list(...), server_args)))
  })

  testthat::expect_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    ),
    NA
  )
})

testthat::test_that("srv_nested_tabs.teal_module warns if both data and datasets are passed", {
  module <- module(label = "test module", server = function(id, datasets, data) {
    moduleServer(id, function(input, output, session) NULL)
  })

  testthat::expect_warning(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    ),
    "Module 'test module' has `data` and `datasets` arguments in the formals"
  )
})

fp_api <- teal.slice:::FilterPanelAPI$new(filtered_data)
testthat::test_that("srv_nested_tabs.teal_module doesn't pass filter_panel_api if not in the args explicitly", {
  module <- module(server = function(id, ...) {
    moduleServer(id, function(input, output, session) checkmate::assert_class(filter_panel_api, "FilterPanelAPI"))
  })

  testthat::expect_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    ),
    "object 'filter_panel_api' not found"
  )
})

testthat::test_that("srv_nested_tabs.teal_module passes filter_panel_api to the server module", {
  module <- module(server = function(id, filter_panel_api) {
    moduleServer(id, function(input, output, session) checkmate::assert_class(filter_panel_api, "FilterPanelAPI"))
  })

  testthat::expect_error(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(module),
        reporter = teal.reporter::Reporter$new()
      ),
      expr = NULL
    ),
    NA
  )
})


get_example_filtered_data <- function() {
  d1 <- data.frame(id = 1:5, pk = c(2, 3, 2, 1, 4), val = 1:5)
  d2 <- data.frame(id = 1:5, value = 1:5)

  cc <- teal.data:::CodeClass$new()
  cc$set_code("d1 <- data.frame(id = 1:5, pk = c(2,3,2,1,4), val = 1:5)", "d1")
  cc$set_code("d2 <- data.frame(id = 1:5, value = 1:5)", "d2")

  teal.slice::init_filtered_data(
    x = list(
      d1 = list(dataset = d1, keys = "id"),
      d2 = list(dataset = d2, keys = "id")
    ),
    join_keys = teal.data::join_keys(teal.data::join_key("d1", "d2", c("pk" = "id"))),
    code = cc,
    check = TRUE
  )
}

testthat::teat_that(".datasets_to_data returns filtered data", {
  datasets <- get_example_filtered_data()
  isolate(datasets$set_filter_state(list(d1 = list(val = list(selected = c(1, 2))))))
  module <- list(filter = "all")
  data <- isolate(.datasets_to_data(module, datasets))

  d1_filtered <- isolate(data[["d1"]]())
  testthat::expect_equal(d1_filtered, data.frame(id = 1:2, pk = 2:3, val = 1:2))
  d2_filtered <- isolate(data[["d2"]]())
  testthat::expect_equal(d2_filtered, data.frame(id = 1:5, value = 1:5))
})


testthat::teat_that(".datasets_to_data returns only data requested by modules$filter", {
  datasets <- get_example_filtered_data()
  module <- list(filter = "d1")
  data <- .datasets_to_data(module, datasets)
  testthat::expect_equal(isolate(names(data)), "d1")
})

testthat::teat_that(".datasets_to_data returns required attributes", {
  datasets <- get_example_filtered_data()
  module <- list(filter = "all")
  data <- .datasets_to_data(module, datasets)

  # join_keys
  testthat::expect_equal(
    attr(data, "join_keys"),
    teal.data::join_keys(teal.data::join_key("d1", "d2", c("pk" = "id")))
  )

  # code
  testthat::expect_equal(
    attr(data, "code")[1],
    "d1 <- data.frame(id = 1:5, pk = c(2, 3, 2, 1, 4), val = 1:5)\nd2 <- data.frame(id = 1:5, value = 1:5)\n\n"
  )
})

testthat::teat_that(".datasets_to_data returns parent datasets for CDISC data", {
  adsl <- data.frame(STUDYID = 1, USUBJID = 1)
  adae <- data.frame(STUDYID = 1, USUBJID = 1, ASTDTM = 1, AETERM = 1, AESEQ = 1)
  adtte <- data.frame(STUDYID = 1, USUBJID = 1, PARAMCD = 1)

  datasets <- teal.slice::init_filtered_data(
    teal.data::cdisc_data(
      teal.data::cdisc_dataset("ADSL", adsl),
      teal.data::cdisc_dataset("ADAE", adae),
      teal.data::cdisc_dataset("ADTTE", adtte)
    )
  )

  module <- list(filter = "ADAE")
  data <- .datasets_to_data(module, datasets)
  testthat::expect_setequal(isolate(names(data)), c("ADSL", "ADAE"))
})

