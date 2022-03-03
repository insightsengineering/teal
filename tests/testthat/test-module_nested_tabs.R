filtered_data <- FilteredData$new()
filtered_data$set_dataset(teal.data::dataset(dataname = "iris", x = head(iris)))
test_module1 <- module(
  label = "test1",
  ui = function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) message("1")),
  filters = NULL
)
test_module2 <- module(
  label = "test2",
  ui = function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) message("2")),
  filters = NULL
)
test_module3 <- module(
  label = "test3",
  ui = function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) message("3")),
  filters = NULL
)
test_module4 <- module(
  label = "test4",
  ui = function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) message("4")),
  filters = NULL
)

# server -------
testthat::test_that("passed shiny module is initialized", {
  testthat::expect_message(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = modules(test_module1)
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
        )
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
    )
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
