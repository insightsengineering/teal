filtered_data <- FilteredData$new()
filtered_data$set_dataset(Dataset$new("iris", head(iris)))
test_module1 <- module(
  label = "test1",
  ui =  function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) message("1")),
  filters = NULL
)
test_module2 <- module(
  label = "test2",
  ui =  function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) message("2")),
  filters = NULL
)
test_module3 <- module(
  label = "test3",
  ui =  function(id, ...) NULL,
  server = function(id, datasets) moduleServer(id, function(input, output, session) message("3")),
  filters = NULL
)
test_module4 <- module(
  label = "test4",
  ui =  function(id, ...) NULL,
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
        modules = root_modules(test_module1)
      ),
      expr = NULL
    ),
    "1"
  )
})

testthat::test_that("passed shiny 'moduleServer' is initialized", {
  testthat::expect_message(
    shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = root_modules(test_module1)
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
        modules = root_modules(
          modules("tab1", test_module1, test_module2),
          modules("tab2", test_module3, test_module4)
        )
      ),
      expr = NULL
    )
  )
  testthat::expect_identical(out, c("1\n", "2\n", "3\n", "4\n"))
})

testthat::test_that("", {
  out <- shiny::testServer(
    app = srv_nested_tabs,
    args = list(
      id = "test",
      datasets = filtered_data,
      modules = root_modules(test_module1)
    ),
    expr = {
      session$setInputs(root = "test1")
      expect_identical(get_active_module(modules, id_parent = NULL), test_module3)

      session$setInputs(root_tab2 = "test4")
      expect_identical(get_active_module(modules, id_parent = NULL), test_module4)

      session$setInputs(root = "tab1", root_tab1 = "test2")
      expect_identical(get_active_module(modules, id_parent = NULL), test_module2)
    }
  )
})

testthat::test_that("changing input value of tab name returns ", {
  out <- shiny::testServer(
      app = srv_nested_tabs,
      args = list(
        id = "test",
        datasets = filtered_data,
        modules = root_modules(
          modules("tab1", test_module1, test_module2),
          modules("tab2", test_module3, test_module4)
        )
      ),
      expr = {
        session$setInputs(root = "tab2", root_tab2 = "test3")
        expect_identical(get_active_module(modules, id_parent = NULL), test_module3)

        session$setInputs(root_tab2 = "test4")
        expect_identical(get_active_module(modules, id_parent = NULL), test_module4)

        session$setInputs(root = "tab1", root_tab1 = "test2")
        expect_identical(get_active_module(modules, id_parent = NULL), test_module2)
      }
    )
})
