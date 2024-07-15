# comment: srv_teal is exported so the tests here are extensive and cover srv_data as well.
#          testing of srv_data is not needed.

testthat::test_that("srv_teal accepts data to be teal_data", {
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

testthat::test_that("srv_teal accepts data to be teal_data_module returning reactive teal_data", {
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

testthat::test_that("srv_teal accepts data to a reactive or reactiveVal teal_data", {
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

testthat::test_that("srv_teal fails when data is not teal_data or teal_data_module", {
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

testthat::test_that("srv_teal: app fails when teal_data_module doesn't return a reactive", {
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

testthat::test_that("srv_teal: data_rv is NULL when data reactive returns an error", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = reactive(stop("Error")),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(data_rv())
    }
  )
})

testthat::test_that("srv_teal: data_rv is NULL when data reactive returns a validation error", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = reactive(validate(need(FALSE, "Error"))),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(data_rv())
    }
  )
})

testthat::test_that("srv_teal: data_rv is NULL when data reactive returns qenv.error", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = reactive(teal.data::teal_data() |> within(stop("Error"))),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(data_rv())
    }
  )
})

testthat::test_that("srv_teal: data_rv is NULL some modules uses datanames unavailable in data", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = reactive(teal.data::teal_data(iris = iris)),
      modules = modules(example_module(datanames = c("iris", "mtcars")))
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(data_rv())
    }
  )
})

testthat::test_that("srv_teal: datasets_rv is set only when filters are not module-specific", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      filter = teal_slices(module_specific = TRUE)
    ),
    expr = {
      session$flushReact()
      testthat::expect_null(datasets_rv)
    }
  )

  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
      # module specific is  FALSE by default
    ),
    expr = {
      testthat::expect_s3_class(datasets_rv, "reactive")
    }
  )
})


testthat::test_that("srv_teal: datasets_rv returns nothing when data_rv is NULL", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal.data::teal_data(iris = iris, mtcars = mtcars),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_null(data_rv())
      testthat::expect_error(datasets_rv())
    }
  )
})


testthat::test_that("srv_teal: datasets_rv returns FilteredData containing same datanames when data_rv is not null", {
  shiny::testServer(
    app = srv_teal,
    args = list(
      id = "test",
      data = teal.data::teal_data(iris = iris, mtcars = mtcars),
      modules = modules(example_module())
    ),
    expr = {
      session$flushReact()
      testthat::expect_s3_class(datasets_rv(), "FilteredData")
      testthat::expect_identical(datasets_rv()$datanames(), c("iris", "mtcars"))
    }
  )
})


testthat::test_that("srv_teal: slices_global is initialized with slices specified in filter", {
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

testthat::test_that("srv_teal: attr(slices_global, 'mapping') is resolved for global_filters  when !module_specific", {
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

testthat::test_that("srv_teal: attr(slices_global, 'mapping') is resolved for global_filters  when module_specific", {
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


# todo: if possible, test if the filters are set globally in slices_global (via srv_module_filter_manager)
