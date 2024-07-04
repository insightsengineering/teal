example_filter <- teal_slices(
  teal.slice::teal_slice(dataname = "iris", varname = "Sepal.Length"),
  teal.slice::teal_slice(dataname = "iris", varname = "Species"),
  teal.slice::teal_slice(dataname = "mtcars", varname = "mpg"),
  teal.slice::teal_slice(dataname = "women", varname = "height"),
  module_specific = TRUE,
  mapping = list(
    m1 = c("iris Sepal.Length"),
    m3 = c("women height"),
    global_filters = "iris Species"
  )
)

# needed as srv_filter_manager uses "callback" from modules through session$userData$module_slices_api
#   we need to set this mock value to test the srv_filter_manager
mock_module_available_slices <- function(filtered_data_list, slices_global) {
  sapply(names(filtered_data_list), simplify = FALSE, function(module_label) {
    fd_mod <- filtered_data_list[[module_label]]
    fd_mod$set_filter_state(
      .filter_module_slices(module_label = module_label, slices = slices_global())
    )
    fd_mod$set_available_teal_slices(slices_global)
    list(get_available_teal_slices = fd_mod$get_available_teal_slices())
  })
}

testthat::test_that(
  "srv_filter_manager returns table containing TRUE (active), FALSE (inactive), NA (unavailable) for each module",
  {
    # module-specific filtered data
    fd1 <- teal.slice::init_filtered_data(list(iris = iris))
    fd2 <- teal.slice::init_filtered_data(list(iris = iris, mtcars = mtcars))
    fd3 <- teal.slice::init_filtered_data(list(iris = iris, women = women))
    filtered_data_list <- list(m1 = fd1, m2 = fd2, m3 = fd3)

    # objects needed for srv_filter_manager
    slices_global <- .make_slices_global(filter = example_filter, module_labels = c("m1", "m2", "m3"))
    module_slices_api <- mock_module_available_slices(filtered_data_list, slices_global)

    shiny::testServer(
      app = srv_filter_manager,
      args = list(
        id = "test",
        slices_global = slices_global
      ),
      expr = {
        session$userData$module_slices_api <- module_slices_api
        testthat::expect_identical(
          mapping_table(),
          data.frame(
            m1 = c(TRUE, TRUE, NA, NA),
            m2 = c(FALSE, TRUE, FALSE, NA),
            m3 = c(FALSE, TRUE, NA, TRUE),
            row.names = c("iris Sepal.Length", "iris Species", "mtcars mpg", "women height")
          )
        )
      }
    )
  }
)

testthat::test_that(
  "srv_filter_manager returns table containing single column (global_filters) for global filter-panel",
  {
    # global filtering - same FilteredData object for each module
    fd <- teal.slice::init_filtered_data(list(iris = iris, mtcars = mtcars, women = women))
    filtered_data_list <- list(m1 = fd, m2 = fd, m3 = fd)
    attr(example_filter, "module_specific") <- FALSE

    # objects needed for srv_filter_manager
    slices_global <- .make_slices_global(filter = example_filter, module_labels = c("m1", "m2", "m3"))
    module_slices_api <- mock_module_available_slices(filtered_data_list, slices_global)

    shiny::testServer(
      app = srv_filter_manager,
      args = list(
        id = "test",
        slices_global = slices_global
      ),
      expr = {
        session$userData$module_slices_api <- module_slices_api
        testthat::expect_identical(
          mapping_table(),
          data.frame(
            `Global filters` = c(TRUE, TRUE, TRUE, TRUE),
            row.names = c("iris Sepal.Length", "iris Species", "mtcars mpg", "women height"),
            check.names = FALSE
          )
        )
      }
    )
  }
)

testthat::test_that(
  "srv_module_filter_manager initially sets filters from slices_global mapped to module",
  {
    fd <- teal.slice::init_filtered_data(list(iris = iris, mtcars = mtcars))
    slices_global <- .make_slices_global(filter = example_filter, module_labels = c("m1", "m2", "m3"))
    module_fd <- reactive(fd)

    shiny::testServer(
      app = srv_module_filter_manager,
      args = list(
        id = "m2",
        module_fd = module_fd,
        slices_global = slices_global
      ),
      expr = {
        expected_slices <- .filter_module_slices(module_label = "m2", slices = slices_global())
        testthat::expect_length(expected_slices, 1)
        testthat::expect_length(module_fd()$get_filter_state(), 0)

        # first reactive flush
        session$flushReact()
        module_slices <- module_fd()$get_filter_state()
        testthat::expect_true(
          length(setdiff_teal_slices(module_slices, expected_slices)) == 0 &&
            length(setdiff_teal_slices(expected_slices, module_slices)) == 0
        )
      }
    )
  }
)

testthat::test_that(
  "srv_module_filter_manager sets available filters to the module's states",
  {
    fd <- teal.slice::init_filtered_data(list(iris = iris, mtcars = mtcars))
    slices_global <- .make_slices_global(filter = example_filter, module_labels = c("m1", "m2", "m3"))
    module_fd <- reactive(fd)

    shiny::testServer(
      app = srv_module_filter_manager,
      args = list(
        id = "m2",
        module_fd = module_fd,
        slices_global = slices_global
      ),
      expr = {
        expected <- slices_global()[1:3] # iris and mtcars filters (women not available)
        testthat::expect_null(module_fd()$get_available_teal_slices()())

        # first reactive flush
        session$flushReact()
        res <- isolate(module_fd()$get_available_teal_slices()())
        testthat::expect_true(
          length(setdiff_teal_slices(res, expected)) == 0 &&
            length(setdiff_teal_slices(expected, res)) == 0
        )
      }
    )
  }
)

testthat::test_that(
  "srv_module_filter_manager sets session$userData$module_slices_api",
  {
    fd <- teal.slice::init_filtered_data(list(iris = iris, mtcars = mtcars))
    slices_global <- .make_slices_global(filter = example_filter, module_labels = c("m1", "m2", "m3"))
    module_fd <- reactive(fd)

    shiny::testServer(
      app = srv_module_filter_manager,
      args = list(
        id = "m2",
        module_fd = module_fd,
        slices_global = slices_global
      ),
      expr = {
        testthat::expect_null(session$userData$module_slices_api[["m2"]])
        session$flushReact()
        testthat::expect_identical(
          session$userData$module_slices_api[["m2"]]$get_available_teal_slices(),
          module_fd()$get_available_teal_slices()()
        )
      }
    )
  }
)

testthat::test_that(
  "srv_module_filter_manager filter set in the module is added to slices_global and mapped to the module",
  {
    fd <- teal.slice::init_filtered_data(list(iris = iris, mtcars = mtcars))
    slices_global <- .make_slices_global(filter = example_filter, module_labels = c("m1", "m2", "m3"))
    module_fd <- reactive(fd)

    shiny::testServer(
      app = srv_module_filter_manager,
      args = list(
        id = "m2",
        module_fd = module_fd,
        slices_global = slices_global
      ),
      expr = {
        session$flushReact()
        testthat::expect_identical(
          sapply(slices_global(), `[[`, "id"),
          c("iris Sepal.Length", "iris Species", "mtcars mpg", "women height")
        )
        module_fd()$set_filter_state(teal_slices(teal_slice(dataname = "mtcars", varname = "cyl")))
        session$flushReact()
        testthat::expect_identical(
          sapply(slices_global(), `[[`, "id"),
          c("iris Sepal.Length", "iris Species", "mtcars mpg", "women height", "mtcars cyl")
        )
        testthat::expect_identical(
          attr(slices_global(), "mapping"),
          list(
            m1 = c("iris Sepal.Length", "iris Species"),
            m2 = c("iris Species", "mtcars cyl"),
            m3 = c("women height", "iris Species")
          )
        )
      }
    )
  }
)


testthat::test_that(
  "srv_module_filter_manager filter set in the module is added to slices_global and mapped to the module",
  {
    fd <- teal.slice::init_filtered_data(list(iris = iris, mtcars = mtcars))
    slices_global <- .make_slices_global(filter = example_filter, module_labels = c("m1", "m2", "m3"))
    module_fd <- reactive(fd)

    shiny::testServer(
      app = srv_module_filter_manager,
      args = list(
        id = "m2",
        module_fd = module_fd,
        slices_global = slices_global
      ),
      expr = {
        session$flushReact()
        module_fd()$remove_filter_state(teal_slices(teal_slice(dataname = "iris", varname = "Species")))
        session$flushReact()
        testthat::expect_identical(
          attr(slices_global(), "mapping"),
          list(
            m1 = c("iris Sepal.Length", "iris Species"),
            m2 = character(0),
            m3 = c("women height", "iris Species")
          )
        )
      }
    )
  }
)

testthat::test_that(
  "srv_module_filter_manager changins slices_global and mapping resets module's states",
  {
    fd <- teal.slice::init_filtered_data(list(iris = iris, mtcars = mtcars))
    slices_global <- .make_slices_global(filter = example_filter, module_labels = c("m1", "m2", "m3"))
    module_fd <- reactive(fd)


    shiny::testServer(
      app = srv_module_filter_manager,
      args = list(
        id = "m2",
        module_fd = module_fd,
        slices_global = slices_global
      ),
      expr = {
        session$flushReact()
        testthat::expect_identical(
          sapply(module_fd()$get_filter_state(), `[[`, "id"),
          "iris Species"
        )

        new_slices_global <- c(
          slices_global(),
          teal_slices(teal_slice(dataname = "mtcars", varname = "cyl"))
        )
        attr(new_slices_global, "mapping")[["m2"]] <- c("mtcars cyl")
        slices_global(new_slices_global)
        session$flushReact()
        testthat::expect_identical(
          sapply(module_fd()$get_filter_state(), `[[`, "id"),
          "mtcars cyl"
        )
      }
    )
  }
)
