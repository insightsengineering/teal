testthat::test_that("snapshot manager holds initial state in history", {
  filter <- teal_slices(
    teal.slice::teal_slice(dataname = "iris", varname = "Sepal.Length"),
    teal.slice::teal_slice(dataname = "iris", varname = "Species"),
    teal.slice::teal_slice(dataname = "mtcars", varname = "mpg"),
    teal.slice::teal_slice(dataname = "women", varname = "height"),
    mapping = list(
      m1 = c("iris Sepal.Length"),
      m3 = c("women height"),
      global_filters = "iris Species"
    )
  )

  fd1 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))
  fd2 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris), mtcars = list(dataset = mtcars)))
  fd3 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris), women = list(dataset = women)))
  filtered_data_list <- list(m1 = fd1, m2 = fd2, m3 = fd3)

  slices_global <- reactiveVal(shiny::isolate(filter))

  mapping_matrix <- reactive({
    module_states <- lapply(filtered_data_list, function(x) x$get_filter_state())
    mapping_ragged <- lapply(module_states, function(x) vapply(x, `[[`, character(1L), "id"))
    all_names <- vapply(slices_global(), `[[`, character(1L), "id")
    mapping_smooth <- lapply(mapping_ragged, is.element, el = all_names)
    as.data.frame(mapping_smooth, row.names = all_names, check.names = FALSE)
  })

  shiny::testServer(
    app = snapshot_manager_srv,
    args = list(
      id = "test",
      slices_global = slices_global,
      mapping_matrix = mapping_matrix,
      filtered_data_list = filtered_data_list
    ),
    expr = {
      testthat::expect_true("Initial application state" %in% names(snapshot_history()))

      snapshot <- snapshot_history()[["Initial application state"]]
      snapshot_state <- as.teal_slices(snapshot)

      testthat::expect_equal(as.list(snapshot_state, recursive = TRUE), as.list(filter, recursive = TRUE))
    }
  )
})
