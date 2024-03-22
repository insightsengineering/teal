testthat::test_that("manager modules return expected values", {
  filter <- teal_slices(
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

  fd1 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))
  fd2 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris), mtcars = list(dataset = mtcars)))
  fd3 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris), women = list(dataset = women)))
  # nolint start: line_length.
  set_filter_state(fd1, filter[shiny::isolate(vapply(filter, `[[`, character(1L), "id")) %in% unlist(attr(filter, "mapping")[c("m1", "global_filters")])])
  set_filter_state(fd1, filter[shiny::isolate(vapply(filter, `[[`, character(1L), "id")) %in% unlist(attr(filter, "mapping")[c("m1", "global_filters")])])
  set_filter_state(fd2, filter[shiny::isolate(vapply(filter, `[[`, character(1L), "id")) %in% unlist(attr(filter, "mapping")[c("m2", "global_filters")])])
  set_filter_state(fd3, filter[shiny::isolate(vapply(filter, `[[`, character(1L), "id")) %in% unlist(attr(filter, "mapping")[c("m3", "global_filters")])])
  # nolint end: line_length.
  datasets <- list(m1 = fd1, tabs = list(m2 = fd2, m3 = fd3))


  shiny::testServer(
    app = wunder_bar_srv,
    args = list(
      id = "wunder_bar_test",
      datasets = datasets,
      filter = filter
    ),
    expr = {
      testthat::context("filter manager returns slices_global as reactiveVal with teal_slices")
      testthat::expect_s3_class(filter_manager_results[["slices_global"]], "reactiveVal")
      testthat::expect_s3_class(filter_manager_results[["slices_global"]](), "teal_slices")
      testthat::expect_equal(
        filter_manager_results[["slices_global"]](),
        filter
      )

      testthat::context("filter manager returns mapping_matrix as reactive with data.frame")
      testthat::expect_s3_class(filter_manager_results[["mapping_matrix"]], "reactive")
      testthat::expect_s3_class(filter_manager_results[["mapping_matrix"]](), "data.frame")
      mapping_matrix_expected <- data.frame(
        row.names = c("iris Sepal.Length", "iris Species", "mtcars mpg", "women height"),
        m1 = c(TRUE, TRUE, NA, NA),
        m2 = c(FALSE, TRUE, FALSE, NA),
        m3 = c(FALSE, TRUE, NA, TRUE)
      )
      testthat::expect_equal(
        filter_manager_results[["mapping_matrix"]](),
        mapping_matrix_expected
      )

      testthat::context("filter manager returns datasets_flat as flat list of FilteredData objects")
      testthat::expect_true(is.list(filter_manager_results[["datasets_flat"]]), info = "datasets_flat is a list")
      testthat::expect_named(filter_manager_results[["datasets_flat"]])
      datasets_flat_classes <- lapply(filter_manager_results[["datasets_flat"]], class)
      testthat::expect_true(
        all(vapply(datasets_flat_classes, identical, logical(1L), c("FilteredData", "R6"))),
        info = "datasets_flat contains only FilteredData objects"
      )
      testthat::expect_equal(
        filter_manager_results[["datasets_flat"]],
        list(m1 = fd1, m2 = fd2, m3 = fd3)
      )

      testthat::context("snapshot manager returns snapshot history as list containing unlisted teal_slices")
      testthat::expect_equal(
        snapshot_history(),
        list("Initial application state" = as.list(filter, recursive = TRUE))
      )

      testthat::context("bookmark manager returns bookmark history as (initially) empty list")
      testthat::expect_equal(
        bookmark_history(),
        list()
      )
    }
  )
})
