filter_global <- teal_slices(
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
filter_modular <- teal_slices(
  teal.slice::teal_slice(dataname = "iris", varname = "Sepal.Length"),
  teal.slice::teal_slice(dataname = "iris", varname = "Species"),
  teal.slice::teal_slice(dataname = "mtcars", varname = "mpg"),
  teal.slice::teal_slice(dataname = "women", varname = "height"),
  module_specific = FALSE,
  mapping = list(
    m1 = c("iris Sepal.Length"),
    m3 = c("women height"),
    global_filters = "iris Species"
  )
)

testthat::test_that("filter_manager_srv initializes properly processes input arguments", {
  fd1 <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))
  fd2 <- teal.slice::init_filtered_data(
    list(iris = list(dataset = iris), mtcars = list(dataset = mtcars))
  )
  fd3 <- teal.slice::init_filtered_data(
    list(iris = list(dataset = iris), women = list(dataset = women))
  )
  filtered_data_list <- list(
    m1 = fd1,
    tab = list(m2 = fd2, m3 = fd3)
  )

  # global filtering
  shiny::testServer(
    app = filter_manager_srv,
    args = list(
      id = "test",
      filtered_data_list = filtered_data_list,
      filter = filter_global
    ),
    expr = {
      testthat::expect_named(filtered_data_list, c("m1", "m2", "m3"))

      testthat::expect_identical(slices_global(), filter)
    }
  )

  # modular filtering
  shiny::testServer(
    app = filter_manager_srv,
    args = list(
      id = "test",
      filtered_data_list = filtered_data_list,
      filter = filter_modular
    ),
    expr = {
      testthat::expect_named(filtered_data_list, "global_filters")

      testthat::expect_identical(slices_global(), filter)
    }
  )
})
