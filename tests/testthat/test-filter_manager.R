filter <- teal::teal_filters(
  teal.slice::filter_var(dataname = "iris", varname = "Sepal.Length"),
  teal.slice::filter_var(dataname = "iris", varname = "Species"),
  teal.slice::filter_var(dataname = "mtcars", varname = "mpg"),
  teal.slice::filter_var(dataname = "women", varname = "height"),
  mapping = list(
    m1 = c("iris Sepal.Length"),
    m3 = c("women height"),
    global_filters = "iris Species"
  )
)

testthat::test_that("filter_manager_srv initializes objects based on initial filter configuration", {
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

  shiny::testServer(
    app = filter_manager_srv,
    args = list(
      id = "test",
      filtered_data_list = filtered_data_list,
      filter = filter
    ),
    expr = {
      testthat::expect_named(filtered_data_list, c("m1", "m2", "m3"))

      testthat::expect_identical(slices_map$m1(), c("iris Sepal.Length", "iris Species"))
      testthat::expect_identical(slices_map$m2(), "iris Species")
      testthat::expect_identical(slices_map$m3(), c("women height", "iris Species"))

      testthat::expect_identical(slices_global(), filter)

      testthat::expect_identical(
        mapping_matrix(),
        as.matrix(
          data.frame(
            m1 = c(TRUE, TRUE, FALSE, FALSE),
            m2 = c(FALSE, TRUE, FALSE, FALSE),
            m3 = c(FALSE, TRUE, FALSE, TRUE),
            row.names = c("iris Sepal.Length", "iris Species", "mtcars mpg", "women height")
          )
        )
      )
    }
  )
})
