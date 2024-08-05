testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: teal_slices filters are initialized when global filters are created", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module(label = "Module_1"),
      example_module(label = "Module_2")
    ),
    filter = teal_slices(
      teal_slice(id = "iris_species", dataname = "iris", varname = "Species", multiple = TRUE),
      teal_slice(id = "mtcars_cyl", dataname = "mtcars", varname = "cyl", selected = c(4, 6)),
      teal_slice(id = "mtcars_drat", dataname = "mtcars", varname = "drat", selected = c(3, 4)),
      teal_slice(id = "mtcars_gear", dataname = "mtcars", varname = "gear")
    )
  )

  testthat::expect_setequal(
    names(app$get_active_data_filters("iris")),
    "Species"
  )
  testthat::expect_setequal(
    names(app$get_active_data_filters("mtcars")),
    c("cyl", "drat", "gear")
  )
  testthat::expect_setequal(
    app$get_active_data_filters("iris")$Species,
    c("setosa", "versicolor", "virginica")
  )
  testthat::expect_setequal(
    app$get_active_data_filters("mtcars")$cyl,
    c("4", "6")
  )
  testthat::expect_setequal(
    app$get_active_data_filters("mtcars")$drat,
    c(3, 4)
  )
  testthat::expect_setequal(
    app$get_active_data_filters("mtcars")$gear,
    c("3", "4", "5")
  )
  app$stop()
})

testthat::test_that("e2e: teal_slices filters are initialized when module specific filters are created", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      example_module(label = "Module_1"),
      example_module(label = "Module_2")
    ),
    filter = teal_slices(
      teal_slice(id = "iris_species", dataname = "iris", varname = "Species", multiple = TRUE),
      teal_slice(id = "mtcars_cyl", dataname = "mtcars", varname = "cyl", selected = c(4, 6)),
      teal_slice(id = "mtcars_drat", dataname = "mtcars", varname = "drat", selected = c(3, 4)),
      teal_slice(id = "mtcars_gear", dataname = "mtcars", varname = "gear"),
      module_specific = TRUE,
      mapping = list(
        "Module_1" = c("iris_species", "mtcars_cyl"),
        "Module_2" = c("iris_species", "mtcars_drat", "mtcars_gear")
      )
    )
  )

  testthat::expect_setequal(
    names(app$get_active_data_filters("iris")),
    "Species"
  )
  testthat::expect_setequal(
    names(app$get_active_data_filters("mtcars")),
    "cyl"
  )
  testthat::expect_setequal(
    app$get_active_data_filters("iris")$Species,
    c("setosa", "versicolor", "virginica")
  )
  testthat::expect_setequal(
    app$get_active_data_filters("mtcars")$cyl,
    c("4", "6")
  )

  testthat::expect_null(app$get_active_data_filters("mtcars")$drat)
  testthat::expect_null(app$get_active_data_filters("mtcars")$gear)

  app$navigate_teal_tab("Module_2")

  testthat::expect_setequal(
    names(app$get_active_data_filters("iris")),
    "Species"
  )
  testthat::expect_setequal(
    names(app$get_active_data_filters("mtcars")),
    c("drat", "gear")
  )
  testthat::expect_setequal(
    app$get_active_data_filters("iris")$Species,
    c("setosa", "versicolor", "virginica")
  )
  testthat::expect_setequal(
    app$get_active_data_filters("mtcars")$drat,
    c(3, 4)
  )
  testthat::expect_setequal(
    app$get_active_data_filters("mtcars")$gear,
    c("3", "4", "5")
  )
  testthat::expect_null(app$get_active_data_filters("mtcars")$cyl)

  app$stop()
})
