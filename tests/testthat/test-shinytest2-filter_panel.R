testthat::test_that("e2e: module content is updated when a data is filtered in filter panel", {
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

  app$wait_for_idle(timeout = default_idle_timeout)

  old_output <- app$get_active_module_output("text")

  app$set_active_filter_selection("iris", "Species", c("setosa", "versicolor"))

  testthat::expect_false(
    identical(old_output, app$get_active_module_output("text"))
  )

  app$stop()
})
