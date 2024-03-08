testthat::test_that("e2e: teal app with landing_popup initializes with no errors", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = modules(
      landing_popup_module(
        title = "Welcome",
        content = tags$b("A welcome message!", style = "color: red;")
      ),
      example_module()
    )
  )

  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()
  app$stop()
})
