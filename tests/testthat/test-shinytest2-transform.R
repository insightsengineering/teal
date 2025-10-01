testthat::test_that("e2e: transform adds wrapped to ids of modules", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module()
  )
  app_transform <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = transform(example_module())
  )

  before_ids <- app$get_values(input = TRUE, output = TRUE)
  transform_ids <- app_transform$get_values(input = TRUE, output = TRUE)

  expect_true(all(grep("wrapped", setdiff(names(transform_ids$output), names(before_ids$output)), fixed = TRUE)))
  expect_true(all(grep("wrapped", setdiff(names(transform_ids$input), names(before_ids$input)), fixed = TRUE)))

  app$stop()
  app_transform$stop()
})

testthat::test_that("e2e: transform modifies nested modules", {
  skip_if_too_deep(5)
  nested_modules <- modules(
    label = "Nested modules",
    example_module(label = "Module 1"),
    example_module(label = "Module 2")
  )
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = nested_modules
  )

  app_transform <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = transform(nested_modules)
  )

  before_ids <- app$get_values(input = TRUE, output = TRUE)
  transform_ids <- app_transform$get_values(input = TRUE, output = TRUE)

  new_input_ids <- setdiff(names(transform_ids$input), names(before_ids$input))
  module_1 <- grepl("module_1-module-wrapped", new_input_ids, fixed = TRUE)
  module_2 <- grepl("module_2-module-wrapped", new_input_ids, fixed = TRUE)

  expect_true(any(module_1), label = "Is 'Module 1' modified?")
  expect_true(any(module_2), label = "Is 'Module 2' modified?")
  expect_true(all(module_1 | module_2), label = "Are all modules modified?")

  app$stop()
  app_transform$stop()
})



testthat::test_that("e2e: transform modifies arbitrary modules structure", {
  skip_if_too_deep(5)
  nested_modules <- modules(
    label = "Nested modules",
    example_module(label = "Module 0"),
    modules(
      label = "Nested",
      example_module(label = "Module 1"),
      example_module(label = "Module 2")
    )
  )
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = nested_modules
  )

  app_transform <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = transform(nested_modules)
  )

  before_ids <- app$get_values(input = TRUE, output = TRUE)
  transform_ids <- app_transform$get_values(input = TRUE, output = TRUE)

  new_input_ids <- setdiff(names(transform_ids$input), names(before_ids$input))
  module_0 <- grepl("module_0-module-wrapped", new_input_ids, fixed = TRUE)
  module_1 <- grepl("module_1-module-wrapped", new_input_ids, fixed = TRUE)
  module_2 <- grepl("module_2-module-wrapped", new_input_ids, fixed = TRUE)

  expect_true(any(module_0), label = "Is 'Module 0' modified?")
  expect_true(any(module_1), label = "Is 'Module 1' modified?")
  expect_true(any(module_2), label = "Is 'Module 2' modified?")
  expect_true(all(module_0 | module_1 | module_2), label = "Are all modules modified?")

  app$stop()
  app_transform$stop()
})
