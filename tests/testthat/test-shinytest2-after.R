testthat::test_that("e2e: after adds wrapped to ids of modules", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module()
  )
  app_after <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = after(example_module())
  )

  before_ids <- app$get_values(input = TRUE, output = TRUE)
  after_ids <- app_after$get_values(input = TRUE, output = TRUE)

  expect_true(all(grep("wrapped", setdiff(names(after_ids$output), names(before_ids$output)), fixed = TRUE)))
  expect_true(all(grep("wrapped", setdiff(names(after_ids$input), names(before_ids$input)), fixed = TRUE)))

  app$stop()
})
