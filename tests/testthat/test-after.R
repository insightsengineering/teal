test_that("after requires a module for ui", {
  expect_error(after(example_module), "no applicable method for ")
  expect_no_error(after(example_module()))
})

test_that("after requires functions for ui", {
  expect_error(after(example_module(), ui = "a"), "ui should be a function")
  expect_no_error(after(example_module(), ui = function(id) {
    ns <- NS(id)
  }))
  expect_no_error(after(example_module(), ui = function(id, elem) {
    ns <- NS(id)
  }))
})

test_that("after requires functions for server", {
  expect_error(after(example_module(), server = "a"), "server should be a function")
  expect_no_error(after(example_module(), server = function(data) {
    data
  }))
  expect_no_error(after(example_module(), server = function(input, output, data, session) {
    data
  }))
})

test_that("after returns a module", {
  expect_s3_class(after(example_module()), "teal_module")
})
