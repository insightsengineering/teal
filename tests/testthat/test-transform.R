test_that("transform requires a module", {
  expect_error(transform(example_module), "cannot coerce class ")
  expect_no_error(transform(example_module()))
})

test_that("transform requires functions for ui", {
  expect_error(transform(example_module(), ui = "a"), "ui should be a function")
  expect_no_error(transform(example_module(), ui = function(id) {
    ns <- NS(id)
  }))
  expect_no_error(transform(example_module(), ui = function(id, elem) {
    ns <- NS(id)
  }))
})

test_that("transform requires functions for server", {
  expect_error(transform(example_module(), server = "a"), "server should be a function")
  expect_no_error(transform(example_module(), server = function(data) {
    data
  }))
  expect_no_error(transform(example_module(), server = function(input, output, data, session) {
    data
  }))
})

test_that("transform returns a module", {
  expect_s3_class(transform(example_module()), "teal_module")
})
