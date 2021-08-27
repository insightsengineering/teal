testthat::test_that("Queue can be initialized", {
  testthat::expect_error(ReactiveQueue$new(), NA)
})

testthat::test_that("Pushing an atomic value does not throw", {
  queue <- Queue$new()
  testthat::expect_error(queue$push(7), NA)
})

testthat::test_that("get returns the elements pushed to the queue", {
  queue <- Queue$new()
  queue$push(7)
  testthat::expect_equal(queue$get(), 7)
  queue$push(8)
  testthat::expect_equal(queue$get(), c(7, 8))
})

testthat::test_that("Pushing an array does not throw", {
  queue <- Queue$new()
  testthat::expect_error(queue$push(c(1, 2)), NA)
})

testthat::test_that("get returns the pushed array", {
  queue <- Queue$new()
  queue$push(c(1, 2))
  testthat::expect_equal(queue$get(), c(1, 2))
})

testthat::test_that("Pushing an R6 objecct does not throw", {
  mock_r6 <- R6::R6Class("test")
  queue <- Queue$new()
  testthat::expect_error(queue$push(mock_r6$new()), NA)
})

testthat::test_that("Get retrieves an array with the pushed R6 object", {
  mock_r6 <- R6::R6Class("test")
  queue <- Queue$new()
  queue$push(mock_r6$new())
  testthat::expect_equal(queue$get(), c(mock_r6$new()))
})

testthat::test_that("pop removes the oldest added element", {
  queue <- Queue$new()
  queue$push(c(7, 13))
  testthat::expect_equal(queue$pop(), 7)
  testthat::expect_equal(queue$get(), 13)
})

testthat::test_that("empty removes all elements from the queue", {
  queue <- Queue$new()
  queue$push(c(7, 13))
  queue$empty()
  testthat::expect_equal(queue$get(), c())
})

testthat::test_that("size returns the length of the queue", {
  queue <- Queue$new()
  testthat::expect_equal(queue$size(), 0)
  queue$push(1)
  testthat::expect_equal(queue$size(), 1)
  queue$push(c(1, 2, 3))
  testthat::expect_equal(queue$size(), 4)
  queue$pop()
  testthat::expect_equal(queue$size(), 3)
})

testthat::test_that("get(reversed = TRUE) returns the pushed elements from the youngest to the oldest", {
  queue <- Queue$new()
  queue$push(c(7, 13))
  testthat::expect_equal(queue$get(reversed = TRUE), c(13, 7))
})

testthat::test_that("Print method returns the name of the class, size and elements", {
  queue <- Queue$new()
  queue$push(c(7, 13))
  testthat::expect_output(queue$print(), regexp = "<Queue>\\nSize: 2\\nElements:\\n7 13")
})
