testthat::test_that("ReactiveQueue can be initialized", {
  testthat::expect_error(ReactiveQueue$new(), NA)
})

testthat::test_that("Can push elements to the queue", {
  queue <- ReactiveQueue$new()
  testthat::expect_error(queue$push(7), NA)
})

testthat::test_that("get returns the elements pushed to the queue", {
  queue <- ReactiveQueue$new()
  queue$push(7)
  testthat::expect_equal(queue$get(), 7)
  queue$push(8)
  testthat::expect_equal(queue$get(), c(7, 8))
})

testthat::test_that("Pushing an array does not throw", {
  queue <- ReactiveQueue$new()
  testthat::expect_error(queue$push(c(1, 2)), NA)
})

testthat::test_that("get returns the pushed array", {
  queue <- ReactiveQueue$new()
  queue$push(c(1, 2))
  testthat::expect_equal(queue$get(), c(1, 2))
})
