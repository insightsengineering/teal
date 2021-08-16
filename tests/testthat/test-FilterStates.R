testthat::test_that("The constructor accepts a call, name or string as input_dataname", {
  testthat::expect_error(
    FilterStates$new(input_dataname = "string", output_dataname = "test", datalabel = "test"),
    NA
  )
  testthat::expect_error(
    FilterStates$new(input_dataname = quote(name), output_dataname = "test", datalabel = "test"),
    NA
  )
  testthat::expect_error(
    FilterStates$new(input_dataname = call("call"), output_dataname = "test", datalabel = "test"),
    NA
  )
})

testthat::test_that("The constructor accepts a call, name or string as output_dataname", {
  testthat::expect_error(
    FilterStates$new(input_dataname = "test", output_dataname = "string", datalabel = "test"),
    NA
  )
  testthat::expect_error(
    FilterStates$new(input_dataname = "test", output_dataname = quote(name), datalabel = "test"),
    NA
  )
  testthat::expect_error(
    FilterStates$new(input_dataname = "test", output_dataname = call("call"), datalabel = "test"),
    NA
  )
})

testthat::test_that("get_call returns NULL after initialization", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_null(filter_states$get_call())
})

testthat::test_that("get_fun returns subset after initialization", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_equal(filter_states$get_fun(), "subset")
})

testthat::test_that("Emptying empty FilterStates does not throw", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(filter_states$queue_empty(), NA)
})

testthat::test_that("queue_get does not throw on a freshly initialized FilterStates object", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(filter_states$queue_get(queue_index = 1), "ReactiveQueue '1' .* 'test'")
  # testthat::expect_equal(filter_states$queue_get(), )
})

testthat::test_that("queue_initialize does not throw when passed a list of ReactiveQueue", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(filter_states$queue_initialize(list(ReactiveQueue$new())), NA)
  testthat::expect_error(filter_states$queue_initialize(list(x = ReactiveQueue$new())), NA)
})

testthat::test_that("queue_initialize throws an error when passed an empty list", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(
    filter_states$queue_initialize(list()),
    msg = "is_class_list(\"ReactiveQueue\")(x) is not TRUE"
    )
})

testthat::test_that("queue_get returns an empty list after queue_initialize with an empty queue", {
  reactive_queue <- ReactiveQueue$new()
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  filter_states$queue_initialize(list(reactive_queue))
  testthat::expect_equal(filter_states$queue_get(1), list())
})

testthat::test_that("queue_push throws before calling queue_initialize", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_state <- FilterState$new("test", varname = "test")
  testthat::expect_error(filter_states$queue_push(x = filter_state, queue_index = 1L, element_id = "test"))
})

testthat::test_that("queue_push does not throw after the queue was initialized if passed a numeric", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  filter_state <- FilterState$new("test", varname = "test")
  testthat::expect_error(filter_states$queue_push(x = filter_state, queue_index = 1L, element_id = "test"), NA)
  testthat::expect_error(filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test"), NA)
})

testthat::test_that("queue_push does not throw after the queue was initialized if passed a string", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  filter_state <- FilterState$new("test", varname = "test")
  testthat::expect_error(filter_states$queue_push(x = filter_state, queue_index = "1", element_id = "test"), NA)
})

testthat::test_that("queue_get returns the list with elements passed to queue_push", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  filter_state <- FilterState$new("test", varname = "test")
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")
  testthat::expect_equal(filter_states$queue_get(1)[[1]], filter_state)
})

testthat::test_that("Elements of the list returned by queue_get have names corresponding to varname", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  filter_state <- FilterState$new("test", varname = "test")
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")
  testthat::expect_equal(names(filter_states$queue_get(1)), as.character(filter_state$get_varname()))
})
