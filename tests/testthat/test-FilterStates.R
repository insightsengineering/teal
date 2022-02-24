# Notice, the particular design of these tests. We don't test the particulars of
# the calls, but only whether they evaluate to the expected value.
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

testthat::test_that("get_call returns NULL after initialization if input_dataname is the same as output_dataname", {
  filter_states <- FilterStates$new(
    input_dataname = "test",
    output_dataname = "test",
    datalabel = "label"
  )
  testthat::expect_null(filter_states$get_call())
})

testthat::test_that("get_call returns a call binding the object output_dataname to input_dataname", {
  test_dataset <- 7
  filter_states <- FilterStates$new(
    input_dataname = "test_dataset",
    output_dataname = "output",
    datalabel = "label"
  )
  eval(filter_states$get_call())
  testthat::expect_equal(output, test_dataset)
})

testthat::test_that("get_call returns a call filtering a data.frame based on a RangeFilterState", {
  test_dataset <- as.data.frame(list(a = seq.int(0, 4, by = 1)))
  filter_states <- FilterStates$new(
    input_dataname = "test_dataset",
    output_dataname = "output",
    datalabel = "label"
  )
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  range_filter <- RangeFilterState$new(x = test_dataset$a, varname = "a")
  isolate(range_filter$set_selected(c(1, 3)))
  filter_states$queue_push(queue_index = 1, x = range_filter, element_id = "test")
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test_dataset[2:4, , drop = FALSE])
  testthat::expect_equal(isolate(filter_states$get_call()), quote(output <- subset(test_dataset, a >= 1 & a <= 3)))
})

testthat::test_that("get_call returns a call filtering a data.frame based on a ChoicesFilterState", {
  choices_dataset <- as.data.frame(list(choices = c("a", "b", "c")))
  filter_states <- FilterStates$new(
    input_dataname = "choices_dataset",
    output_dataname = "choices_output",
    datalabel = "label"
  )
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  choices_filter <- ChoicesFilterState$new(x = choices_dataset$choices, varname = "choices")
  isolate(choices_filter$set_selected(c("a", "c")))
  filter_states$queue_push(queue_index = 1, x = choices_filter, element_id = "test")
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(choices_output, choices_dataset[c(1, 3), , drop = FALSE])
})

testthat::test_that("get_call returns a call filtering a data.frame based on a LogicalFilterState", {
  logical_dataset <- as.data.frame(list(logical = c(TRUE, FALSE, FALSE)))
  filter_states <- FilterStates$new(
    input_dataname = "logical_dataset",
    output_dataname = "logical_output",
    datalabel = "label"
  )
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  logical_filter <- LogicalFilterState$new(x = logical_dataset$logical, varname = "logical")
  isolate(logical_filter$set_selected(FALSE))
  filter_states$queue_push(queue_index = 1, x = logical_filter, element_id = "test")
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(logical_output, logical_dataset[c(2, 3), , drop = FALSE])
})

testthat::test_that("get_call returns a call filtering a data.frame based on a DateFilterState", {
  date_dataset <- data.frame(date = seq(as.Date("2021/08/25"), by = "day", length.out = 3))
  filter_states <- FilterStates$new(
    input_dataname = "date_dataset",
    output_dataname = "date_output",
    datalabel = "label"
  )
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  date_filter <- DateFilterState$new(x = date_dataset$date, varname = "date")
  isolate(date_filter$set_selected(c("2021/08/25", "2021/08/26")))
  filter_states$queue_push(queue_index = 1, x = date_filter, element_id = "test")
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(date_output, date_dataset[c(1, 2), , drop = FALSE])
})

testthat::test_that("get_call returns a call filtering a data.frame based on a DatetimeFilterState", {
  datetime_dataset <- data.frame(
    datetime = seq(ISOdate(2021, 8, 25, tz = Sys.timezone()), by = "day", length.out = 3)
  )
  filter_states <- FilterStates$new(
    input_dataname = "datetime_dataset",
    output_dataname = "datetime_output",
    datalabel = "label"
  )

  filter_states$queue_initialize(list(ReactiveQueue$new()))
  datetime_filter <- DatetimeFilterState$new(x = datetime_dataset$datetime, varname = "datetime")
  isolate(datetime_filter$set_selected(rep(ISOdate(2021, 8, 27, tz = Sys.timezone()), 2)))
  filter_states$queue_push(queue_index = 1, x = datetime_filter, element_id = "test")
  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(datetime_output, datetime_dataset[c(3), , drop = FALSE])
})

testthat::test_that("get_call returns a call filtering a data.frame base on a combination of FilterState objects", {
  # setting up the test dataset
  test_dataset <- data.frame(
    a = c(seq(1, 5, by = 1)),
    choices = letters[1:5],
    logical = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    date = seq(as.Date("2021/08/25"), by = "day", length.out = 5),
    datetime = seq(ISOdate(2021, 8, 25, tz = Sys.timezone()), by = "day", length.out = 5)
  )

  # setting up filters
  filter_states <- FilterStates$new(
    input_dataname = "test_dataset",
    output_dataname = "output",
    datalabel = "label"
  )
  filter_states$queue_initialize(list(ReactiveQueue$new()))

  range_filter <- RangeFilterState$new(x = test_dataset$a, varname = "a")
  isolate(range_filter$set_selected(c(1, 3)))
  choices_filter <- ChoicesFilterState$new(x = test_dataset$choices, varname = "choices")
  isolate(choices_filter$set_selected(c("a", "c")))
  logical_filter <- LogicalFilterState$new(x = test_dataset$logical, varname = "logical")
  isolate(logical_filter$set_selected(FALSE))
  date_filter <- DateFilterState$new(x = test_dataset$date, varname = "date")
  isolate(date_filter$set_selected(c("2021/08/25", "2021/08/26")))
  datetime_filter <- DatetimeFilterState$new(x = test_dataset$datetime, varname = "datetime")
  isolate(datetime_filter$set_selected(rep(ISOdate(2021, 8, 25, tz = Sys.timezone()), 2)))


  filter_states$queue_push(queue_index = 1, x = range_filter, element_id = "test")
  filter_states$queue_push(queue_index = 1, x = choices_filter, element_id = "test")
  filter_states$queue_push(queue_index = 1, x = logical_filter, element_id = "test")
  filter_states$queue_push(queue_index = 1, x = date_filter, element_id = "test")
  filter_states$queue_push(queue_index = 1, x = datetime_filter, element_id = "test")

  eval(isolate(filter_states$get_call()))
  testthat::expect_equal(output, test_dataset[1, , drop = FALSE])
})

testthat::test_that("get_fun returns subset after initialization", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_equal(filter_states$get_fun(), "subset")
})

testthat::test_that("Emptying empty FilterStates does not throw", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(filter_states$queue_empty(), NA)
})

testthat::test_that("queue_get throws on a freshly initialized FilterStates object", {
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  testthat::expect_error(filter_states$queue_get(queue_index = 1), "ReactiveQueue 1 .* test")
})

testthat::test_that("The error message displays the queue index if datalabel is character(0)", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = character(0))
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  testthat::expect_error(
    filter_states$queue_get(7),
    regexp = "ReactiveQueue 7 has not been initialized in FilterStates object belonging to the dataset "
  )
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
    msg = "Assertion on 'x'"
  )
})

testthat::test_that("queue_get returns an empty list after queue_initialize with an empty queue", {
  reactive_queue <- ReactiveQueue$new()
  filter_states <- FilterStates$new(input_dataname = "test", "test", "test")
  filter_states$queue_initialize(list(reactive_queue))
  testthat::expect_equal(filter_states$queue_get(1), NULL)
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

testthat::test_that("Passing a FilterState to queue_push is the same as passing it in the list to queue_push", {
  filter_states_no_list <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states_no_list$queue_initialize(list(ReactiveQueue$new()))

  filter_states_list <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states_list$queue_initialize(list(ReactiveQueue$new()))

  filter_state <- FilterState$new("test", varname = "test")
  filter_states_no_list$queue_push(x = filter_state, queue_index = 1, element_id = "test")
  filter_states_list$queue_push(x = filter_state, queue_index = 1, element_id = "test")
  testthat::expect_equal(
    filter_states_no_list$queue_get(queue_index = 1),
    filter_states_list$queue_get(queue_index = 1)
  )
})

testthat::test_that("queue_get returns the list of FilterState objects", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  filter_state <- FilterState$new("test", varname = "test")
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")
  checkmate::expect_list(filter_states$queue_get(queue_index = 1), types = "FilterState")
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

testthat::test_that("queue_remove does not throw before initializing the queue", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  testthat::expect_error(filter_states$queue_remove(queue_index = 1, element_id = "test"))
})

testthat::test_that("queue_remove does not throw after initializing the queue", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  testthat::expect_error(filter_states$queue_remove(queue_index = 1, element_id = "test"), NA)
})

testthat::test_that("queue_remove does not throw after pushing an element to the initialized queue", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  filter_state <- FilterState$new("test", varname = "test")
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")
  testthat::expect_error(filter_states$queue_remove(queue_index = 1, element_id = "test"), NA)
})

testthat::test_that("FilterStates' queue is empty after pushing and removing an element from it", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  filter_state <- FilterState$new("test", varname = "test")
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")
  filter_states$queue_remove(queue_index = 1, element_id = "test")
  testthat::expect_length(filter_states$queue_get(1), 0)
})

testthat::test_that("FilterStates' queue is empty after queue_empty", {
  filter_states <- FilterStates$new(input_dataname = "test", output_dataname = "test", datalabel = "test")
  filter_states$queue_initialize(list(ReactiveQueue$new()))
  filter_state <- FilterState$new("test", varname = "test")
  filter_states$queue_push(x = filter_state, queue_index = 1, element_id = "test")
  filter_states$queue_empty()
  testthat::expect_length(filter_states$queue_get(1), 0)
})

testthat::test_that("data_choices_labeled returns an empty character array if choices are an empty array", {
  testthat::expect_equal(data_choices_labeled(7, choices = c()), character(0))
})

testthat::test_that("data_choices_labeled returns a choices_labeled object if choices are not empty", {
  testthat::expect_true(is(data_choices_labeled(data.frame(a = 1), choices = c("a")), "choices_labeled"))
})

testthat::test_that("data_choices_labeled returns names of the elements matching the choices", {
  testthat::expect_equal(data_choices_labeled(data.frame(a = 1, b = 2), choices = c("a"))[1], c("a: a" = "a"))
})

testthat::test_that("data_choices_labeled returns labels of the elements matching the choices
  if the varlabels are provided for the elements", {
  result <- unname(data_choices_labeled(list(a = 1, b = 2), choices = c("a"), varlabels = c(a = "labelA"))[1])
  testthat::expect_equal(result, "a")
})
