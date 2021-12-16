options(teal_logging = FALSE)

ds <- teal:::CDISCFilteredData$new()

test_that("datanames() returns an empty character array after initialization", {
  expect_setequal(isolate(ds$datanames()), character(0))
})

adsl <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl$sex <- "F"
adae <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))


data <- cdisc_data(
  cdisc_dataset("ADSL", adsl),
  cdisc_dataset("ADAE", adae)
)

filtered_data_set(data, ds)

test_that("load and set_datasets", {
  expect_silent({
    testthat::expect_equal(ds$get_data("ADSL", filtered = FALSE), adsl)
    testthat::expect_equal(ds$get_data("ADAE", filtered = FALSE), adae)
  })
  expect_setequal(ds$datanames(), c("ADSL", "ADAE"))
})

test_that("set filter state", {
  filter_state_adsl <- ChoicesFilterState$new(adsl$sex, varname = "sex")
  filter_state_adsl$set_selected("F")

  queue <- ds$get_filtered_dataset("ADSL")$get_filter_states(1)
  queue$queue_push(filter_state_adsl, queue_index = 1L, element_id = "sex")

  expect_identical(
    isolate(queue$get_call()),
    quote(ADSL_FILTERED <- dplyr::filter(ADSL, sex == "F")) # nolint
  )
})

test_that("get_varlabels returns the column labels of the passed dataset", {
  rtables::var_labels(adsl) <- colnames(adsl)
  on.exit(ds$set_dataset(dataset("ADSL", adsl)))

  data <- adsl

  ds$set_dataset(dataset("ADSL", data))
  expect_equal(
    ds$get_varlabels("ADSL"),
    rtables::var_labels(adsl)
  )
  # only some variables
  expect_equal(
    ds$get_varlabels("ADSL", variables = c("sex")),
    rtables::var_labels(adsl)[c("sex")]
  )
})

test_that("get_filterable_varnames does not return child duplicates", {
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, a = 1L, b = 1L)
  )
  child <- cdisc_dataset(
    dataname = "ADTTE",
    parent = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, PARAMCD = 1L, a = 1L, c = 1L)
  )
  data <- cdisc_data(adsl, child)

  fd <- filtered_data_new(data)
  filtered_data_set(data, fd)

  expect_identical(
    fd$get_filterable_varnames("ADTTE"),
    c("PARAMCD", "c")
  )
})

test_that("get_filterable_varnames return all from parent dataset", {
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, a = 1L, b = 1L)
  )
  child <- cdisc_dataset(
    dataname = "ADTTE",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, PARAMCD = 1L, a = 1L, c = 1L)
  )
  data <- cdisc_data(adsl, child)

  fd <- filtered_data_new(data)
  filtered_data_set(data, fd)

  expect_identical(
    fd$get_filterable_varnames("ADSL"),
    c("USUBJID", "STUDYID", "a", "b")
  )
})

test_that("set_filter_state returns warning when setting a filter on a column which belongs to parent dataset", {
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, a = 1L, b = 1L)
  )
  child <- cdisc_dataset(
    dataname = "ADTTE",
    parent = "ADSL",
    x = data.frame(USUBJID = 1L, STUDYID = 1L, PARAMCD = 1L, a = 1L, c = 1L)
  )
  data <- cdisc_data(adsl, child)

  fd <- filtered_data_new(data)
  filtered_data_set(data, fd)
  testthat::expect_warning(
    fd$set_filter_state(list(ADTTE = list(USUBJID = "1"))),
    "These columns filters were excluded: USUBJID from dataset ADTTE"
  )
})
