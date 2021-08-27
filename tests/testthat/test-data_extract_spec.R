library(random.cdisc.data)

test_that("data_extract_spec argument checking", {
  expect_error(
    data_extract_spec("toyDataset", select = NULL),
    "Either select or filter should be not empty", fixed = TRUE
  )
  expect_error(
    data_extract_spec("toyDataset", select = c("A", "B")),
    "select, \"select_spec\"", fixed = TRUE
  )
})

test_that("data_extract_spec works with valid input", {
  # the dataset does not exist, so we just check if the combinations are accepted
  # we add 1 to the var names to avoid confusion with their respective functions

  select_spec1 <- select_spec(
    label = "Select variable:",
    choices = c("SEX", "RACE"),
    selected = "SEX",
    multiple = FALSE,
    fixed = FALSE
  )
  data_extract_spec1 <- expect_silent(data_extract_spec(
    "toyDataset",
    select = select_spec1
  ))
  expect_identical(data_extract_spec1$select, select_spec1)
  expect_identical(class(data_extract_spec1), "data_extract_spec")

  expect_identical(
    expect_silent(data_extract_spec(
      "toyDataset",
      select = select_spec1
    )),
    expect_silent(data_extract_spec(
      "toyDataset",
      select = select_spec1,
      filter = NULL
    ))
  )

  # with filter
  select_spec1 <- select_spec(
    label = "Select variable:",
    choices = c("AVAL", "CNSR"),
    selected = "AVAL",
    multiple = FALSE,
    fixed = FALSE
  )
  filter_spec1 <- filter_spec(
    label = "Select parameter:",
    vars = "PARAMCD",
    choices = c("OS", "PFS"),
    selected = "PFS",
    multiple = FALSE
  )
  filter_spec1$dataname <- "ADTTE"

  filter_spec2 <- filter_spec(
    label = "Select parameter:",
    vars = "AVISIT",
    choices = c("BASELINE", "SCREENIG"),
    selected = "BASELINE",
    multiple = FALSE
  )
  filter_spec2$dataname <- "ADTTE"

  data_extract_spec1 <- expect_silent(data_extract_spec(
    dataname = "ADTTE",
    select = select_spec1,
    filter = filter_spec1
  ))
  expect_identical(data_extract_spec1$select, select_spec1)

  expect_identical(data_extract_spec1$filter, list(filter_spec1))

  data_extract_spec2 <- expect_silent(data_extract_spec(
    dataname = "ADTTE",
    select = select_spec1,
    filter = list(filter_spec1, filter_spec2)
  ))

  expect_identical(data_extract_spec2$select, select_spec1)
  expect_identical(data_extract_spec2$filter, list(filter_spec1, filter_spec2))

  # with reshape (only makes sense when filter is there)
  filter_spec1 <- filter_spec(
    label = "Select parameter:",
    vars = "PARAMCD",
    choices = c("OS", "PFS", "OS2"),
    selected = c("OS", "PFS"),
    multiple = TRUE
  )
  filter_spec1$dataname <- "ADTTE"
  data_extract_spec1 <- expect_silent(data_extract_spec(
    dataname = "ADTTE",
    select = select_spec1,
    filter = filter_spec1,
    reshape = TRUE
  ))
  expect_identical(data_extract_spec1$select, select_spec1)
  expect_identical(data_extract_spec1$filter, list(filter_spec1))
  expect_identical(data_extract_spec1$reshape, TRUE)
})

test_that("delayed data_extract_spec works", {
  set.seed(1)
  ADSL <- data.frame( # nolint
    USUBJID = letters[1:10],
    SEX = sample(c("F", "M", "U"), 10, replace = T),
    BMRKR1 = rnorm(10),
    BMRKR2 = sample(c("L", "M", "H"), 10, replace = T),
    stringsAsFactors = FALSE)
  attr(ADSL, "keys") <- get_cdisc_keys("ADSL")

  filter_normal <- filter_spec(
    vars = variable_choices(ADSL, "SEX"),
    sep = "-",
    choices = value_choices(ADSL, "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  filter_delayed <- filter_spec(
    vars = variable_choices("ADSL", "SEX"),
    sep = "-",
    choices = value_choices("ADSL", "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  select_normal <- select_spec(
    choices = variable_choices(ADSL, c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  select_delayed <- select_spec(
    choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  expected_spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_normal,
    filter = filter_normal
  )

  # obtained via delayed approach
  delayed_spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = filter_delayed
  )

  mix1 <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = filter_normal
  )

  mix2 <- data_extract_spec(
    dataname = "ADSL",
    select = select_normal,
    filter = filter_delayed
  )

  mix3 <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = list(filter_delayed, filter_normal)
  )

  expect_equal(class(delayed_spec), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  expect_equal(class(mix1), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  expect_equal(class(mix2), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  expect_equal(class(mix3), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))

  expect_equal(names(expected_spec), names(delayed_spec))
  expect_equal(names(expected_spec), names(mix1))
  expect_equal(names(expected_spec), names(mix2))
  expect_equal(names(expected_spec), names(mix3))

  ds <- teal:::CDISCFilteredData$new()
  isolate({
    ds$set_dataset(cdisc_dataset("ADSL", ADSL))
    expect_identical(expected_spec, resolve_delayed(delayed_spec, ds))
    expect_identical(expected_spec, resolve_delayed(mix1, ds))
    expect_identical(expected_spec, resolve_delayed(mix2, ds))

    mix3_res <- resolve_delayed(mix3, ds)
  })

  expect_identical(expected_spec$filter[[1]], mix3_res$filter[[1]])
  expect_identical(expected_spec$filter[[1]], mix3_res$filter[[2]])
  mix3_res$filter <- NULL
  expected_spec$filter <- NULL
  expect_identical(expected_spec, mix3_res)
})

adsl <- radsl(cached = TRUE)
adtte <- radtte(cached = TRUE)
data <- cdisc_data(
  cdisc_dataset("ADSL", adsl),
  cdisc_dataset("ADTTE", adtte)
)

ds <- teal:::CDISCFilteredData$new()
isolate(filtered_data_set(data, ds))

vc_hard <- variable_choices("ADSL", subset = c("STUDYID", "USUBJID"))
vc_hard_exp <- structure(
  list(data = "ADSL", subset = c("STUDYID", "USUBJID"), key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_hard_short <- variable_choices("ADSL", subset = "STUDYID")
vc_hard_short_exp <- structure(
  list(data = "ADSL", subset = "STUDYID", key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
vc_fun_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1:2], key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun_short <- variable_choices("ADSL", subset = function(data) colnames(data)[1])
vc_fun_short_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1], key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

testthat::test_that("delayed version of data_extract_spec", {
  # hard-coded subset
  obj <- data_extract_spec(
    "ADSL",
    select = select_spec(vc_hard, selected = vc_hard_short, multiple = FALSE),
    filter = filter_spec(
      vars = variable_choices("ADSL", subset = "ARMCD"),
      choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(adsl, c("STUDYID", "USUBJID"), key = get_cdisc_keys("ADSL")),
      selected = variable_choices(adsl, "STUDYID", key = get_cdisc_keys("ADSL"))),
    filter = filter_spec(
      vars = variable_choices(adsl, subset = "ARMCD", key = get_cdisc_keys("ADSL")),
      choices = value_choices(adsl, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(adsl, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  testthat::expect_equal(res_obj$select, exp_obj$select)
  testthat::expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  testthat::expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)


  # functional subset
  obj <- data_extract_spec(
    "ADSL",
    select = select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE),
    filter = filter_spec(
      vars = variable_choices("ADSL", subset = "ARMCD"),
      choices = value_choices(
        "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) c("ARM A", "ARM B")),
      selected = value_choices(
        "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) "ARM A"),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(adsl, c("STUDYID", "USUBJID"), key = get_cdisc_keys("ADSL")),
      selected = variable_choices(adsl, "STUDYID", key = get_cdisc_keys("ADSL"))),
    filter = filter_spec(
      vars = variable_choices(adsl, subset = "ARMCD", key = get_cdisc_keys("ADSL")),
      choices = value_choices(adsl, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(adsl, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  testthat::expect_equal(res_obj$select, exp_obj$select)
  testthat::expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  testthat::expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)
})
