test_that("data_extract_spec argument checking", {
  expect_error(
    data_extract_spec("toyDataset", select = NULL),
    "select, \"select_spec\"", fixed = TRUE
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

  filter_spec2 <- filter_spec(
    label = "Select parameter:",
    vars = "AVISIT",
    choices = c("BASELINE", "SCREENIG"),
    selected = "BASELINE",
    multiple = FALSE
  )

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
  ADSL <- data.frame(USUBJID = letters[1:10],  # nolint
                     SEX = sample(c("F", "M", "U"), 10, replace = T),
                     BMRKR1 = rnorm(10),
                     BMRKR2 = sample(c("L", "M", "H"), 10, replace = T),
                     stringsAsFactors = F)

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

  normal <- data_extract_spec(
    dataname = "ADSL",
    select = select_normal,
    filter = filter_normal
  )

  delayed <- data_extract_spec(
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

  expect_equal(class(delayed), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  expect_equal(class(mix1), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  expect_equal(class(mix2), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  expect_equal(class(mix3), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))

  expect_equal(names(normal), names(delayed))
  expect_equal(names(normal), names(mix1))
  expect_equal(names(normal), names(mix2))
  expect_equal(names(normal), names(mix3))

  ds <- teal:::FilteredData$new()
  ds$set_data("ADSL", ADSL)

  expect_identical(normal, resolve_delayed(delayed, ds))
  expect_identical(normal, resolve_delayed(mix1, ds))
  expect_identical(normal, resolve_delayed(mix2, ds))

  mix3_res <- resolve_delayed(mix3, ds)
  expect_identical(normal$filter[[1]], mix3_res$filter[[1]])
  expect_identical(normal$filter[[1]], mix3_res$filter[[2]])
  mix3_res$filter <- NULL
  normal$filter <- NULL
  expect_identical(normal, mix3_res)
})
