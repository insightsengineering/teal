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
