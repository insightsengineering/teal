library(scda)

# covr adds extra lines of code into the code base
# to keep track of coverage but these are included
# in the deparsed get_filter_expr string!
# In the future this should be fixed in a general way but at
# the moment this is the only test case causing issues so for now we check the environment
# variable R_COVR (requires covr >= 3.0.0) to determine if we are running tests within
# covr and pull out the appropriate lines of the deparsed string to check
covr_deparse_fix <- function(covr_lines, normal_lines) {
  if (covr::in_covr())
    expected_lines <- covr_lines
  else
    expected_lines <- normal_lines
  return(expected_lines)
}

options(teal_logging = FALSE)

ds <- teal:::CDISCFilteredData$new()

test_that("Initialization is correct", {
  expect_setequal(isolate(ds$datanames()), character(0))
})

ADSL <- synthetic_cdisc_data("rcd_2021_05_05")$adsl #nolint
ADAE <- synthetic_cdisc_data("rcd_2021_05_05")$adae #nolint

ADSL$AGE[1:40] <- NaN
ADSL$AGE[41:50] <- NA
ADSL$AGE[51:100] <- Inf
ADSL$SEX[90:110] <- NA
ADAE$ASEQ[sample(seq_len(nrow(ADAE)), 100)] <- NA
ADAE$ASEQ[sample(seq_len(nrow(ADAE)), 100)] <- Inf

data <- cdisc_data(
  cdisc_dataset("ADSL", ADSL),
  cdisc_dataset("ADAE", ADAE)
)

filtered_data_set(data, ds)

test_that("load and set_datasets", {
  expect_silent({
    expect_equal(ds$get_data("ADSL", filtered = FALSE), ADSL)
    expect_equal(ds$get_data("ADAE", filtered = FALSE), ADAE)
  })
  expect_setequal(ds$datanames(), c("ADSL", "ADAE"))
})

test_that("set filter state", {
  filter_state_adsl <- teal:::init_filter_state(
    ADSL$SEX,
    varname = "SEX"
  )
  filter_state_adsl$set_selected("F")

  queue <- ds$get_filtered_datasets("ADSL")$get_filter_states(1)
  queue$queue_push(filter_state_adsl, queue_index = 1L, element_id = "SEX")

  expect_identical(
    isolate(deparse(queue$get_call())),
    'ADSL_FILTERED <- dplyr::filter(ADSL, SEX == "F")'
  )
})

test_that("precedence NA and Inf", {
  var <- c(NA, -Inf, 0, 1, 2, 3, Inf)
  expect_identical(
    is.infinite(var) | var >= 1 & var <= 2,
    c(NA, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )

  expect_identical(
    is.na(var) | var >= 1 & var <= 2,
    c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(
    var >= 1 & var <= 2,
    c(NA, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(
    is.na(var) | (var >= 1 & var <= 2 | is.infinite(var)),
    c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )

  expect_identical(
    is.na(var) | is.infinite(var) | var >= 1 & var <= 2,
    c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("column_labels works", {
  on.exit(ds$set_dataset(dataset("ADSL", ADSL)))

  data <- ADSL

  ## check with all labels provided
  # preconditions on ADSL so tests below work as expected
  stopifnot(
    all_false(rtables::var_labels(ADSL), is.na), # no NA label
    setequal(names(rtables::var_labels(ADSL)), colnames(ADSL)) # all variables have labels
  )
  ds$set_dataset(dataset("ADSL", data))
  expect_equal(
    ds$get_varlabels("ADSL"),
    rtables::var_labels(ADSL)
  )
  # only some variables
  expect_equal(
    ds$get_varlabels("ADSL", variables = c("AGE", "SEX")),
    rtables::var_labels(ADSL)[c("AGE", "SEX")]
  )
})
