library(teal)
library(testthat)
library(random.cdisc.data)

context("FilteredData")

options(teal_logging = FALSE)

x <- teal:::FilteredData$new(datanames = c("ASL", "AAE"))

test_that(
  "Initialization is correct",
  expect_identical(x$datanames(), c("ASL", "AAE"))
)

ADSL <- radsl() # nolint
ADAE <- radae(ADSL) # nolint

x$set_data("ASL", ADSL)
x$set_data("AAE", ADAE)

test_that(
  "load and set_datasets", {
    expect_identical(x$get_data("ASL"), ADSL)
    expect_identical(x$get_data("AAE"), ADAE)

    expect_identical(x$datanames(), c("ASL", "AAE"))
  }
)

test_that(
  "data info", {

    expect_true(
      info = "list_data_info does not work as expected",
      is.null(x$list_data_info("ASL", variables = c("AGE", "SEX")))
    )

  }
)

test_that(
  "set default filter state", {
    x$set_default_filter_state("ASL", "AGE")

    expect_equal(names(x$get_filter_state("ASL")), "AGE")
    expect_identical(x$get_filter_state("ASL")$AGE, range(ADSL$AGE))

    x$set_default_filter_state("ASL", "SEX")
    expect_equal(names(x$get_filter_state("ASL")), c("AGE", "SEX"))
    expect_identical(x$get_filter_state("ASL")$SEX, as.character(levels(ADSL$SEX)))

  }
)


test_that(
  "overwrite filter states", {

    x$set_filter_state("ASL", state = list(AGE = range(ADSL$AGE) + c(+1, -1)))

    expect_equal(
      x$get_filter_state("ASL")$AGE,
      range(ADSL$AGE) + c(+1, -1)
    )

    x$set_filter_state("ASL", state = list(AGE = c(38, 40), SEX = "F"))

    expect_identical(
      x$get_data("ASL", filtered = TRUE, reactive = FALSE),
      subset(ADSL, SEX == "F" & AGE >= 38 & AGE <= 40)
    )
  }
)


test_that(
  "reset filter states", {
    x$set_filter_state("ASL", state = NULL)

    expect_identical(
      x$get_data("ASL", filtered = TRUE, reactive = FALSE),
      ADSL
    )
  }
)
