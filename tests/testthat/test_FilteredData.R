library(random.cdisc.data)

context("FilteredData")

options(teal_logging = FALSE)

x <- teal:::FilteredData$new(datanames = c("ADSL", "ADAE"))

test_that(
  "Initialization is correct",
  expect_identical(x$datanames(), c("ADSL", "ADAE"))
)

ADSL <- radsl() # nolint
ADAE <- radae(ADSL) # nolint

x$set_data("ADSL", ADSL)
x$set_data("ADAE", ADAE)

test_that(
  "load and set_datasets", {
    expect_equal(x$get_data("ADSL"), ADSL)
    expect_equal(x$get_data("ADAE"), ADAE)

    expect_identical(x$datanames(), c("ADSL", "ADAE"))
  }
)

test_that(
  "data info", {

    expect_true(
      info = "list_data_info does not work as expected",
      is.null(x$list_data_info("ADSL", variables = c("AGE", "SEX")))
    )

  }
)

test_that(
  "set default filter state", {
    x$set_default_filter_state("ADSL", "AGE")

    expect_equal(names(x$get_filter_state("ADSL")), "AGE")
    expect_identical(x$get_filter_state("ADSL")$AGE, range(ADSL$AGE))

    x$set_default_filter_state("ADSL", "SEX")
    expect_equal(names(x$get_filter_state("ADSL")), c("AGE", "SEX"))
    expect_identical(x$get_filter_state("ADSL")$SEX, as.character(levels(ADSL$SEX)))

  }
)


test_that(
  "overwrite filter states", {

    x$set_filter_state("ADSL", state = list(AGE = range(ADSL$AGE) + c(+1, -1)))

    expect_equal(
      x$get_filter_state("ADSL")$AGE,
      range(ADSL$AGE) + c(+1, -1)
    )

    x$set_filter_state("ADSL", state = list(AGE = c(38, 40), SEX = "F"))

    expect_equal(
      x$get_data("ADSL", filtered = TRUE, reactive = FALSE),
      subset(ADSL, SEX == "F" & AGE >= 38 & AGE <= 40)
    )
  }
)


test_that(
  "reset filter states", {
    x$set_filter_state("ADSL", state = NULL)

    expect_equal(
      x$get_data("ADSL", filtered = TRUE, reactive = FALSE),
      ADSL
    )
  }
)
