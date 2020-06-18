library(random.cdisc.data)

context("FilteredData")

options(teal_logging = FALSE)

ds <- teal:::FilteredData$new()

test_that("Initialization is correct", {
  isolate(expect_setequal(ds$datanames(), character(0)))
})

ADSL <- radsl(cached = TRUE) #nolintr
attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
ADAE <- radae(cached = TRUE) #nolintr
attr(ADAE, "keys") <- get_cdisc_keys("ADAE")

isolate({
  ds$set_data("ADSL", ADSL)
  ds$set_data("ADAE", ADAE)

  test_that("load and set_datasets", {
      expect_equal(ds$get_data("ADSL", filtered = FALSE), ADSL)
      expect_equal(ds$get_data("ADAE", filtered = FALSE), ADAE)

      expect_setequal(ds$datanames(), c("ADSL", "ADAE"))
  })

  test_that("data info shows info", {
      expect_output(expect_true(
        is.null(ds$print_filter_info("ADSL", variables = c("AGE", "SEX")))
      ), "has filter type range")
  })

  test_that("set filter state works", {
    # since no filter was set before, will just take the default filter
    set_default_filter <- function(dataname, varname) {
      ds$set_filter_state(dataname, varname, state = ds$get_default_filter_state(dataname, varname))
    }
    expect_true(set_default_filter("ADSL", "AGE"))

    expect_equal(names(ds$get_filter_state("ADSL")), "AGE")
    expect_identical(ds$get_filter_state("ADSL")$AGE, list(range = range(ADSL$AGE), keep_na = FALSE))

    expect_error(
      # real range is (20, 69)
      ds$set_filter_state("ADSL", "AGE", list(range = c(-10, 110), keep_na = TRUE)),
      "full range"
    )

    expect_true(ds$set_filter_state("ADSL", "AGE", list(range = c(31, 50), keep_na = TRUE)))
    expect_true(ds$set_filter_state("ADSL", "AGE", list(range = c(30, 50), keep_na = TRUE)))
    expect_identical(ds$get_filter_state("ADSL")$AGE, list(range = c(30, 50), keep_na = TRUE))
    expect_false(ds$set_filter_state("ADSL", "AGE", list(range = c(30, 50), keep_na = TRUE)))
    expect_identical(ds$get_filter_state("ADSL")$AGE, list(range = c(30, 50), keep_na = TRUE))

    expect_true(set_default_filter("ADSL", "AGE"))
    expect_true(set_default_filter("ADSL", "SEX"))

    expect_setequal(names(ds$get_filter_state("ADSL")), c("AGE", "SEX"))
    # also counts are added after numbers
    expect_identical(
      ds$get_filter_state("ADSL")$SEX$choices,
      as.character(levels(ADSL$SEX))
    )

    ds$set_filter_state(
      "ADSL", varname = NULL,
      state = list(AGE = list(range = range(ADSL$AGE) + c(+1, -1), keep_na = TRUE))
    )
    expect_equal(
      ds$get_filter_state("ADSL")$AGE,
      list(range = range(ADSL$AGE) + c(+1, -1), keep_na = TRUE)
    )
  })

  test_that("check filtering", {
    ds$set_filter_state("ADSL", varname = NULL, state = list(
      AGE = list(range = c(38, 40), keep_na = TRUE),
      SEX = list(choices = "F", keep_na = TRUE)
    ))
    expect_equal(
      ds$get_data("ADSL", filtered = TRUE),
      dplyr::filter(ADSL, SEX == "F" & AGE >= 38 & AGE <= 40)
    )

    ds$set_filter_state("ADSL", "AGE", state = NULL)
    ds$set_filter_state("ADSL", "SEX", state = NULL)
    expect_equal(
      ds$get_data("ADSL", filtered = TRUE),
      ADSL
    )
  })

})
