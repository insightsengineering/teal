library(random.cdisc.data)

context("FilteredData")

options(teal_logging = FALSE)

ds <- teal:::FilteredData$new()

test_that("Initialization is correct", {
  isolate(expect_setequal(ds$datanames(), character(0)))
})

ADSL <- radsl(cached = TRUE) #nolint
attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
ADAE <- radae(cached = TRUE) #nolint
attr(ADAE, "keys") <- get_cdisc_keys("ADAE")

isolate({
  ds$set_data("ADSL", ADSL)
  ds$set_data("ADAE", ADAE)

  test_that("load and set_datasets", {
    expect_silent({
      expect_equal(ds$get_data("ADSL", filtered = FALSE), ADSL)
      expect_equal(ds$get_data("ADAE", filtered = FALSE), ADAE)
    })
    expect_setequal(ds$datanames(), c("ADSL", "ADAE"))
  })

  test_that("data info shows info", {
    expect_output(
      expect_null(ds$print_filter_info("ADSL", variables = c("AGE", "SEX"))),
      "has filter type range"
    )
  })

  test_that("set filter state works", {
    # `set_single_filter_state` is a convenient wrapper around it

    # since no filter was set before, will just take the default filter
    # returns `logical` whether state was actually changed
    set_default_filter <- function(dataname, varname) {
      set_single_filter_state(ds, dataname = dataname, varname = varname, state = default_filter())
    }
    expect_true(set_default_filter("ADSL", "AGE"))

    expect_equal(names(ds$get_filter_state("ADSL")), "AGE")
    expect_identical(ds$get_filter_state("ADSL")$AGE, list(range = range(ADSL$AGE), keep_na = FALSE))

    expect_error(
      # real range is (20, 69)
      set_single_filter_state(ds, "ADSL", "AGE", list(range = c(-10, 110), keep_na = TRUE)),
      "full range"
    )

    expect_true(set_single_filter_state(ds, "ADSL", "AGE", list(range = c(31, 50), keep_na = TRUE)))
    expect_true(set_single_filter_state(ds, "ADSL", "AGE", list(range = c(30, 50), keep_na = TRUE)))
    expect_identical(ds$get_filter_state("ADSL")$AGE, list(range = c(30, 50), keep_na = TRUE))
    expect_false(set_single_filter_state(ds, "ADSL", "AGE", list(range = c(30, 50), keep_na = TRUE)))
    expect_identical(ds$get_filter_state("ADSL")$AGE, list(range = c(30, 50), keep_na = TRUE))

    expect_true(set_default_filter("ADSL", "AGE"))
    expect_true(set_default_filter("ADSL", "SEX"))

    expect_setequal(names(ds$get_filter_state("ADSL")), c("AGE", "SEX"))
    expect_identical(
      ds$get_filter_state("ADSL")$SEX$choices,
      as.character(levels(ADSL$SEX))
    )

    # range too large
    expect_error(
      set_single_filter_state(ds, "ADSL", "AGE", list(range = range(ADSL$AGE) + c(-1, +1), keep_na = TRUE)),
      "not valid for", fixed = TRUE
    )

    # range slightly smaller
    set_single_filter_state(ds, "ADSL", "AGE", list(range = range(ADSL$AGE) + c(+1, -1), keep_na = TRUE))
    expect_equal(
      ds$get_filter_state("ADSL")$AGE,
      list(range = range(ADSL$AGE) + c(+1, -1), keep_na = TRUE)
    )
  })

  test_that("set_filter_state works with more than one filter specified", {
    ds$set_filter_state("ADSL", state = list(
      AGE = list(range = c(38, 40), keep_na = TRUE),
      SEX = list(choices = "F", keep_na = TRUE)
    ))
    expect_equal(
      ds$get_data("ADSL", filtered = TRUE),
      dplyr::filter(ADSL, SEX == "F" & AGE >= 38 & AGE <= 40)
    )

    set_single_filter_state(ds, "ADSL", "AGE", state = NULL)
    set_single_filter_state(ds, "ADSL", "SEX", state = NULL)
    expect_equal(
      ds$get_data("ADSL", filtered = TRUE),
      ADSL
    )

    # remove omitted works
    ds$set_filter_state("ADSL", state = list(
      AGE = list(range = c(38, 40), keep_na = TRUE),
      SEX = list(choices = "F", keep_na = TRUE)
    ), remove_omitted = TRUE)
    expect_setequal(names(ds$get_filter_state("ADSL")), c("AGE", "SEX"))
    ds$set_filter_state("ADSL", state = list(
      AGE = list(range = c(38, 40), keep_na = TRUE),
      COUNTRY = list(choices = "CHN", keep_na = TRUE)
    ), remove_omitted = TRUE)
    expect_setequal(names(ds$get_filter_state("ADSL")), c("AGE", "COUNTRY"))
  })

  test_that("column_labels works", {
    on.exit(ds$set_data("ADSL", ADSL))

    data <- ADSL

    ## check with all labels provided
    # preconditions on ADSL so tests below work as expected
    stopifnot(
      all_false(var_labels(ADSL), is.na), # no NA label
      setequal(names(var_labels(ADSL)), colnames(ADSL)) # all variables have labels
    )
    attr(data, "column_labels") <- var_labels(ADSL)
    ds$set_data("ADSL", data)
    expect_equal(
      ds$get_variable_labels("ADSL"),
      var_labels(ADSL)
    )
    # only some variables
    expect_equal(
      ds$get_variable_labels("ADSL", variables = c("AGE", "SEX")),
      var_labels(ADSL)[c("AGE", "SEX")]
    )

    ## check with no labels (NULL)
    attr(data, "column_labels") <- NULL
    ds$set_data("ADSL2", data)
    expect_equal(
      ds$get_variable_labels("ADSL2"),
      NULL
    )
    # only some variables
    expect_equal(
      ds$get_variable_labels("ADSL2", variables = c("AGE", "SEX")),
      NULL
    )
  })

})
