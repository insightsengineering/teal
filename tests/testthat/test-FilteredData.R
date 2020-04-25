library(random.cdisc.data)

context("FilteredData")

options(teal_logging = FALSE)

x <- teal:::FilteredData$new()

test_that("Initialization is correct", {
  isolate(expect_setequal(x$datanames(), character(0)))
})

ADSL <- radsl(cached = TRUE) #nolintr
ADAE <- radae(cached = TRUE) #nolintr

isolate({
  x$set_data("ADSL", ADSL)
  x$set_data("ADAE", ADAE)

  test_that("load and set_datasets", {
      expect_equal(x$get_data("ADSL"), ADSL)
      expect_equal(x$get_data("ADAE"), ADAE)

      expect_setequal(x$datanames(), c("ADSL", "ADAE"))
  })

  test_that("data info shows info", {
      expect_output(expect_true(
        is.null(x$list_data_info("ADSL", variables = c("AGE", "SEX")))
      ), "has filter type range")
  })

  test_that("set filter state works", {
    # since no filter was set before, will just take the default filter
    x$restore_filter("ADSL", "AGE")

    expect_equal(names(x$get_filter_state("ADSL")), "AGE")
    expect_identical(x$get_filter_state("ADSL")$AGE, list(selection = range(ADSL$AGE), keep_na = FALSE))

    expect_error(
      # real range is (20, 69)
      x$set_filter_state("ADSL", "AGE", list(selection = c(-10, 110), keep_na = TRUE)),
      "full range"
    )

    expect_true(x$set_filter_state("ADSL", "AGE", list(selection = c(31, 50), keep_na = TRUE)))
    expect_true(x$set_filter_state("ADSL", "AGE", list(selection = c(30, 50), keep_na = TRUE)))
    expect_identical(x$get_filter_state("ADSL")$AGE, list(selection = c(30, 50), keep_na = TRUE))
    expect_false(x$set_filter_state("ADSL", "AGE", list(selection = c(30, 50), keep_na = TRUE)))
    expect_identical(x$get_filter_state("ADSL")$AGE, list(selection = c(30, 50), keep_na = TRUE))

    expect_true(x$restore_filter("ADSL", "AGE"))
    expect_identical(x$get_filter_state("ADSL")$AGE, list(selection = c(31, 50), keep_na = TRUE))
    expect_true(x$restore_filter("ADSL", "AGE"))
    expect_identical(x$get_filter_state("ADSL")$AGE, list(selection = c(30, 50), keep_na = TRUE))

    expect_true(x$restore_filter("ADSL", "SEX"))
    expect_setequal(names(x$get_filter_state("ADSL")), c("AGE", "SEX"))
    # also counts are added after numberrs
    expect_identical(
      x$get_filter_state("ADSL")$SEX$selection,
      setNames(
        as.character(levels(ADSL$SEX)),
        paste0(names(table(ADSL$SEX)), " (", as.vector(table(ADSL$SEX)), ")")
      )
    )

    x$set_filter_state(
      "ADSL", varname = NULL,
      state = list(AGE = list(selection = range(ADSL$AGE) + c(+1, -1), keep_na = TRUE))
    )
    expect_equal(
      x$get_filter_state("ADSL")$AGE,
      list(selection = range(ADSL$AGE) + c(+1, -1), keep_na = TRUE)
    )
  })

  test_that("check filtering", {
    x$set_filter_state("ADSL", varname = NULL, state = list(
      AGE = list(selection = c(38, 40), keep_na = TRUE),
      SEX = list(selection = "F", keep_na = TRUE)
    ))
    expect_equal(
      x$get_data("ADSL", filtered = TRUE),
      subset(ADSL, SEX == "F" & AGE >= 38 & AGE <= 40)
    )

    x$remove_filter("ADSL", "AGE")
    x$remove_filter("ADSL", "SEX")
    expect_equal(
      x$get_data("ADSL", filtered = TRUE),
      ADSL
    )
  })

})
