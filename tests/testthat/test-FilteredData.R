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

ADSL$AGE[1:40] <- NaN
ADSL$AGE[41:50] <- NA
ADSL$AGE[51:100] <- Inf
ADSL$SEX[90:110] <- NA
ADAE$ASEQ[sample(seq_len(nrow(ADAE)), 100)] <- NA
ADAE$ASEQ[sample(seq_len(nrow(ADAE)), 100)] <- Inf

isolate({
  ds$set_data("ADSL", ADSL)
  ds$set_data("ADAE", ADAE)

  # helper function
  set_default_filter <- function(dataname, varname) {
    set_single_filter_state(ds, dataname = dataname, varname = varname, state = default_filter())
  }

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

  test_that("set default filter state", {
    # `set_single_filter_state` is a convenient wrapper around it

    # since no filter was set before, will just take the default filter
    # returns `logical` whether state was actually changed
    expect_true(set_default_filter("ADSL", "AGE"))

    expect_equal(names(ds$get_filter_state("ADSL")), "AGE")
    expect_identical(
      ds$get_filter_state("ADSL")$AGE,
      list(range = range(ADSL$AGE, finite = TRUE), keep_na = FALSE, keep_inf = FALSE)
    )

    expect_identical(
      ds$get_data("ADSL", filtered = TRUE)$AGE,
      structure(
        ADSL$AGE[!is.na(ADSL$AGE) & !is.infinite(ADSL$AGE)],
        label = "Age"
      )
    )
  })

  test_that("set wrong filter states", {
    expect_error(
      # real range is (20, 69)
      set_single_filter_state(ds, "ADSL", "AGE", list(range = c(-10, 110), keep_na = TRUE)),
      "full range"
    )

    # range too large
    expect_error(
      set_single_filter_state(
        datasets = ds,
        dataname =  "ADSL",
        varname = "AGE",
        state = list(range = range(ADSL$AGE, finite = TRUE) + c(-1, +1), keep_na = TRUE)
      ),
      "not valid for", fixed = TRUE
    )
  })

  test_that("set filter state - single var", {
    expect_true(
      set_single_filter_state(ds, "ADSL", "AGE", list(range = c(31, 50), keep_na = TRUE))
    )

    expect_true(
      set_single_filter_state(
        datasets = ds,
        dataname = "ADSL",
        varname = "AGE",
        state = list(range = c(30, 50), keep_na = TRUE, keep_inf = TRUE)
      )
    )

    expect_identical(
      ds$get_filter_state("ADSL")$AGE,
      list(range = c(30, 50), keep_na = TRUE, keep_inf = TRUE)
    )

    expect_identical(
      deparse(ds$get_filter_expr(dataname = "ADSL"), width.cutoff = 500L),
      "ADSL_FILTERED <- dplyr::filter(ADSL, is.na(AGE) | (is.infinite(AGE) | AGE >= 30 & AGE <= 50))"
    )

    expect_identical(
      ds$get_data(dataname = "ADSL", filtered = TRUE)$AGE,
      structure(
        ADSL$AGE[is.infinite(ADSL$AGE) | is.na(ADSL$AGE) | ADSL$AGE >= 30 & ADSL$AGE <= 50],
        label = "Age"
      )
    )

    expect_false(
      set_single_filter_state(ds, "ADSL", "AGE", list(range = c(30, 50), keep_na = TRUE, keep_inf = TRUE))
    )

    expect_identical(
      ds$get_filter_state("ADSL")$AGE,
      list(range = c(30, 50), keep_na = TRUE, keep_inf = TRUE)
    )
  })

  test_that("set filter state - multiple vars", {
    expect_true(set_default_filter("ADSL", "AGE"))
    expect_true(set_default_filter("ADSL", "SEX"))

    expect_setequal(names(ds$get_filter_state("ADSL")), c("AGE", "SEX"))
    expect_identical(
      ds$get_filter_state("ADSL")$SEX,
      list(choices = levels(ADSL$SEX), keep_na = FALSE, keep_inf = FALSE)
    )

    expect_true(
      set_single_filter_state(
        datasets = ds,
        dataname = "ADSL",
        varname = "AGE",
        state = list(range = range(ADSL$AGE, finite = TRUE) + c(+1, -1), keep_na = FALSE)
      )
    )

    expect_true(
      set_single_filter_state(
        datasets = ds,
        dataname = "ADSL",
        varname = "SEX",
        state = list(choices = c("M", "F"), keep_na = FALSE)
      )
    )

    expect_equal(
      ds$get_filter_state("ADSL")$AGE,
      list(range = range(ADSL$AGE, finite = TRUE) + c(+1, -1), keep_na = FALSE, keep_inf = FALSE)
    )

    expect_equal(
      ds$get_filter_state("ADSL")$SEX,
      list(choices = c("M", "F"), keep_na = FALSE, keep_inf = FALSE)
    )

    expect_identical(
      deparse(ds$get_filter_expr("ADSL"), width.cutoff = 500L),
      "ADSL_FILTERED <- dplyr::filter(ADSL, AGE >= 21 & AGE <= 68, SEX %in% c(\"M\", \"F\"))"
    )

    expect_identical(
      ds$get_data("ADSL")$AGE,
      structure(
        ADSL$AGE[(!is.na(ADSL$AGE) & ADSL$AGE >= 21 & ADSL$AGE <= 68) & ADSL$SEX %in% c("M", "F")],
        label = "Age"
      )
    )
  })

  test_that("set_filter_state works with more than one filter specified", {
    expect_true(
      ds$set_filter_state(
        dataname = "ADSL",
        state = list(
          AGE = list(range = c(38, 40), keep_na = TRUE, keep_inf = FALSE),
          SEX = list(choices = "F", keep_na = TRUE, keep_inf = FALSE)
        )
      )
    )

    expect_identical(
      deparse(ds$get_filter_expr("ADSL"), width.cutoff = 500L),
      "ADSL_FILTERED <- dplyr::filter(ADSL, is.na(AGE) | AGE >= 38 & AGE <= 40, is.na(SEX) | SEX == \"F\")"
    )

    expect_equal(
      ds$get_data("ADSL", filtered = TRUE),
      dplyr::filter(ADSL, is.na(AGE) | AGE >= 38 & AGE <= 40, is.na(SEX) | SEX == "F")
    )

    set_single_filter_state(ds, "ADSL", "AGE", state = NULL)
    set_single_filter_state(ds, "ADSL", "SEX", state = NULL)
    expect_identical(
      deparse(ds$get_filter_expr("ADSL"), width.cutoff = 500L),
      "ADSL_FILTERED <- ADSL"
    )
    expect_equal(
      ds$get_data("ADSL", filtered = TRUE),
      ADSL
    )
  })

  test_that("remove omitted", {
    # remove omitted works
    ds$set_filter_state(
      dataname = "ADSL",
      state = list(
        AGE = list(range = c(38, 40), keep_na = TRUE, keep_inf = FALSE),
        SEX = list(choices = "F", keep_na = TRUE, keep_inf = FALSE)
      ),
      remove_omitted = TRUE
    )

    expect_setequal(names(ds$get_filter_state("ADSL")), c("AGE", "SEX"))

    ds$set_filter_state(
      dataname = "ADSL",
      state = list(
        AGE = list(range = c(38, 40), keep_na = TRUE),
        COUNTRY = list(choices = "CHN", keep_na = TRUE)
      ),
      remove_omitted = TRUE
    )
    expect_setequal(names(ds$get_filter_state("ADSL")), c("AGE", "COUNTRY"))
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

  test_that("keep_na and keep_inf", {
    ds$set_filter_state(
      dataname = "ADSL",
      state = list(
        AGE = list(range = c(38, 40), keep_na = FALSE, keep_inf = FALSE),
        SEX = list(choices = "F", keep_na = TRUE, keep_inf = FALSE)
      ),
      remove_omitted = TRUE
    )

    expect_equal(
      ds$get_data("ADSL"),
      ADSL %>% dplyr::filter(AGE >= 38 & AGE <= 40, is.na(SEX) | SEX == "F")
    )

    ds$set_filter_state(
      dataname = "ADSL",
      state = list(
        AGE = list(range = range(ADSL$AGE, finite = TRUE), keep_na = FALSE, keep_inf = TRUE),
        SEX = list(choices = "F", keep_na = TRUE, keep_inf = FALSE)
      ),
      remove_omitted = TRUE
    )

    expect_identical(
      deparse(ds$get_filter_expr("ADSL"), width.cutoff = 500L),
      "ADSL_FILTERED <- dplyr::filter(ADSL, is.infinite(AGE) | AGE >= 20 & AGE <= 69, is.na(SEX) | SEX == \"F\")" # nolint
    )

    expect_equal(
      ds$get_data("ADSL"),
      ADSL %>% dplyr::filter(is.infinite(AGE) | AGE >= 20 & AGE <= 69, is.na(SEX) | SEX == "F")
    )

    ds$set_filter_state(
      dataname = "ADSL",
      state = list(
        AGE = list(range = c(38, 40), keep_na = TRUE, keep_inf = FALSE),
        SEX = list(choices = "F", keep_na = FALSE, keep_inf = TRUE),
        COUNTRY = c("CHN", "USA", "BRA")
      ),
      remove_omitted = TRUE
    )

    expect_identical(
      ds$get_filter_state("ADSL"),
      list(
        AGE = list(range = c(38, 40), keep_na = TRUE, keep_inf = FALSE),
        SEX = list(choices = "F", keep_na = FALSE, keep_inf = TRUE),
        COUNTRY = list(choices = c("CHN", "USA", "BRA"), keep_na = FALSE, keep_inf = FALSE)
      )
    )

    expect_identical(
      deparse(ds$get_filter_expr(dataname = "ADSL"), width.cutoff = 500L),
      "ADSL_FILTERED <- dplyr::filter(ADSL, is.na(AGE) | AGE >= 38 & AGE <= 40, SEX == \"F\", COUNTRY %in% c(\"CHN\", \"USA\", \"BRA\"))" # nolint
    )


    expect_equal(
      ds$get_data("ADSL"),
      ADSL %>% dplyr::filter(is.na(AGE) | AGE >= 38 & AGE <= 40,
                             SEX == "F",
                             COUNTRY %in% c("CHN", "USA", "BRA"))
    )
  })

  test_that("non ADSL filter", {
    ds$set_filter_state(
      dataname = "ADSL",
      state = list(
        AGE = list(range = c(38, 40), keep_na = TRUE, keep_inf = FALSE),
        SEX = list(choices = "F", keep_na = FALSE, keep_inf = TRUE)
      ),
      remove_omitted = TRUE
    )

    ds$set_filter_state(
      dataname = "ADAE",
      state = list(
        ASEQ = list(range = c(1, 5), keep_na = TRUE, keep_inf = TRUE)
      ),
      remove_omitted = TRUE
    )

    expect_identical(
      deparse(ds$get_filter_expr("ADSL"), width.cutoff = 500L),
      "ADSL_FILTERED <- dplyr::filter(ADSL, is.na(AGE) | AGE >= 38 & AGE <= 40, SEX == \"F\")"
    )

    expect_identical(
      deparse(ds$get_filter_expr("ADAE"), width.cutoff = 500L)[2:3],
      c(
        "    ADAE_FILTERED_ALONE <- dplyr::filter(ADAE, is.na(ASEQ) | (is.infinite(ASEQ) | ASEQ >= 1 & ASEQ <= 5))", # nolint
        "    ADAE_FILTERED <- dplyr::inner_join(x = ADSL_FILTERED[, c(\"STUDYID\", \"USUBJID\")], y = ADAE_FILTERED_ALONE, by = c(\"STUDYID\", \"USUBJID\"))" # nolint
      )
    )
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
