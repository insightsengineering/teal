
library(teal)
library(testthat)
library(random.cdisc.data)

context("FilteredData")

options(teal_logging = FALSE)

x <- teal:::FilteredData$new(datanames = c('ASL', 'AAE'))

test_that(
  "Initialization is correct",
  expect_identical(x$datanames(), c('ASL', 'AAE'))
)

ADSL <- radsl()

ADAE <- radae(ADSL)

x$set_data("ASL", ADSL)
x$set_data("AAE", ADAE)

test_that(
  "load and set_datasets",{
    expect_identical(x$get_data('ASL'), ADSL)
    expect_identical(x$get_data('AAE'), ADAE)

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
    expect_identical(x$get_filter_state("ASL")$SEX, as.character(unique(ADSL$SEX)))

  }
)


test_that(
  "overwrite filter states", {

    x$set_filter_state("ASL", state = list(AGE = range(ADSL$AGE) + c(+1, -1)))

    expect_equal(
      x$get_filter_state("ASL")$AGE,
      range(ADSL$AGE) + c(+1, -1)
    )

    x$set_filter_state("ASL", state = list(AGE=c(38, 40), SEX = "F"))

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


# more detailed testing that needs refactoring
# x$load_data("devel/ars.sas7bdat")
#
# x$list_data_info("ars")
#
# x$set_filter_state("ars", state=list(PARAMCD=c("BESRSPI", "OVRINV"), ADY=c(4,100)))
# x$set_filter_state("ars", state=list())
#
# x$get_filter_call("ars")
#
# x$get_filter_call("ars")
# x$get_filter_call("ars", merge=FALSE, asl=FALSE)
# x$get_filter_call("ars", merge=FALSE)
# x$get_filter_call("ars", merge=TRUE, asl=TRUE)
#
# df_11 <- x$get_data("ars")
# df_12 <- x$get_data("ars", filtered = TRUE)
#
# nrow(df_11)
# nrow(df_12)
#
# e <- new.env()
#
# e$ASL <- x$get_data("asl")
# e$ARS <- x$get_data("ars")
#
# calls <- x$get_filter_call("ars", merge=TRUE, asl=TRUE)
#
# eval(calls[[1]], e)
# eval(calls[[2]], e)
# eval(calls[[3]], e)
#
# e$ARS_FILTERED
