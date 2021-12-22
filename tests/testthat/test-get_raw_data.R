library(scda)

testthat::test_that("get_raw_data validates the argument dataname", {
  x <- dataset(dataname = "head_iris", x = head(iris))

  testthat::expect_error(get_raw_data(x, dataname = 1))
  testthat::expect_silent(get_raw_data(x))
  testthat::expect_warning(get_raw_data(x, "dataname"))
})

testthat::test_that("get_raw_data.TealDataset returns a data.frame verbatim ", {
  x <- dataset(dataname = "head_iris", x = head(iris))

  testthat::expect_identical(
    get_raw_data(x),
    head(iris)
  )
})

testthat::test_that("get_raw_data.TealDataset gives warning when dataname is supplied ", {
  x <- dataset(dataname = "head_iris", x = head(iris))

  testthat::expect_warning(
    get_raw_data(x, dataname = "dataname")
  )
})

testthat::test_that("get_raw_data.TealDatasetConnector returns a SCDA data frame verbatim ", {
  pull_fun_adsl <- callable_function(
    function() {
      synthetic_cdisc_data("latest")$adsl
    }
  )
  dc <- dataset_connector("ADSL", pull_fun_adsl)
  load_dataset(dc)

  testthat::expect_identical(
    get_raw_data(dc),
    synthetic_cdisc_data("latest")$adsl
  )
})

testthat::test_that("get_raw_data.TealDatasetConnector gives warning when dataname is supplied ", {
  pull_fun_adsl <- callable_function(
    function() {
      synthetic_cdisc_data("latest")$adsl
    }
  )
  dc <- dataset_connector("ADSL", pull_fun_adsl)
  load_dataset(dc)

  testthat::expect_warning(
    get_raw_data(dc, dataname = "dataname")
  )
})

testthat::test_that("get_raw_data.TealDataAbstract returns dataset objects verbatim when input is TealData", {
  x <- dataset(dataname = "head_iris", x = head(iris))

  y <- dataset(dataname = "head_mtcars", x = head(mtcars))

  rd <- teal:::TealData$new(x, y)

  out <- get_raw_data(rd)

  testthat::expect_equal(length(out), 2)

  testthat::expect_identical(out$head_iris, head(iris))
  testthat::expect_identical(out$head_mtcars, head(mtcars))
})

testthat::test_that(
  "get_raw_data.TealDataAbstract returns dataset objects verbatim when input is TealData",
  { # nolint
    x <- dataset(dataname = "head_iris", x = head(iris))

    y <- dataset(dataname = "head_mtcars", x = head(mtcars))

    rd <- teal:::TealData$new(x, y)

    out <- get_raw_data(rd)

    testthat::expect_equal(length(out), 2)

    testthat::expect_identical(out$head_iris, head(iris))
    testthat::expect_identical(out$head_mtcars, head(mtcars))
  }
)

testthat::test_that(
  "get_raw_data.TealDataAbstract returns dataset objects verbatim when input is TealDataConnector or CDISCTealData",
  { # nolint
    adsl_cf <- callable_function(function() synthetic_cdisc_data("latest")$adsl)
    adsl <- cdisc_dataset_connector(
      dataname = "ADSL",
      pull_callable = adsl_cf,
      keys = get_cdisc_keys("ADSL")
    )
    load_dataset(adsl)
    adlb_cf <- callable_function(function() synthetic_cdisc_data("latest")$adlb)
    adlb <- cdisc_dataset_connector(
      dataname = "ADLB",
      pull_callable = adlb_cf,
      keys = get_cdisc_keys("ADLB")
    )
    load_dataset(adlb)

    rdc <- relational_data_connector(
      connection = data_connection(),
      connectors = list(adsl, adlb)
    )

    out <- get_raw_data(rdc)

    testthat::expect_equal(length(out), 2)

    testthat::expect_identical(out$ADSL, synthetic_cdisc_data("latest")$adsl)
    testthat::expect_identical(out$ADLB, synthetic_cdisc_data("latest")$adlb)

    drc <- cdisc_data(rdc)
    out_cdisc <- get_raw_data(drc)

    testthat::expect_identical(out_cdisc$ADSL, synthetic_cdisc_data("latest")$adsl)
    testthat::expect_identical(out_cdisc$ADLB, synthetic_cdisc_data("latest")$adlb)
  }
)
