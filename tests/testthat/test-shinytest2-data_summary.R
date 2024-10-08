testthat::test_that("e2e: data summary list only data names if there is no MAE or data.frames in teal_data", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = teal.data::teal_data(x = 1),
    modules = example_module()
  )

  testthat::expect_identical(
    as.data.frame(app$get_active_data_summary_table()),
    data.frame(
      `Data Name` = c("x"),
      check.names = FALSE
    )
  )

  app$stop()
})


testthat::test_that("e2e: data summary is displayed with 2 columns data without keys", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = simple_teal_data(), # iris, mtcars
    modules = example_module()
  )

  testthat::expect_identical(
    as.data.frame(app$get_active_data_summary_table()),
    data.frame(
      `Data Name` = c("iris", "mtcars"),
      Obs = c("150/150", "32/32"),
      check.names = FALSE
    )
  )

  app$stop()
})

testthat::test_that("e2e: data summary displays datasets by topological_sort of join_keys", {
  skip_if_too_deep(5)

  data <- teal.data::teal_data(mtcars1 = mtcars, mtcars2 = data.frame(am = c(0, 1), test = c("a", "b")))

  teal.data::join_keys(data) <- teal.data::join_keys(
    teal.data::join_key("mtcars2", "mtcars1", keys = c("am"))
  )

  app <- TealAppDriver$new(
    data = data,
    modules = example_module()
  )

  testthat::expect_identical(
    as.data.frame(app$get_active_data_summary_table())[["Data Name"]],
    c("mtcars2", "mtcars1")
  )

  app$stop()
})

testthat::test_that("e2e: data summary is displayed with 3 columns for data with join keys", {
  skip_if_too_deep(5)

  data <- teal.data::teal_data(mtcars1 = mtcars, mtcars2 = data.frame(am = c(0, 1), test = c("a", "b")))

  teal.data::join_keys(data) <- teal.data::join_keys(
    teal.data::join_key("mtcars2", "mtcars1", keys = c("am"))
  )

  app <- TealAppDriver$new(
    data = data,
    modules = example_module()
  )

  testthat::expect_identical(
    as.data.frame(app$get_active_data_summary_table()),
    data.frame(
      `Data Name` = c("mtcars2", "mtcars1"),
      Obs = c("2/2", "32/32"),
      Subjects = c("", "2/2"),
      check.names = FALSE
    )
  )

  app$stop()
})

testthat::test_that(
  "e2e: data summary is displayed properly if teal_data include data.frames with join keys, MAE objects and vectors",
  {
    testthat::skip_if_not_installed("MultiAssayExperiment")
    skip_if_too_deep(5)

    data <- within(
      teal.data::teal_data(),
      {
        mtcars1 <- mtcars
        mtcars2 <- data.frame(am = c(0, 1), test = c("a", "b"))
        iris <- iris
        library(MultiAssayExperiment)
        data("miniACC", package = "MultiAssayExperiment", envir = environment())
        # nolint start: object_name.
        CO2 <- CO2
        factors <- names(Filter(isTRUE, vapply(CO2, is.factor, logical(1L))))
        CO2[factors] <- lapply(CO2[factors], as.character)
        # nolint end: object_name.
      }
    )

    datanames(data) <- c("CO2", "iris", "miniACC", "mtcars2", "mtcars1", "factors")

    teal.data::join_keys(data) <- teal.data::join_keys(
      teal.data::join_key("mtcars2", "mtcars1", keys = c("am"))
    )

    app <- TealAppDriver$new(
      data = data,
      modules = example_module()
    )

    testthat::expect_identical(
      as.data.frame(app$get_active_data_summary_table()),
      data.frame(
        `Data Name` = c(
          "CO2", "iris", "miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray", "- Mutations", "- miRNASeqGene",
          "mtcars2", "mtcars1", "factors"
        ),
        Obs = c("84/84", "150/150", "", "198/198", "198/198", "33/33", "97/97", "471/471", "2/2", "32/32", ""),
        Subjects = c("", "", "92/92", "79/79", "90/90", "46/46", "90/90", "80/80", "", "2/2", ""),
        check.names = FALSE
      )
    )

    app$stop()
  }
)

testthat::test_that("e2e: data summary displays datasets by datanames() order if no join_keys", {
  skip_if_too_deep(5)

  data <- teal.data::teal_data(mtcars1 = mtcars, mtcars2 = data.frame(am = c(0, 1), test = c("a", "b")))

  app <- TealAppDriver$new(
    data = data,
    modules = example_module()
  )

  testthat::expect_identical(
    as.data.frame(app$get_active_data_summary_table())[["Data Name"]],
    c("mtcars1", "mtcars2")
  )

  app$stop()
})
