testthat::test_that("e2e: data summary just list the unfilterable objects at the bottom when provided", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    data = teal.data::teal_data(x = 1, y = "z", foo = function() NULL),
    modules = example_module()
  )

  testthat::expect_match(
    app$get_text(sprintf("#%s", app$active_data_summary_ns())),
    "\\And 3 more unfilterable object\\(s\\)"
  )

  app$stop()
})


testthat::test_that("e2e: data summary table is displayed with 2 columns data without keys", {
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

testthat::test_that("e2e: data summary table displays datasets by topological_sort of join_keys", {
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

testthat::test_that("e2e: data summary table is displayed with 3 columns for data with join keys", {
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
  "e2e: data summary table does not list unsupported objects",
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
        unsupported <- function(x) x
        # nolint end: object_name.
      }
    )

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
          "iris", "miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray", "- Mutations", "- miRNASeqGene",
          "mtcars2", "mtcars1"
        ),
        Obs = c("150/150", "", "198/198", "198/198", "33/33", "97/97", "471/471", "2/2", "32/32"),
        Subjects = c("", "92/92", "79/79", "90/90", "46/46", "90/90", "80/80", "", "2/2"),
        check.names = FALSE
      )
    )

    app$stop()
  }
)

testthat::test_that("e2e: data summary table displays datasets by names() order if no join_keys", {
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
