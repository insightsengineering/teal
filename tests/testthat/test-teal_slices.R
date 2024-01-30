testthat::test_that("teal_slices produces teal_slices object with teal-specific-attributes", {
  tss <- teal_slices()
  testthat::expect_s3_class(tss, "teal_slices")
  testthat::expect_s3_class(tss, "modules_teal_slices")
  testthat::expect_true(is.list(attr(tss, "mapping")))
  testthat::expect_identical(attr(tss, "module_specific"), FALSE)
})

testthat::test_that("teal_slices fails when inexisting teal_slice id is specified in mapping", {
  testthat::expect_no_error(
    teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      module_specific = FALSE,
      mapping = list(
        module = "inexisting"
      )
    )
  )
  testthat::expect_error(
    teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      module_specific = TRUE,
      mapping = list(
        module = "inexisting"
      )
    ),
    "Filters in mapping don't match any available filter"
  )
})

testthat::test_that("teal_slices processes filter mapping", {
  # if missing, all filters are global
  tss <- teal_slices(
    teal.slice::teal_slice("iris", "Species"),
    teal.slice::teal_slice("mtcars", "mpg")
  )
  testthat::expect_identical(
    attr(tss, "mapping"),
    list(global_filters = c("iris Species", "mtcars mpg"))
  )
  # if empty, no filters are global
  tss <- teal_slices(
    teal.slice::teal_slice("iris", "Species"),
    teal.slice::teal_slice("mtcars", "mpg"),
    mapping = list()
  )
  testthat::expect_true(is.list(attr(tss, "mapping")) && length(attr(tss, "mapping")) == 0L)
  # if partial mapping defined, global unaffected
  tss <- teal_slices(
    teal.slice::teal_slice("iris", "Species"),
    teal.slice::teal_slice("mtcars", "mpg"),
    mapping = list()
  )
})

testthat::test_that("teal_slices drops non-global filters if module_specific = FALSE", {
  tss <- teal_slices(
    teal.slice::teal_slice("iris", "Species"),
    teal.slice::teal_slice("mtcars", "mpg"),
    module_specific = TRUE,
    mapping = list(
      mod1 = "iris Species",
      mod2 = "mtcars mpg"
    )
  )
  testthat::expect_identical(
    attr(tss, "mapping"),
    list(
      mod1 = "iris Species",
      mod2 = "mtcars mpg"
    )
  )

  tss <- teal_slices(
    teal.slice::teal_slice("iris", "Species"),
    teal.slice::teal_slice("mtcars", "mpg"),
    module_specific = FALSE,
    mapping = list(
      mod1 = "iris Species",
      mod2 = "mtcars mpg"
    )
  )
  testthat::expect_true(is.list(attr(tss, "mapping")) && length(attr(tss, "mapping")) == 0L)
})

testthat::test_that(
  paste(
    "deep_copy_filters copies teal_slice changes",
    "pointer of teal_slice object but values remain the same"
  ),
  {
    tss <- teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var1", choices = c("a", "b"), selected = "a"),
      teal.slice::teal_slice(dataname = "data", varname = "var2", choices = c("A", "B"), selected = "B")
    )
    tss_copy <- deep_copy_filter(tss)

    testthat::expect_false(identical(tss, tss_copy))
    testthat::expect_identical(
      {
        tss_temp <- lapply(tss, as.list)
        attributes(tss_temp) <- attributes(tss)
        tss_temp
      },
      {
        tss_copy_temp <- lapply(tss_copy, as.list)
        attributes(tss_copy_temp) <- attributes(tss_copy)
        tss_copy_temp
      }
    )
  }
)







# from different file



testthat::test_that("teal_slices mapping should be an empty list or a named list or missing", {
  testthat::expect_no_error(
    teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test")
    )
  )
  testthat::expect_no_error(
    teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list()
    )
  )
  testthat::expect_no_error(
    teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list(module = c())
    )
  )
  testthat::expect_error(
    teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list("1", "2", "3")
    ),
    "Assertion.+failed"
  )
  testthat::expect_error(
    teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list(1, 2, 3)
    ),
    "Assertion.+failed"
  )

  testthat::expect_error(
    teal_slices(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = "mapping"
    ),
    "Assertion.+failed"
  )
})
