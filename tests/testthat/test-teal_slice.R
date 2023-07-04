testthat::test_that("teal_slices produces teal_slices object with teal-specific-attributes", {
  tss <- teal_slices()
  testthat::expect_s3_class(tss, "teal_slices")
  testthat::expect_identical(attr(tss, "mapping"), list())
  testthat::expect_identical(attr(tss, "module_specific"), FALSE)
})


testthat::test_that("deep_copy_filters copies teal_slice changes pointer of teal_slice object
  but values remain the same", {
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
})
