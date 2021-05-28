## CDISCDataset ====
testthat::test_that("CDISCDataset basics", {

  x <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = TRUE)
  rtables::var_labels(x) <- c("X", "Y")

  testthat::expect_error(
    CDISCDataset$new(dataname = "abc", x = x)
  )

  testthat::expect_silent({
    test_ds <- CDISCDataset$new(
      dataname = "testds",
      x = x,
      keys = "x",
      parent = "testds2"
    )
  })

  testthat::expect_equal(
    test_ds$get_parent(),
    "testds2"
  )

  testthat::expect_silent(test_ds$set_parent("testds3"))
  testthat::expect_equal(
    test_ds$get_parent(),
    "testds3"
  )
})
