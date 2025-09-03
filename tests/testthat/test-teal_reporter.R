testthat::test_that("TealReportCard object can be initialized", {
  testthat::expect_no_error(TealReportCard$new())
})

testthat::test_that("TealReportCard inherits from ReportCard", {
  testthat::expect_true(inherits(TealReportCard$new(), "ReportCard"))
})

testthat::test_that("TealReportCard$new returns an object of type TealReportCard", {
  testthat::expect_true(inherits(TealReportCard$new(), "TealReportCard"))
})

testthat::test_that("TealReportCard$get_content returns content with metadata", {
  card <- TealReportCard$new()$append_text("test")$append_src("test_src")$append_encodings(list(data = "test"))
  testthat::expect_equal(length(card$get_content()), 4)
  testthat::expect_equal(length(card$get_metadata()), 2)
  testthat::expect_identical(card$get_content()[[1]]$get_content(), "test")
  testthat::expect_identical(
    card$get_content()[[2]]$get_content(),
    "test_src"
  )
  testthat::expect_identical(card$get_content()[[4]]$get_content(), "data: test\n")
})

testthat::test_that("TealReportCard$append_src accepts a character", {
  card <- TealReportCard$new()
  testthat::expect_no_error(card$append_src("test"))
})

testthat::test_that("TealReportCard$append_src returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_src("test"), card)
})

testthat::test_that("TealReportCard$append_src returns title and content", {
  card <- TealReportCard$new()
  card$append_src("test")
  testthat::expect_identical(card$get_content()[[1]]$get_content(), "test")
})

testthat::test_that("TealReportCard$append_encodings accepts list of character", {
  card <- TealReportCard$new()
  testthat::expect_no_error(card$append_encodings(list(a = "test")))
})

testthat::test_that("TealReportCard$append_encodings returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_encodings(list(a = "test_encodings")), card)
})

testthat::test_that("TealReportCard$append_encodings returns title and content", {
  card <- TealReportCard$new()
  card$append_encodings(list(a = "test"))
  testthat::expect_identical(card$get_content()[[1]]$get_content(), "Selected Options")
  testthat::expect_identical(card$get_content()[[2]]$get_content(), "a: test\n")
})

testthat::test_that("TealReportCard$append_fs accepts only a teal_slices", {
  card <- TealReportCard$new()
  testthat::expect_error(card$append_fs(c(a = 1, b = 2)),
    regexp = "Assertion on 'fs' failed: Must inherit from class 'teal_slices', but has class 'numeric'."
  )
  testthat::expect_no_error(
    card$append_fs(
      teal.slice::teal_slices(teal.slice::teal_slice(dataname = "a", varname = "b"))
    )
  )
})

testthat::test_that("TealReportCard$append_fs returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(
    card$append_fs(teal.slice::teal_slices(teal.slice::teal_slice(dataname = "a", varname = "b"))),
    card
  )
})

testthat::test_that("TealReportCard$append_fs returns title and content", {
  card <- TealReportCard$new()
  card$append_fs(teal.slice::teal_slices(teal.slice::teal_slice(dataname = "a", varname = "b")))
  testthat::expect_identical(card$get_content()[[1]]$get_content(), "Filter State")
})

