pkg_name <- "teal"
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  is_on_ci <- isTRUE(as.logical(Sys.getenv("CI")))
  if (is_on_ci) {
    reporter <- MultiReporter$new(list(
      CheckReporter$new(),
      JunitReporter$new(file = "junit-result.xml")
    ))
    test_results <- test_check(pkg_name, reporter = reporter)
    saveRDS(test_results, "unit_testing_results.rds")
  } else {
    reporter <- ParallelProgressReporter$new()
    test_check(pkg_name, reporter = reporter)
  }
}
