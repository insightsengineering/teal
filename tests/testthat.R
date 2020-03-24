library(testthat)

test_results <- test_check(
  package = "teal",
  reporter = JunitReporter$new(file = "unit_testing_results.xml")
)
saveRDS(test_results, "unit_testing_results.rds")
