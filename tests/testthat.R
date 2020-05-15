library(testthat)
library(magrittr)

test_results <- test_check("teal")
saveRDS(test_results, "unit_testing_results.rds")
