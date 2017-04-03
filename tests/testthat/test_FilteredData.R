
library(teal)
context("FilteredData")

options(teal_logging = FALSE)

x <- teal:::FilteredData$new(datanames = c('ASL', 'atr'))

test_that(
  "Initialization is correct",
  expect_identical(x$datanames(), c('ASL', 'atr'))
)



ASL <- generate_sample_data("ASL")
x$set_data("ASL", ASL)

test_that(
  "load and set_datasets",
  expect_identical(x$get_data('ASL'), ASL)
)


