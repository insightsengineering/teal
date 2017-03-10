
library(teal)
context("FilteredData")

options(teal_logging = FALSE)

x <- teal:::FilteredData$new(datanames = c('asl', 'atr'))

test_that(
  "Initialization is correct",
  expect_identical(x$datanames(), c('asl', 'atr'))
)



ASL <- generate_sample_data("ASL")
x$set_data("asl", ASL)

test_that(
  "load and set_datasets",
  expect_identical(x$get_data('asl'), ASL)
)


