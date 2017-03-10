
library(teal)
context("FilteredData")

x <- teal:::FilteredData$new(datanames = c('asl', 'atr'))

test_that(
  "Initialization is correct",
  expect_identical(x$datanames(), c('asl', 'atr'))
)
