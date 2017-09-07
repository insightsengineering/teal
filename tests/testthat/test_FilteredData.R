
library(teal)
library(testthat)

context("FilteredData")

options(teal_logging = TRUE)

x <- teal:::CDISCFilteredData$new(datanames = c('ASL', 'atr'))

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



x$set_filter_state(dataname = "ASL", varname = "AGE", state = c(45, 50))

test_that(
  "Simple Filering Works",
  expect_identical(
    x$get_data("ASL", reactive = FALSE, filtered = TRUE),
    subset(ASL, AGE >= 45 & AGE <= 50)
  )
)




context("Test SimpleFilteredData")

# sfd <- teal:::SimpleFilteredData$new(c("iris", "mtcars"))
#
# sfd$set_data("iris", iris)
#
# sfd$set_data("mtcars", mtcars)
#
# ## add indiviual filters




