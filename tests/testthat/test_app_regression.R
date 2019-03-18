library(shinytest)
library(testthat)

context("Test shiny app regression")

# open shiny app
app <- ShinyDriver$new(path = "app_regr", loadTimeout = 100000, debug = "all", seed = 1000)

test_that("app can be interacted without error", {
  # set numeric input
  app$snapshotInit("mytest")

  app$setInputs(`teal_modules.root` = "Regression")
  Sys.sleep(0.5)
  output <- app$findElement(css = "#teal_modules_root\\.Regression-formula")$getText()
  # test
  expect_equal(output, "ADTE.AVAL ~ ADTE.AVAL")
  app$setInputs(`teal_modules_root.Regression-regressor-ds` = "ASL")
  app$setInputs(`teal_modules_root.Regression-regressor-ASL-column` = c("SEX", "AGE"))
  output <- app$findElement(css = "#teal_modules_root\\.Regression-formula")$getText()
  # test
  expect_equal(output, "ADTE.AVAL ~ ASL.SEX + ASL.AGE")
  app$setInputs(`teal_modules_root.Regression-response-ADTE-column` = "BMRKR1")
  output <- app$findElement(css = "#teal_modules_root\\.Regression-model")$getText()
  expect_match(output, regexp = "6\\.17100")
  output <- app$findElement(css = "#teal_modules_root\\.Regression-formula")$getText()
  # test
  expect_equal(output, "ADTE.BMRKR1 ~ ASL.SEX + ASL.AGE")
  
  app$setInputs(`teal_modules_root.Regression-regressor-ds` = "ADTE")
  app$setInputs(`teal_modules_root.Regression-regressor-ADTE-filter` = c("OS", "PFS"))
  app$setInputs(`teal_modules_root.Regression-regressor-ADTE-filter` = "OS")
  app$snapshot()
})

# stop shiny app
app$stop()
