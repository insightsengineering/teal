# nolint start
# because app$setInput etc. are not well chosen names
if (Sys.getenv("R_COVR") == "true") {
  print("Not running shinytest while covr is running")
  return(NULL)
} else {
  library(shinytest) # needs shinytest::installDependencies()
  # for headless browser (execute as root, before run mkdir /root/bin/phantomjs)

  # open shiny app
  app <- ShinyDriver$new(path = testthat::test_path("app_merge"), loadTimeout = 100000, debug = "all", phantomTimeout = 10000)
  testthat::test_that("Check that merge table changes upon data_extract change", {
    skip_if_too_deep(3)
    # set numeric input
    app$snapshotInit("mytest")
    Sys.sleep(2.5)
    select_id <- app$findElement(xpath = "//select[contains(@id,'facetting-dataset_ADSL_singleextract-select')]")$getAttribute("id")
    do.call(app$setInputs, stats::setNames(list("SEX", TRUE), c(select_id, "wait_")))
    Sys.sleep(1)
    output <- app$findElement(xpath = "//div[contains(@id,'test_table')]//thead")$getText()
    testthat::expect_equal(output, "STUDYID USUBJID AGE SEX AVAL")
    Sys.sleep(1)
    do.call(app$setInputs, stats::setNames(list("AGE"), select_id))
    Sys.sleep(1)
    output <- app$findElement(xpath = "//div[contains(@id,'test_table')]//thead")$getText()
    testthat::expect_equal(output, "STUDYID USUBJID AGE AVAL")
  })
  app$stop()
}
# nolint end
