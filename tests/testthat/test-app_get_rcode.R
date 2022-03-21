# nolint start
# because app$setInput etc. are not well chosen names
if (Sys.getenv("R_COVR") == "true") {
  print("Not running shinytest while covr is running")
  return(NULL)
} else {
  library(shinytest) # needs shinytest::installDependencies()
  # for headless browser (execute as root, before run mkdir /root/bin/phantomjs)

  # open shiny app
  app <- ShinyDriver$new(testthat::test_path("app_get_rcode"), loadTimeout = 100000, debug = "all")
  testthat::test_that("Check that R-code changes upon data_extract change", {
    skip_if_too_deep(3)
    # set numeric input
    app$snapshotInit("mytest")
    Sys.sleep(2.5)


    select_id <- app$findElement(xpath = "//select[contains(@id,'dataset_ADTTE_singleextract-filter1')]")$getAttribute("id")
    do.call(app$setInputs, stats::setNames(list(c("OS", "PFS")), select_id))

    Sys.sleep(2.5)
    output <- app$findElement(xpath = "//*[contains(@id,'regression-outtext')]")$getText()
    testthat::expect_equal(output, "regressor.AVAL")
    output <- app$findElement(xpath = "//*[contains(@id,'regression-outtext_response')]")$getText()
    testthat::expect_equal(output, "response.AVAL")

    # Show R Code
    elem <- app$findElement(xpath = "//button[contains(@id,'-show_rcode')]")
    elem$click()
    Sys.sleep(5)

    #    Store R Code
    output_old <- app$findElement(xpath = "//pre[contains(@id,'-r_code')]")$getText()

    #    Dismiss Window
    dismiss_button <- app$findElement(css = "button[data-dismiss=modal]")
    dismiss_button$click()
    Sys.sleep(2)
    testthat::expect_error(app$findElement(xpath = "//pre[contains(@id,'-r_code')]")$getText())

    #    Reset one input
    select_id <- app$findElement(xpath = "//select[contains(@id,'dataset_ADTTE_singleextract-select')]")$getAttribute("id")
    do.call(app$setInputs, stats::setNames(list("AVALU"), select_id))

    output <- app$findElement(xpath = "//*[contains(@id,'regression-outtext')]")$getText()
    testthat::expect_equal(output, "AVALU")

    # Show R Code
    elem <- app$findElement(xpath = "//button[contains(@id,'-show_rcode')]")
    elem$click()
    Sys.sleep(5)

    #    Store R Code
    output_new <- app$findElement(xpath = "//pre[contains(@id,'-r_code')]")$getText()

    # Check that R-code changed and is reactive
    testthat::expect_true(output_new != output_old)
  })
  app$stop()
}
# nolint end
