# Tests for zzz.R functions

testthat::test_that(".onLoad sets default options correctly", {
  # Save current options
  old_opts <- options()
  
  # Clear teal-related options
  options(
    teal.show_js_log = NULL,
    teal.lockfile.mode = NULL,
    shiny.sanitize.errors = NULL,
    teal.sidebar.position = NULL,
    teal.sidebar.width = NULL,
    teal.reporter.nav_buttons = NULL,
    teal.show_src = NULL
  )
  
  # Call .onLoad
  .onLoad(NULL, NULL)
  
  # Check that options are set
  testthat::expect_equal(getOption("teal.show_js_log"), FALSE)
  testthat::expect_equal(getOption("teal.lockfile.mode"), "auto")
  testthat::expect_equal(getOption("shiny.sanitize.errors"), FALSE)
  testthat::expect_equal(getOption("teal.sidebar.position"), "left")
  testthat::expect_equal(getOption("teal.sidebar.width"), 250)
  testthat::expect_equal(getOption("teal.reporter.nav_buttons"), c("preview", "download", "load", "reset"))
  testthat::expect_equal(getOption("teal.show_src"), TRUE)
  
  # Restore original options
  options(old_opts)
})

testthat::test_that(".onLoad does not override existing options", {
  # Save current options
  old_opts <- options()
  
  # Set custom values
  options(
    teal.show_js_log = TRUE,
    teal.sidebar.width = 300
  )
  
  # Call .onLoad
  .onLoad(NULL, NULL)
  
  # Check that custom options are preserved
  testthat::expect_equal(getOption("teal.show_js_log"), TRUE)
  testthat::expect_equal(getOption("teal.sidebar.width"), 300)
  
  # Restore original options
  options(old_opts)
})

testthat::test_that(".onLoad returns invisible NULL", {
  testthat::expect_invisible(.onLoad(NULL, NULL))
})

testthat::test_that(".onAttach displays package version message", {
  testthat::expect_message(
    .onAttach(NULL, NULL),
    "You are using teal version"
  )
})

testthat::test_that("setdiff_teal_slices is available from teal.slice namespace", {
  testthat::expect_true(is.function(setdiff_teal_slices))
})

testthat::test_that("coalesce_r is available from teal.slice namespace", {
  testthat::expect_true(is.function(coalesce_r))
})

testthat::test_that("lang2calls is available from teal.code namespace", {
  testthat::expect_true(is.function(lang2calls))
})

testthat::test_that("code2list is available from teal.data namespace", {
  testthat::expect_true(is.function(code2list))
})

testthat::test_that(".action_button_busy is available from teal.reporter namespace", {
  testthat::expect_true(is.function(.action_button_busy))
})
