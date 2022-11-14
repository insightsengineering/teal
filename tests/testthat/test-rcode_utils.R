testthat::test_that("Check padding", {
  testthat::expect_identical(pad(str = "test"), c("test", ""))

  testthat::expect_identical(pad(str = "test", pre = "pre", post = c("post", "post")), c("pre", "test", "post", "post"))

  testthat::expect_identical(pad(str = NULL), NULL)

  testthat::expect_identical(pad(str = ""), NULL)

  testthat::expect_identical(pad(str = character(0)), NULL)

  testthat::expect_identical(pad(str = character(0), pre = ""), NULL)
})

testthat::test_that("No arguments", {
  testthat::expect_silent(get_rcode_header())
})

testthat::test_that("Check title", {
  testthat::expect_identical(
    get_rcode_header(title = "Teal App")[1:3],
    pad("# Teal App", pre = "# ", post = "# ")
  )
})

testthat::test_that("Check description", {
  testthat::expect_identical(
    get_rcode_header(title = "Teal App", description = "Test Description")[4:6],
    pad("# Test Description", post = c("# ", "# "))
  )
})

testthat::test_that("Check Running on", {
  compare <- paste("#", c(
    paste("  Running:", getwd()),
    paste("       on:", Sys.info()["nodename"]),
    paste("R version:", utils::sessionInfo()[["R.version"]][["version.string"]])
  ))

  testthat::expect_true(all(compare %in% get_rcode_header()))
})

testthat::test_that("Check .libPaths()", {
  compare <- paste0("#   - ", .libPaths())
  testthat::expect_true(all(compare %in% get_rcode_header(title = NULL)))
})

testthat::test_that("Check package versions", {
  packages <- sapply(utils::sessionInfo()$otherPkgs, function(x) sprintf("%s (%s)", x$Package, x$Version))
  compare <- paste(
    "#",
    fold_lines(
      paste("Packages versions:", paste(packages, collapse = ", ")),
      80,
      indent_from = ":"
    )
  )


  testthat::expect_true(all(compare %in% get_rcode_header()))
})


testthat::test_that("With no teal.load_nest_code option set get_rcode_str_install returns default string", {
  withr::with_options(
    list(teal.load_nest_code = NULL),
    testthat::expect_equal(get_rcode_str_install(), "# Add any code to install/load your NEST environment here")
  )
})


testthat::test_that("With teal.load_nest_code option set to character get_rcode_str_install returns option value", {
  withr::with_options(
    list(teal.load_nest_code = "code_here()"),
    testthat::expect_equal(get_rcode_str_install(), "code_here()")
  )


  withr::with_options(
    list(teal.load_nest_code = c("code_here()", "and_here()")),
    testthat::expect_equal(get_rcode_str_install(), c("code_here()", "and_here()"))
  )
})


testthat::test_that("With teal.load_nest_code option is not character get_rcode_str_install (silently) returns
          default string", {
  withr::with_options(
    list(teal.load_nest_code = TRUE),
    testthat::expect_equal(get_rcode_str_install(), "# Add any code to install/load your NEST environment here")
  )


  withr::with_options(
    list(teal.load_nest_code = list(x = "boo")),
    testthat::expect_equal(get_rcode_str_install(), "# Add any code to install/load your NEST environment here")
  )
})


testthat::test_that("get_rcode_libraries returns current session packages", {
  testthat::expect_true(
    setequal(
      strsplit(gsub("library\\(|\\)", "", get_rcode_libraries()), "\n")[[1]],
      vapply(sessionInfo()$otherPkgs, FUN = `[[`, index = "Package", FUN.VALUE = character(1), USE.NAMES = FALSE)
    )
  )
})
