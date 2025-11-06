testthat::describe(".onLoad: Initialised options on package load", {
  it("that are unset are loaded with defaults", {
    testthat::skip_if(getOption("testthat_interactive"))
    withr::with_options(
      list(
        teal.show_js_log = NULL,
        teal.lockfile.mode = NULL,
        shiny.sanitize.errors = NULL,
        teal.sidebar.position = NULL,
        teal.sidebar.width = NULL,
        teal.reporter.nav_buttons = NULL,
        teal.show_src = NULL
      ),
      {
        testthat::expect_no_error(.onLoad())
        testthat::expect_equal(getOption("teal.show_js_log"), FALSE)
        testthat::expect_equal(getOption("teal.lockfile.mode"), "auto")
        testthat::expect_equal(getOption("shiny.sanitize.errors"), FALSE)
        testthat::expect_equal(getOption("teal.sidebar.position"), "left")
        testthat::expect_equal(getOption("teal.sidebar.width"), 250)
        testthat::expect_equal(getOption("teal.reporter.nav_buttons"), c("preview", "download", "load", "reset"))
        testthat::expect_equal(getOption("teal.show_src"), TRUE)
      }
    )
  })

  it("are retained and not overwritten", {
    testthat::skip_if(getOption("testthat_interactive"))
    withr::with_options(
      list(
        teal.show_js_log = TRUE,
        teal.lockfile.mode = "manual",
        shiny.sanitize.errors = TRUE,
        teal.sidebar.position = "right",
        teal.sidebar.width = 300,
        teal.reporter.nav_buttons = c("preview", "download"),
        teal.show_src = FALSE
      ),
      {
        testthat::expect_no_error(.onLoad())
        testthat::expect_equal(getOption("teal.show_js_log"), TRUE)
        testthat::expect_equal(getOption("teal.lockfile.mode"), "manual")
        testthat::expect_equal(getOption("shiny.sanitize.errors"), TRUE)
        testthat::expect_equal(getOption("teal.sidebar.position"), "right")
        testthat::expect_equal(getOption("teal.sidebar.width"), 300)
        testthat::expect_equal(getOption("teal.reporter.nav_buttons"), c("preview", "download"))
        testthat::expect_equal(getOption("teal.show_src"), FALSE)
      }
    )
  })

})

testthat::describe(".onAttach: packageStartupMessage", {
  it("shows version message", {
    testthat::skip_if(getOption("testthat_interactive"))

    testthat::expect_message(
      .onAttach(),
      regexp = "You are using teal version",
      fixed = TRUE
    )
  })

  it("includes version number in message", {
    testthat::skip_if(getOption("testthat_interactive"))

    version <- read.dcf(system.file("DESCRIPTION", package = "teal"))[, "Version"]
    testthat::expect_message(
      .onAttach(),
      regexp = version,
      fixed = TRUE
    )
  })
})

