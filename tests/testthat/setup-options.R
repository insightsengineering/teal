# `opts_partial_match_old` is left for exclusions due to partial matching in dependent packages (i.e. not fixable here)
# it might happen that it is not used right now, but it is left for possible future use
# use with: `withr::with_options(opts_partial_match_old, { ... })` inside the test
opts_partial_match_old <- list(
  warnPartialMatchDollar = getOption("warnPartialMatchDollar"),
  warnPartialMatchArgs = getOption("warnPartialMatchArgs"),
  warnPartialMatchAttr = getOption("warnPartialMatchAttr")
)
opts_partial_match_new <- list(
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE
)

if (isFALSE(getFromNamespace("on_cran", "testthat")()) && requireNamespace("withr", quietly = TRUE)) {
  withr::local_options(
    opts_partial_match_new,
    .local_envir = testthat::teardown_env()
  )
}
