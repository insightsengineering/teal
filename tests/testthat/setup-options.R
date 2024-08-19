withr::local_options(
  list(
    warnPartialMatchDollar = TRUE,
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE,
    teal.renv.enable = FALSE
  ),
  .local_envir = testthat::teardown_env()
)
