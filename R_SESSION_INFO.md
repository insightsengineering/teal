# R Session Info

This document captures the R session information for the development environment.

## How to generate

To generate the session info, run the following in R:

```r
sessionInfo()
```

Or for more detailed information:

```r
if (requireNamespace("sessioninfo", quietly = TRUE)) {
  sessioninfo::session_info()
} else {
  sessionInfo()
}
```

## Session Info Output

The session info will be populated here once R is available in the environment.

To capture this information and add it to the PR:

1. Open R console in the project directory
2. Run: `writeLines(capture.output(sessionInfo()), "R_SESSION_INFO.txt")`
3. The output will be saved to `R_SESSION_INFO.txt`

## Testing with example_module and example_module2

To test that both modules display 2 outputs, run:

```r
library(teal)

app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = modules(
    example_module(label = "example 1"),
    example_module2(label = "example 2")
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```

This will create a Shiny application with two tabs, each showing the output of `example_module` and `example_module2` respectively. Both modules will display dataset information in their respective tabs.
