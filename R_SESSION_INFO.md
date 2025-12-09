# R Session Info

This document provides instructions for capturing R session information for this PR.

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

## Capturing Session Info for this PR

Since R is not available in the GitHub Copilot agent environment, the session info should be captured by running the CI pipeline or manually in a local R environment.

### Option 1: Using CI Pipeline
The CI pipeline automatically runs in a Docker container with R installed. Check the GitHub Actions logs for session info.

### Option 2: Manual Capture
To capture this information manually and add it to the PR:

1. Ensure you're in the project directory
2. Open an R console
3. Run: `writeLines(capture.output(sessionInfo()), "R_SESSION_INFO.txt")`
4. The output will be saved to `R_SESSION_INFO.txt`
5. Commit and push the file

### Option 3: Using the Demo Script
Run the demo script which prints session info:

```bash
Rscript demo_two_modules.R
```

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
