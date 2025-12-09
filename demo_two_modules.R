#!/usr/bin/env Rscript
# Demo script showing example_module and example_module2 working together

library(teal)

# Create an application with both modules
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = modules(
    example_module(label = "example 1"),
    example_module2(label = "example 2")
  )
)

# Print session info
cat("\n=== R Session Info ===\n")
print(sessionInfo())
cat("\n=== End of Session Info ===\n\n")

# Launch the app (only if interactive)
if (interactive()) {
  cat("Launching Shiny application with two example modules...\n")
  shinyApp(app$ui, app$server)
} else {
  cat("Application created successfully!\n")
  cat("Both example_module and example_module2 are included.\n")
  cat("To run interactively, source this file in an R session.\n")
}
