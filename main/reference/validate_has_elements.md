# Validates that vector has length greater than 0

This function is a wrapper for
[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html).

## Usage

``` r
validate_has_elements(x, msg)
```

## Arguments

- x:

  vector

- msg:

  message to display

## See also

Other validations:
[`validate_has_data()`](https://insightsengineering.github.io/teal/reference/validate_has_data.md),
[`validate_has_variable()`](https://insightsengineering.github.io/teal/reference/validate_has_variable.md),
[`validate_in()`](https://insightsengineering.github.io/teal/reference/validate_in.md),
[`validate_input()`](https://insightsengineering.github.io/teal/reference/validate_input.md),
[`validate_inputs()`](https://insightsengineering.github.io/teal/reference/validate_inputs.md),
[`validate_n_levels()`](https://insightsengineering.github.io/teal/reference/validate_n_levels.md),
[`validate_no_intersection()`](https://insightsengineering.github.io/teal/reference/validate_no_intersection.md),
[`validate_one_row_per_id()`](https://insightsengineering.github.io/teal/reference/validate_one_row_per_id.md)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEATKKShzFFqxiXN47AdOm0z0gLzSC7AIyI-gAMuNL+gQBMoeFBwTy4rtIAzqTMVt7SjHCo7H58YACCBWEFAEIFCdJwUAQAFpn+AKz8EK0qtDbK1B1mAApQAOZwLhBuyXDUcGIAkhCoKqQuYNlK-iXSBQDKk9OkKWmWUP7SpES+RDCoLHAlSW71RLQEcMmZ+UUb5V9gAMKVYQmUzEcE8PgKxTASQSSSBezmCyWBVWkR+O2B+1S6SgkVO52IVxudzGbl8dSeLzePg+kNKYAqeE2fwBKV2ILBTIZ0MSJKkjHolloMF0cAAHqQAPKLRHLFgwAD6yRUMBgLFYlQErQEE0YfK6qnU4i0QkRYSI0sWOhASXNpERABI5Yrlaq2F1shAzCIReL2NaSeNYKgpvKTgppHYoPaPMBI-asUdvD4TYt7at-ABde4pIMh3HhuMxuMJjJeZPzVMorMQbMSbgeSxweV1KDJeW7eBkZLsZK5pv+OkAOXOSvoACs9m8hAdsf4MBqA9I63Q7E2W22OxRSN3e1c80ORypx5P3GMSzj52AtYvrqk4MFRqTSQVB8r6CJpEQlDm969T4gZyOfwvA2KYIEGUg6h7PtQxhRc3AKC5CUYTQIDec9IhAxkwIgqDd2DJtIlaJ9iOkABfVoyIEWhv3YIRyFEcQpG0K1YTqIRWEKdB2A6QERD5SiwDIjMgA)

## Examples

``` r
data <- data.frame(
  id = c(1:10, 11:20, 1:10),
  strata = rep(c("A", "B"), each = 15)
)
ui <- fluidPage(
  selectInput("ref1", "Select strata1 to compare",
    choices = c("A", "B", "C"), selected = "A"
  ),
  selectInput("ref2", "Select strata2 to compare",
    choices = c("A", "B", "C"), selected = "B"
  ),
  verbatimTextOutput("arm_summary")
)

server <- function(input, output) {
  output$arm_summary <- renderText({
    sample_1 <- data$id[data$strata == input$ref1]
    sample_2 <- data$id[data$strata == input$ref2]

    validate_has_elements(sample_1, "No subjects in strata1.")
    validate_has_elements(sample_2, "No subjects in strata2.")

    paste0(
      "Number of samples in: strata1=", length(sample_1),
      " comparions strata2=", length(sample_2)
    )
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
