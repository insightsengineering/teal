# Validate that dataset has a minimum number of observations

This function is a wrapper for
[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html).

## Usage

``` r
validate_has_data(
  x,
  min_nrow = NULL,
  complete = FALSE,
  allow_inf = TRUE,
  msg = NULL
)
```

## Arguments

- x:

  (`data.frame`)

- min_nrow:

  (`numeric(1)`) Minimum allowed number of rows in `x`.

- complete:

  (`logical(1)`) Flag specifying whether to check only complete cases.
  Defaults to `FALSE`.

- allow_inf:

  (`logical(1)`) Flag specifying whether to allow infinite values.
  Defaults to `TRUE`.

- msg:

  (`character(1)`) Additional message to display alongside the default
  message.

## See also

Other validations:
[`validate_has_elements()`](https://insightsengineering.github.io/teal/reference/validate_has_elements.md),
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIF0mLDl14CVtOYqXU7AEwAKUAOZx2A6dIBnOhcRAEkIVBVSXxoKPjxpeIBZKAAPaQAZCk9SAAtpIiVpAGU4VG543D9-aRghaQBeaQAWDABmXFq0xukAdgwATk6JbhUZJoBWap4qiH9UaiJSAHkoyOj4haX4-ghdgQCRKUYHZTUNLSF1zqI1qJ0QatvSdYASLdJTxgoQxjdF6KPOY1WiMWgBAD6LiKCmkoPBwHhAVepXK1AwWQgOXysiaVyir2oFE6AF1qv4RsEoOQIbkoJCXNSoL5gTU4WCGUpZmz-HUIBCIIwiAB3HoARgADNyecQYAs4OQegAxACCGWKRmlbJgAU8PXi-zg9JkUBcACsVAFPil0pjsQUiqiKmBydJ9qzpLlwdEkVClCiytwMdk8p0mEaANYBHoTXb+AC+u3jAloRXYQnIonEUm0D2qAS9EFYKvQ7DsnUOjGOSYEYHjJKAA)

## Examples

``` r
library(teal)
ui <- fluidPage(
  sliderInput("len", "Max Length of Sepal",
    min = 4.3, max = 7.9, value = 5
  ),
  plotOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    iris_df <- iris[iris$Sepal.Length <= input$len, ]
    validate_has_data(
      iris_df,
      min_nrow = 10,
      complete = FALSE,
      msg = "Please adjust Max Length of Sepal"
    )

    hist(iris_df$Sepal.Length, breaks = 5)
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
