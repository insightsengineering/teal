# Validates that vector includes all expected values

This function is a wrapper for
[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html).

## Usage

``` r
validate_in(x, choices, msg)
```

## Arguments

- x:

  Vector of values to test.

- choices:

  Vector to test against.

- msg:

  (`character(1)`) Error message to display if some elements of `x` are
  not elements of `choices`.

## See also

Other validations:
[`validate_has_data()`](https://insightsengineering.github.io/teal/reference/validate_has_data.md),
[`validate_has_elements()`](https://insightsengineering.github.io/teal/reference/validate_has_elements.md),
[`validate_has_variable()`](https://insightsengineering.github.io/teal/reference/validate_has_variable.md),
[`validate_input()`](https://insightsengineering.github.io/teal/reference/validate_input.md),
[`validate_inputs()`](https://insightsengineering.github.io/teal/reference/validate_inputs.md),
[`validate_n_levels()`](https://insightsengineering.github.io/teal/reference/validate_n_levels.md),
[`validate_no_intersection()`](https://insightsengineering.github.io/teal/reference/validate_no_intersection.md),
[`validate_one_row_per_id()`](https://insightsengineering.github.io/teal/reference/validate_one_row_per_id.md)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEracxUurmAJgAUoAczjsB06QGc41OGICSEKgqpG4QHh58YJ6ofrRwnlG47hFRAMo+fqResQTxiXgpHgQAFkS0BAnSALzSBG7RcKREnlBJ0lFSjJ4VRNREjO2dtIxOQhVteB1gagDWEEQA7uExcQlRPMnhEd6+YnC2NdPeza1JRdIwKtTiqL5HAGIAggAyaSbbmyld9FDiMLo4AAPUgAeRCwVCUU8KhgMBYrA2An4EAE3kYXUsyjUGi0QkhuGkRAhIR0IBSxNIkIAJDC4QisYwKLYRPZGEJQuTth4JNxaLY-nAAPpCdj4kK03L5QkjWiealpKUJQnpJWeaS2IhVBbZYFy0gYJHc6QQRhLMXszzAWXyxVrdXVWri0iS+2EgC6KI8AF8Ud6BLQlNIxcJROIpNoySlPCUhKwnuh2OZCeiun6BGBve6gA)

## Examples

``` r
ui <- fluidPage(
  selectInput(
    "species",
    "Select species",
    choices = c("setosa", "versicolor", "virginica", "unknown species"),
    selected = "setosa",
    multiple = FALSE
  ),
  verbatimTextOutput("summary")
)

server <- function(input, output) {
  output$summary <- renderPrint({
    validate_in(input$species, iris$Species, "Species does not exist.")
    nrow(iris[iris$Species == input$species, ])
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
