# Validate that variables has expected number of levels

If the number of levels of `x` is less than `min_levels` or greater than
`max_levels` the validation will fail. This function is a wrapper for
[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html).

## Usage

``` r
validate_n_levels(x, min_levels = 1, max_levels = 12, var_name)
```

## Arguments

- x:

  variable name. If `x` is not a factor, the unique values are treated
  as levels.

- min_levels:

  cutoff for minimum number of levels of `x`

- max_levels:

  cutoff for maximum number of levels of `x`

- var_name:

  name of variable being validated for use in validation message

## See also

Other validations:
[`validate_has_data()`](https://insightsengineering.github.io/teal/reference/validate_has_data.md),
[`validate_has_elements()`](https://insightsengineering.github.io/teal/reference/validate_has_elements.md),
[`validate_has_variable()`](https://insightsengineering.github.io/teal/reference/validate_has_variable.md),
[`validate_in()`](https://insightsengineering.github.io/teal/reference/validate_in.md),
[`validate_input()`](https://insightsengineering.github.io/teal/reference/validate_input.md),
[`validate_inputs()`](https://insightsengineering.github.io/teal/reference/validate_inputs.md),
[`validate_no_intersection()`](https://insightsengineering.github.io/teal/reference/validate_no_intersection.md),
[`validate_one_row_per_id()`](https://insightsengineering.github.io/teal/reference/validate_one_row_per_id.md)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEATKKShzFFqxiXN47AdOkkZAXmmM4qFyh8eNLUFADmpAAWGEQqpNLeAEwADDy4rtKkAO5ECT5+7AQBUEG40kH0QWkh4VExcXkpaRlRvl75-kVBJcEVpeWEVWWhEBHRsfFJqekQbkqxjHm+ncX9fb2DG2ZDNaN1E43TGQDOpIxCYccAgscAYlBiRIzHefrGAvwQKrQ2ytTfZgAClAwnAXLNpMc4KExABJCCoOLgtxuIISFilDKosAAZWhcDE0nR5yg9FCmIhbgIkSItAIcBe3i6YA8azA2SIbNacDgbPmKkYQyxkPxYjgZjyQVZYAyzQhUkY9EstBgujgAA9SAB5OKI0gBY4qGAwFisKofUwQKGMBW-VTqcRaIR6soTPU6EAZN1xAAkhuNpt+vggZhEas17E9lKJ3FodjgAH0IAnQlJqMd2HYoMBgM7fcSALoFsowIQpuBpxnSRIlqDq8uVvIARgArGViUnYO086QfcTPijpKgoKc4MlkYOBgAZCvQl5EJQimHkCVnOCWeBkGMkslwRADGaTocj8jsVNzzOWbO5hH5lhFuVHqlEajUNBQyUbYVuAco38AX0+f8BFoRd2CEchRHEKRtA9E5IiEVgrnQdhvjKa0FSAsB-wLIA)

## Examples

``` r
data <- data.frame(
  one = rep("a", length.out = 20),
  two = rep(c("a", "b"), length.out = 20),
  three = rep(c("a", "b", "c"), length.out = 20),
  four = rep(c("a", "b", "c", "d"), length.out = 20),
  stringsAsFactors = TRUE
)
ui <- fluidPage(
  selectInput(
    "var",
    "Select variable",
    choices = c("one", "two", "three", "four"),
    selected = "one"
  ),
  verbatimTextOutput("summary")
)

server <- function(input, output) {
  output$summary <- renderText({
    validate_n_levels(data[[input$var]], min_levels = 2, max_levels = 15, var_name = input$var)
    paste0(
      "Levels of selected treatment variable: ",
      paste(levels(data[[input$var]]),
        collapse = ", "
      )
    )
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
