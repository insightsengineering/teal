# Validates that dataset contains specific variable

This function is a wrapper for
[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html).

## Usage

``` r
validate_has_variable(data, varname, msg)
```

## Arguments

- data:

  (`data.frame`)

- varname:

  (`character(1)`) name of variable to check for in `data`

- msg:

  (`character(1)`) message to display if `data` does not include
  `varname`

## See also

Other validations:
[`validate_has_data()`](https://insightsengineering.github.io/teal/reference/validate_has_data.md),
[`validate_has_elements()`](https://insightsengineering.github.io/teal/reference/validate_has_elements.md),
[`validate_in()`](https://insightsengineering.github.io/teal/reference/validate_in.md),
[`validate_input()`](https://insightsengineering.github.io/teal/reference/validate_input.md),
[`validate_inputs()`](https://insightsengineering.github.io/teal/reference/validate_inputs.md),
[`validate_n_levels()`](https://insightsengineering.github.io/teal/reference/validate_n_levels.md),
[`validate_no_intersection()`](https://insightsengineering.github.io/teal/reference/validate_no_intersection.md),
[`validate_one_row_per_id()`](https://insightsengineering.github.io/teal/reference/validate_one_row_per_id.md)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEATKKShzFFqxiXN47AdOkkZAXmmM4qFyh8eNLUFADmpAAWGEQqpNLeAEwADDy4rtKkAO5ECT5+7AQBUEG40kH0QWkh4VExcXkp-BDNKrQ2ytRtZgAKUGFwLhBuAM5woWIAkhCocUNubkESLKUZi2AAyuNwYtLLjLRQ9KGrwwsEkUS0BHAjeUVBHqXlYNlEz0FRvnAfYEqxjCq6TOo22YjgZjyjwgPzAGTSGSkjHolloMF0cAAHqQAPJxWakAIjFQwGAsVhVATNARjRhIjqqdTiLRCAllWKkAk6EAZDkEgAkxNJ5I6vggZhEGOx7B5IL23FodjgAH1IlARsr9odjoM7FAyqy4vz9s0FtJUOryMkAlsJuRIaRfJZ4GR5QcjqERogXmULSNyOxDaRjSwysRqNQ0GMocEqqbpABfZoJgS0JTSQPCUTiKTabkZEaRISsACC6HYbTKtKRybACYAukA)

## Examples

``` r
data <- data.frame(
  one = rep("a", length.out = 20),
  two = rep(c("a", "b"), length.out = 20)
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
    validate_has_variable(data, input$var)
    paste0("Selected treatment variables: ", paste(input$var, collapse = ", "))
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
