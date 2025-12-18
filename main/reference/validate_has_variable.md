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
