# Validate that dataset has unique rows for key variables

This function is a wrapper for
[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html).

## Usage

``` r
validate_one_row_per_id(x, key = c("USUBJID", "STUDYID"))
```

## Arguments

- x:

  (`data.frame`)

- key:

  (`character`) Vector of ID variables from `x` that identify unique
  records.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIG1GtAM4ASWgBM5ixnFTsAjIgCsABlzTx8JbSALzSAMz8ECq0jsrU0XYAClAA5nDsAtLSlnDUcGIAkhCoKqQZEFlZQiWkBQ6hfGCWqPm0cJaNuJmV1FD0uSHSjQDKufmk2S0EbR143VkEABZEtATtgwQZTXCkRJZQnUNgUoyWq0TURIyHjRLmKUKrB2A8XRWVOXlicPVHObv7TrzaQwFTUcSoPKDfTGbqvbqQoikADypRqW0RpEakUiOUYJ1iqnU4i01VKfiIaNKOhA3UppBq1kxsWcEDsIkSlzKtPeVQslgA+kpYuYrMBRTZhlMZtIAKRCWXSMmkazNVrtPwAXWBEm49ig5AFJDgAsYRAA7gKWowBfZ2BKhX4ANZwVgbLb2bGRYGLKxlB1KayjVDcDAAGQoKVIiz8TDgUCdQVCnkiWQAvpE02ZhfbhKJxFJtDTupZfRBWABBdDsaJ+PEnTMCMBpzVAA)

## Examples

``` r
iris$id <- rep(1:50, times = 3)
ui <- fluidPage(
  selectInput(
    inputId = "species",
    label = "Select species",
    choices = c("setosa", "versicolor", "virginica"),
    selected = "setosa",
    multiple = TRUE
  ),
  plotOutput("plot")
)
server <- function(input, output) {
  output$plot <- renderPlot({
    iris_f <- iris[iris$Species %in% input$species, ]
    validate_one_row_per_id(iris_f, key = c("id"))

    hist(iris_f$Sepal.Length, breaks = 5)
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
