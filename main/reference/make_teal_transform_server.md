# Make teal_transform_module's server

A factory function to simplify creation of a
[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)'s
server. Specified `expr` is wrapped in a shiny module function and
output can be passed to the `server` argument in
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
call. Such a server function can be linked with ui and values from the
inputs can be used in the expression. Object names specified in the
expression will be substituted with the value of the respective input
(matched by the name) - for example in
`expression(graph <- graph + ggtitle(title))` object `title` will be
replaced with the value of `input$title`.

## Usage

``` r
make_teal_transform_server(expr)
```

## Arguments

- expr:

  (`language`) An R call which will be evaluated within
  [`teal.data::teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
  environment.

## Value

`function(id, data)` returning `shiny` module

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogNNlGtGAH1aFgM5zFXaldLMIdpUUbWYRACYq1HDsAtLS1FD0cNTSALzSfGAAypaodEq0cP7SQuSi4lLSblAeXj5QpN7K1ba0dkm4YdL+laWwcA4JSXUNeM0qtPHKahpatP46IM3hHo7SAHLJ7BP8EOGzKvAWBACSEKgqpOweoZBWjEQA7n08uIkpKvR2cKTS65c3jdIS3CoyCQAbPcYEJhgBGEFQAAeEIArAAGe52cioCFrcIAXya62kL0YUkYwxgUAA1nBXHBuK53J5vNZ8YT2HBoahGJ07JoICt7PMABZU-w8+r3CAXa52HhSgRrARoNEKXKCY7NVqkKDDZxWNVQYVdXL2O7NPyBYL6lmwdIUk1BEIlMr0yrefVuSw2Q0ygS0JTSFbCAqSEJS6TTXF2PlCVgAQXQ7HlABJBvcE4yRGtMQIwJiALpAA)

## Examples

``` r
trim_iris <- teal_transform_module(
  label = "Simplified interactive transformator for iris",
  datanames = "iris",
  ui = function(id) {
    ns <- NS(id)
    numericInput(ns("n_rows"), "Subset n rows", value = 6, min = 1, max = 150, step = 1)
  },
  server = make_teal_transform_server(expression(iris <- head(iris, n_rows)))
)

app <- init(
  data = teal_data(iris = iris),
  modules = example_module(transformators = trim_iris)
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
