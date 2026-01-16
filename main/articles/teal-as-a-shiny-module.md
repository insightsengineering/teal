# Teal as a Shiny Module

## Introduction

A `shiny` developer can embed a `teal` application into their own
`shiny` app by using `shiny` module components of `teal`:
[`ui_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md)
and
[`srv_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md).
This approach differs from using
[`init()`](https://insightsengineering.github.io/teal/reference/init.md)
and offers greater flexibility. While
[`init()`](https://insightsengineering.github.io/teal/reference/init.md)
includes a session info footer automatically, when using `teal` as a
`shiny` module you can optionally add it manually with
[`ui_session_info()`](https://insightsengineering.github.io/teal/reference/module_session_info.md)
and
[`srv_session_info()`](https://insightsengineering.github.io/teal/reference/module_session_info.md).
Using `teal` as a `shiny` module offers several advantages:

- Embedding one or more `teal` applications within a larger `shiny` app
- Creating `teal` applications with dynamically generated components
  (initial data, modules, filters)

## Example

The following example demonstrates embedding `teal` as a `shiny` module
within a larger `shiny` application. Users can select dataset names
which are passed to the embedded `teal` component. On the server side,
[`srv_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md)
is called with a reactive `teal_data` object passed from the parent
app’s server.

``` r

library(teal)

data <- teal_data() |> within({
  iris <- iris
  mtcars <- mtcars
  df <- data.frame(a = 1:10, b = letters[1:10])
})

mods <- modules(
  example_module("mod1"),
  example_module("mod2")
)

ui_app <- fluidPage(
  title = "Your app with teal as a module",
  selectInput("datasets", "Select datasets", choices = c("iris", "mtcars", "df"), selected = "iris", multiple = TRUE),
  ui_teal("teal", mods),
  ui_session_info("session_info")
)

srv_app <- function(input, output, session) {
  data_subset <- reactive(data[input$datasets])
  srv_teal("teal", data = data_subset, modules = mods)
  srv_session_info("session_info")
}

if (interactive()) {
  shinyApp(ui_app, srv_app)
}
```
