# Apply `teal_transform_module` decorators to reactive `teal_data`

Shiny module pair (`srv_transform_teal_data` / `ui_transform_teal_data`)
that runs a sequence of
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
decorators against a reactive `teal_data` object. Decorators are applied
one after another via `Reduce`, so each one receives the output of the
previous as its `data` argument. Failed transformators are tracked and
any downstream transformator is automatically disabled until the failure
is resolved. An optional `expr` argument allows additional code to be
evaluated on the final decorated output.

## Usage

``` r
srv_transform_teal_data(
  id,
  data,
  transformators,
  modules = NULL,
  is_transform_failed = reactiveValues(),
  expr
)

ui_transform_teal_data(id, transformators, class = "well", ...)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- data:

  (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
  The data which application will depend on.

- transformators:

  (`list` of `teal_transform_module`) decorator modules to apply
  sequentially to `data`. Each transformator receives the output of the
  previous one as input.

- modules:

  **\[deprecated\]** No longer used.

- is_transform_failed:

  **\[deprecated\]** No longer used.

- expr:

  (`expression` or `reactive`) optional expression evaluated on top of
  the decorated output. Useful for post-processing after all
  transformators have run.

- class:

  **\[deprecated\]** No longer used.

- ...:

  additional arguments passed to `.ui_transform_teal_data` (e.g.
  `class`).

## Value

`reactive` `teal_data`

A `list` of
[`bslib::accordion`](https://rstudio.github.io/bslib/reference/accordion.html)
UI elements, one per transformator, or `NULL` if `transformators` is
empty.

## See also

[`assert_decorators()`](https://insightsengineering.github.io/teal/reference/check_decorators.md)

## Examples

``` r
library(shiny)
library(teal.data)

# A decorator that sets a fixed title on a ggplot2 object named `plot`
static_decorator <- teal_transform_module(
  label = "Static decorator",
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        req(data())
        within(data(), {
          plot <- plot + ggplot2::ggtitle("Decorated title")
        })
      })
    })
  }
)

if (interactive()) {
  shinyApp(
    ui = fluidPage(
      ui_transform_teal_data("decorate", transformators = list(static_decorator)),
      plotOutput("plot")
    ),
    server = function(input, output, session) {
      data <- reactive(
        teal_data(
          plot = ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) +
            ggplot2::geom_point()
        )
      )
      decorated <- srv_transform_teal_data(
        "decorate",
        data = data,
        transformators = list(static_decorator)
      )
      output$plot <- renderPlot(decorated()[["plot"]])
    }
  )
}
```
