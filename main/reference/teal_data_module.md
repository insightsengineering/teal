# Data module for `teal` applications

**\[experimental\]**

Create a `teal_data_module` object and evaluate code on it with history
tracking.

## Usage

``` r
teal_data_module(ui, server, label = "data module", once = TRUE)

# S4 method for class 'teal_data_module'
eval_code(object, code)

# S3 method for class 'teal_data_module'
within(data, expr, ...)
```

## Arguments

- ui:

  (`function(id)`) `shiny` module UI function; must only take `id`
  argument

- server:

  (`function(id)`) `shiny` module server function; must only take `id`
  argument; must return reactive expression containing `teal_data`
  object

- label:

  (`character(1)`) Label of the module.

- once:

  (`logical(1)`) If `TRUE`, the data module will be shown only once and
  will disappear after successful data loading. App user will no longer
  be able to interact with this module anymore. If `FALSE`, the data
  module can be reused multiple times. App user will be able to interact
  and change the data output from the module multiple times.

- object:

  (`teal_data_module`)

- code:

  (`character`, `language` or `expression`) code to evaluate. It is
  possible to preserve original formatting of the `code` by providing a
  `character` or an `expression` being a result of
  `parse(keep.source = TRUE)`.

- data:

  (`teal_data_module`) object

- expr:

  (`expression`) to evaluate. Must be inline code. See `within()`

- ...:

  See `Details`.

## Value

`teal_data_module` returns a list of class `teal_data_module` containing
two elements, `ui` and `server` provided via arguments.

`eval_code` returns a `teal_data_module` object with a delayed
evaluation of `code` when the module is run.

`within` returns a `teal_data_module` object with a delayed evaluation
of `expr` when the module is run.

## Details

`teal_data_module` creates a `shiny` module to interactively supply or
modify data in a `teal` application. The module allows for running any
code (creation *and* some modification) after the app starts or reloads.
The body of the server function will be run in the app rather than in
the global environment. This means it will be run every time the app
starts, so use sparingly.

Pass this module instead of a `teal_data` object in a call to
[`init()`](https://insightsengineering.github.io/teal/reference/init.md).
Note that the server function must always return a `teal_data` object
wrapped in a reactive expression.

See vignette
[`vignette("data-as-shiny-module", package = "teal")`](https://insightsengineering.github.io/teal/articles/data-as-shiny-module.md)
for more details.

`eval_code` evaluates given code in the environment of the `teal_data`
object created by the `teal_data_module`. The code is added to the
`@code` slot of the `teal_data`.

`within` is a convenience function for evaluating inline code inside the
environment of a `teal_data_module`. It accepts only inline expressions
(both simple and compound) and allows for injecting values into `expr`
through the `...` argument: as `name:value` pairs are passed to `...`,
`name` in `expr` will be replaced with `value.`

## See also

[`teal.data::teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data-class.html),
[`teal.code::qenv()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/qenv.html)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIGkAJjDmKu1APrmopKHZhFzK6nHYDp0lbTSALzKahpatOY6IL5+0hAAztbSAHIAyuyR-BBx0s4A5gkAJOaSPjm5eVCFRag+YLoAFrRJLdJQOXAAHrCoXtJESu3Sjs7Sbh5eGHxgPLixuQXFdQuVMwDCdAQA1nmNMvQqpKQkeUTS1ERQ5ntwM7hVNcTm3jO0jC0zc9IzHeb3j2Kz1eYBgpAILASX3mFUqPzAoygCTgpFaZHOpH27XQ7SheEBRWB9URyNIAEZofC-gCloT3CCSSiAEzQ1a5GaMOAJVBwDRSaisaZgNnSOYiqDhCAAISOJwg7ES9QSKnoMFopEp1Cg9Dg1GC8IAMlcboivqtsn4AL4wvzIxhSRj61TqcQRKLSGKw8buTxwNIiB2ZcwPZ2SzIQVBHB5EI6R0gPZEJBKaCDREVwKRkLBwCXiKThuNFZWq9UPT1wvyI5IAd3VzXlIsWOfsiO0NorfnLHbijPJyXeny9Hd7TOSYIhjChQ8qlsbotM08rTigIstFria9is9TC7QqH7glI7CrIQsMAeE19SRC3V6XlcPq82myAloQ3D5FEee8PDTFQS9asAAgug7B7kUAQPOBdoOtk25gJaAC6QA)

## Examples

``` r
tdm <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      tags$p("This is an example of a data module."),
      tags$p(
        "Click the button to load the", tags$code("iris"), "and", tags$code("mtcars"),
        "datasets into the app as", tags$code("dataset1"), "and", tags$code("dataset2"),
        "respectively."
      ),
      actionButton(ns("submit"), label = "Load data")
    )
  },
  server = function(id) {
    moduleServer(id, function(input, output, session) {
      eventReactive(input$submit, {
        data <- within(
          teal_data(),
          {
            dataset1 <- iris
            dataset2 <- mtcars
          }
        )

        data
      })
    })
  }
)

app <- init(data = tdm, modules = example_module())

if (interactive()) {
  shinyApp(app$ui, app$server)
}

eval_code(tdm, "dataset1 <- subset(dataset1, Species == 'virginica')")
#> $ui
#> function(id) {
#>       ns <- NS(id)
#>       object$ui(ns("mutate_inner"))
#>     }
#> <environment: 0x55997d3bd318>
#> 
#> $server
#> function(id) {
#>         data_out <- server(id)
#>         decorate_err_msg(
#>           assert_reactive(data_out),
#>           pre = sprintf("From: 'teal_data_module()':\nA 'teal_data_module' with \"%s\" label:", label),
#>           post = "Please make sure that this module returns a 'reactive` object containing 'teal_data' class of object." # nolint: line_length_linter.
#>         )
#>       }
#> <environment: 0x55997d3bd5b8>
#> 
#> attr(,"label")
#> [1] "data module"
#> attr(,"class")
#> [1] "teal_data_module"
#> attr(,"once")
#> [1] TRUE

within(tdm, dataset1 <- subset(dataset1, Species == "virginica"))
#> $ui
#> function(id) {
#>       ns <- NS(id)
#>       object$ui(ns("mutate_inner"))
#>     }
#> <environment: 0x55998a6f77d8>
#> 
#> $server
#> function(id) {
#>         data_out <- server(id)
#>         decorate_err_msg(
#>           assert_reactive(data_out),
#>           pre = sprintf("From: 'teal_data_module()':\nA 'teal_data_module' with \"%s\" label:", label),
#>           post = "Please make sure that this module returns a 'reactive` object containing 'teal_data' class of object." # nolint: line_length_linter.
#>         )
#>       }
#> <environment: 0x55998a6f73e8>
#> 
#> attr(,"label")
#> [1] "data module"
#> attr(,"class")
#> [1] "teal_data_module"
#> attr(,"once")
#> [1] TRUE

# use additional parameter for expression value substitution.
valid_species <- "versicolor"
within(tdm, dataset1 <- subset(dataset1, Species %in% species), species = valid_species)
#> $ui
#> function(id) {
#>       ns <- NS(id)
#>       object$ui(ns("mutate_inner"))
#>     }
#> <environment: 0x559987ff2af0>
#> 
#> $server
#> function(id) {
#>         data_out <- server(id)
#>         decorate_err_msg(
#>           assert_reactive(data_out),
#>           pre = sprintf("From: 'teal_data_module()':\nA 'teal_data_module' with \"%s\" label:", label),
#>           post = "Please make sure that this module returns a 'reactive` object containing 'teal_data' class of object." # nolint: line_length_linter.
#>         )
#>       }
#> <environment: 0x559987ff85a0>
#> 
#> attr(,"label")
#> [1] "data module"
#> attr(,"class")
#> [1] "teal_data_module"
#> attr(,"once")
#> [1] TRUE
```
