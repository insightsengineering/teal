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

## Examples

``` r
tdm <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    actionButton(ns("submit"), label = "Load data")
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

eval_code(tdm, "dataset1 <- subset(dataset1, Species == 'virginica')")
#> $ui
#> function(id) {
#>       ns <- NS(id)
#>       object$ui(ns("mutate_inner"))
#>     }
#> <environment: 0x5582d9731ef8>
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
#> <environment: 0x5582d9725c00>
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
#> <environment: 0x5582d86f2aa8>
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
#> <environment: 0x5582d86eeb88>
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
#> <environment: 0x5582d2f45a60>
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
#> <environment: 0x5582d2fad908>
#> 
#> attr(,"label")
#> [1] "data module"
#> attr(,"class")
#> [1] "teal_data_module"
#> attr(,"once")
#> [1] TRUE
```
