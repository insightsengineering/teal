# Executes modifications to the result of a module

**\[experimental\]**

Exported to be able to use methods not to be used directly by
module-developers or app-users. Primarily used to modify the output
object of module.

## Usage

``` r
after(x, server = function(input, output, session, data) data, ...)
```

## Arguments

- x:

  (`teal_module` or `teal_modules`).

- server:

  (`function(input, output, session, data, ...)`) function to receive
  output data from `tm$server`. Must return data

- ...:

  Additional arguments passed to the server wrapper function by matching
  their formal names.

## Value

A `teal_module` or `teal_modules` object with a wrapped server.

## See also

[`disable_src()`](https://insightsengineering.github.io/teal/reference/disable_src.md),
[`disable_report()`](https://insightsengineering.github.io/teal/reference/disable_report.md)

## Examples

``` r
library("teal.reporter")
#> 
#> Attaching package: ‘teal.reporter’
#> The following object is masked from ‘package:testthat’:
#> 
#>     Reporter
hide_code <- function(input, output, session, data) {
  teal_card(data) <- Filter(function(x) !inherits(x, "code_chunk"), teal_card(data))
  data
}
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = example_module() |>
    after(server = hide_code)
)

if (interactive()) {
  runApp(app)
}
```
