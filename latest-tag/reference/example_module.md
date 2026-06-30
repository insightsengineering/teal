# An example `teal` module

This module creates an object called `object` that can be modified with
decorators. The `object` is determined by what's selected in
`Choose a dataset` input in UI. The object can be anything that can be
handled by
[`renderPrint()`](https://rdrr.io/pkg/shiny/man/renderPrint.html). See
the
[`vignette("transform-module-output", package = "teal")`](https://insightsengineering.github.io/teal/articles/transform-module-output.md)
or
[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
to read more about decorators.

## Usage

``` r
example_module(
  label = "example teal module",
  datanames = "all",
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For
  [`modules()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  defaults to `"root"`. See `Details`.

- datanames:

  (`character`) Names of the datasets relevant to the item. There are 2
  reserved values that have specific behaviors:

  - The keyword `"all"` includes all datasets available in the data
    passed to the teal application.

  - `NULL` hides the sidebar panel completely.

  - If `transformators` are specified, their `datanames` are
    automatically added to this `datanames` argument.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/articles/transform-input-data.md).

- decorators:

  **\[experimental\]** (`list` of `teal_transform_module`) optional,
  decorator for `object` included in the module.

## Value

A `teal` module which can be included in the `modules` argument to
[`init()`](https://insightsengineering.github.io/teal/reference/init.md).

## Reporting

This module returns an object of class `teal_module`, that contains a
`server` function. Since the server function returns a `teal_report`
object, this makes this module reportable, which means that the
reporting functionality will be turned on automatically by the `teal`
framework.

For more information on reporting in `teal`, see the vignettes:

- [`vignette("reportable-shiny-application", package = "teal.reporter")`](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/reportable-shiny-application.html)

- `vignette("adding-support-for-reporting-to-custom-modules", package = "teal")`

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIFpUcxUNql2A6dIAmUUlGkBeaV2oB9Z6-YASSxAgGUPaVpGWgBnXGkAWV0AYQBBLHDPGFICFhieXHtpGCJHFWo4GIi4AA9YVAqfErKK7QF+QSVpdiFyUXEpbR0QIpiACyFWVPR2cwASFVp4+ZiRKUYOgF8wTYBdIA)

## Examples

``` r
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = example_module()
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
