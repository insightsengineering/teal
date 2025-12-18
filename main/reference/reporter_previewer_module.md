# Create a `teal` module for previewing a report

**\[deprecated\]**

This function controls the appearance of the drop-down menu for the
reporter. It is now deprecated in favor of the options:

- `teal.reporter.nav_buttons = c("preview", "download", "load", "reset")`
  to control which buttons will be displayed in the drop-down.

- `teal.reporter.rmd_output`: passed to
  [`teal.reporter::download_report_button_srv()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/download_report_button.html)

- `teal.reporter.rmd_yaml_args`: passed to
  [`teal.reporter::download_report_button_srv()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/download_report_button.html)

- `teal.reporter.global_knitr`: passed to
  [`teal.reporter::download_report_button_srv()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/download_report_button.html)

## Usage

``` r
reporter_previewer_module(label = "Report Previewer", server_args = list())
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For
  [`modules()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  defaults to `"root"`. See `Details`.

- server_args:

  (named `list`) Arguments will overwrite the default `teal.reporter`
  options described in the description.

## Value

`teal_module` (extended with `teal_module_previewer` class) containing
the `teal.reporter` previewer functionality.
