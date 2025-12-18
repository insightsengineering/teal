# Module to transform `reactive` `teal_data`

Module calls
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
in sequence so that `reactive teal_data` output from one module is
handed over to the following module's input.

## Usage

``` r
ui_transform_teal_data(id, transformators, class = "well")

srv_transform_teal_data(
  id,
  data,
  transformators,
  modules = NULL,
  is_transform_failed = reactiveValues()
)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/articles/transform-input-data.md).

- class:

  (character(1)) CSS class to be added in the `div` wrapper tag.

- data:

  (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
  The data which application will depend on.

- modules:

  (`teal_modules` or `teal_module`) For `datanames` validation purpose

- is_transform_failed:

  (`reactiveValues`) contains `logical` flags named after each
  transformator. Help to determine if any previous transformator failed,
  so that following transformators can be disabled and display a generic
  failure message.

## Value

`reactive` `teal_data`
