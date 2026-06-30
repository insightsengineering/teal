# Internal module handling list of [`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)

This module calls
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
sequentially by passing output from one step to the next. Modules have
error/warning handling feature and highlight containers when something
goes wrong.

## Usage

``` r
.ui_transform_teal_data(id, transformators, class = "well")

.srv_transform_teal_data(
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

  (`list` of `teal_transform_module`) decorator modules to apply
  sequentially to `data`. Each transformator receives the output of the
  previous one as input.

- class:

  **\[deprecated\]** No longer used.

- data:

  (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
  The data which application will depend on.

- modules:

  **\[deprecated\]** No longer used.

- is_transform_failed:

  **\[deprecated\]** No longer used.

## Value

`shiny.tag`

`reactive` `teal_data` object
