# Filter panel module in teal

Creates filter panel module from `teal_data` object and returns
`teal_data`. It is build in a way that filter panel changes and anything
what happens before (e.g.
[`module_init_data`](https://insightsengineering.github.io/teal/reference/module_init_data.md))
is triggering further reactive events only if something has changed and
if the module is visible. Thanks to this special implementation all
modules' data are recalculated only for those modules which are
currently displayed.

## Usage

``` r
ui_filter_data(id)

srv_filter_data(id, datasets, active_datanames, data, is_active)

.make_filtered_teal_data(modules, data, datasets = NULL, datanames)

.observe_active_filter_changed(datasets, is_active, active_datanames, data)

.get_filter_expr(datasets, datanames)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- datasets:

  (`reactive` returning `FilteredData` or `NULL`) When `datasets` is
  passed from the parent module (`srv_teal`) then `dataset` is a
  singleton which implies the filter-panel to be "global". When `NULL`
  then filter-panel is "module-specific".

- active_datanames:

  (`reactive` returning `character`) this module's data names

- data:

  (`reactive` returning `teal_data`)

- modules:

  (`teal_modules`) `teal_modules` object. These are the specific output
  modules which will be displayed in the `teal` application. See
  [`modules()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  and
  [`module()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  for more details.

## Value

A `eventReactive` containing `teal_data` containing filtered objects and
filter code. `eventReactive` triggers only if all conditions are met:

- tab is selected (`is_active`)

- when filters are changed (`get_filter_expr` is different than
  previous)
