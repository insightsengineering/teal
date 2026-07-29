# Manage multiple `FilteredData` objects

Oversee filter states across the entire application.

## Usage

``` r
ui_filter_manager_panel(id)

srv_filter_manager_panel(id, slices_global)

ui_filter_manager(id)

srv_filter_manager(id, slices_global)

srv_module_filter_manager(id, module_fd, slices_global)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- slices_global:

  (`reactiveVal`) containing `teal_slices`.

- module_fd:

  (`FilteredData`) Object containing the data to be filtered in a single
  `teal` module.

## Value

Module returns a `slices_global` (`reactiveVal`) containing a
`teal_slices` object with mapping.

## Slices global

The key role in maintaining the module-specific filter states is played
by the `.slicesGlobal` object. It is a reference class that holds the
following fields:

- `all_slices` (`reactiveVal`) - reactive value containing all filters
  registered in an app.

- `module_slices_api` (`reactiveValues`) - reactive field containing
  references to each modules' `FilteredData` object methods. At this
  moment it is used only in `srv_filter_manager` to display the filter
  states in a table combining informations from `all_slices` and from
  `FilteredData$get_available_teal_slices()`.

During a session only new filters are added to `all_slices` unless
[`module_snapshot_manager`](https://insightsengineering.github.io/teal/reference/module_snapshot_manager.md)
is used to restore previous state. Filters from `all_slices` can be
activated or deactivated in a module which is linked (both ways) by
`attr(, "mapping")` so that:

- If module's filter is added or removed in its `FilteredData` object,
  this information is passed to `SlicesGlobal` which updates
  `attr(, "mapping")` accordingly.

- When mapping changes in a `SlicesGlobal`, filters are set or removed
  from module's `FilteredData`.

## Filter manager

Filter-manager is split into two parts:

1.  `ui/srv_filter_manager_panel` - Called once for the whole app. This
    module observes changes in the filters in `slices_global` and
    displays them in a table utilizing information from `mapping`:

- (`TRUE`) - filter is active in the module

- (`FALSE`) - filter is inactive in the module

- (`NA`) - filter is not available in the module

1.  `ui/srv_module_filter_manager` - Called once for each `teal_module`.
    Handling filter states for of single module and keeping module
    `FilteredData` consistent with `slices_global`, so that local
    filters are always reflected in the `slices_global` and its mapping
    and vice versa.
