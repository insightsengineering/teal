# `teal` main module

Module to create a `teal` app as a Shiny Module.

## Usage

``` r
ui_teal(id, modules)

srv_teal(
  id,
  data,
  modules,
  filter = teal_slices(),
  reporter = teal.reporter::Reporter$new()
)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- modules:

  (`teal_modules`) `teal_modules` object. These are the specific output
  modules which will be displayed in the `teal` application. See
  [`modules()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  and
  [`module()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  for more details.

- data:

  (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
  The data which application will depend on.

- filter:

  (`teal_slices`) Optionally, specifies the initial filter using
  [`teal_slices()`](https://insightsengineering.github.io/teal/reference/teal_slices.md).

- reporter:

  (`Reporter`) object used to store report contents. Set to `NULL` to
  globally disable reporting.

## Value

`NULL` invisibly

## Details

This module can be used instead of
[`init()`](https://insightsengineering.github.io/teal/reference/init.md)
in custom Shiny applications. Unlike
[`init()`](https://insightsengineering.github.io/teal/reference/init.md),
it doesn't automatically include
[`module_session_info`](https://insightsengineering.github.io/teal/reference/module_session_info.md).

Module is responsible for creating the main `shiny` app layout and
initializing all the necessary components. This module establishes
reactive connection between the input `data` and every other component
in the app. Reactive change of the `data` passed as an argument, reloads
the app and possibly keeps all input settings the same so the user can
continue where one left off.

### data flow in `teal` application

This module supports multiple data inputs but eventually, they are all
converted to `reactive` returning `teal_data` in this module. On this
`reactive teal_data` object several actions are performed:

- data loading in
  [`module_init_data`](https://insightsengineering.github.io/teal/reference/module_init_data.md)

- data filtering in
  [`module_filter_data`](https://insightsengineering.github.io/teal/reference/module_filter_data.md)

- data transformation in
  [`module_transform_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)

### Fallback on failure

`teal` is designed in such way that app will never crash if the error is
introduced in any custom `shiny` module provided by app developer (e.g.
[`teal_data_module()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md),
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)).
If any module returns a failing object, the app will halt the evaluation
and display a warning message. App user should always have a chance to
fix the improper input and continue without restarting the session.
