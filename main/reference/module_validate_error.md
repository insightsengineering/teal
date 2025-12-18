# Execute and validate `teal_data_module`

This is a low level module to handle `teal_data_module` execution and
validation.
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
inherits from
[`teal_data_module()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
so it is handled by this module too.
[`srv_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md)
accepts various `data` objects and eventually they are all transformed
to `reactive`
[`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
which is a standard data class in whole `teal` framework.

## Usage

``` r
ui_validate_error(id)

srv_validate_error(id, data, validate_shiny_silent_error)

ui_check_class_teal_data(id)

srv_check_class_teal_data(id, data)

ui_check_module_datanames(id)

srv_check_module_datanames(id, data, modules)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- data:

  (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
  The data which application will depend on.

- validate_shiny_silent_error:

  (`logical`) If `TRUE`, then `shiny.silent.error` is validated and

- modules:

  (`teal_modules` or `teal_module`) For `datanames` validation purpose

## Value

`reactive` `teal_data`

## data validation

Executed
[`teal_data_module()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
is validated and output is validated for consistency. Output `data` is
invalid if:

1.  [`teal_data_module()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
    is invalid if server doesn't return `reactive`. **Immediately
    crashes an app!**

2.  `reactive` throws a `shiny.error` - happens when module creating
    [`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
    fails.

3.  `reactive` returns `qenv.error` - happens when
    [`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
    evaluates a failing code.

4.  `reactive` object doesn't return
    [`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html).

5.  [`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
    object lacks any `datanames` specified in the `modules` argument.

`teal` (observers in `srv_teal`) always waits to render an app until
`reactive` `teal_data` is returned. If error 2-4 occurs, relevant error
message is displayed to the app user. Once the issue is resolved, the
app will continue to run. `teal` guarantees that errors in data don't
crash the app (except error 1).
