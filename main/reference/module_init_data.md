# Data Module for teal

This module manages the `data` argument for `srv_teal`. The `teal`
framework uses
[`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html),
which can be provided in various ways:

1.  Directly as a
    [`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
    object. This will automatically convert it into a `reactive`
    `teal_data`.

2.  As a `reactive` object that returns a
    [`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
    object.

## Usage

``` r
ui_init_data(id)

srv_init_data(id, data)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- data:

  (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
  The data which application will depend on.

## Value

A `reactive` object that returns: Output of the `data`. If `data` fails
then returned error is handled (after
[`tryCatch()`](https://rdrr.io/r/base/conditions.html)) so that rest of
the application can respond to this respectively.

## Details

### Reactive `teal_data`:

The data in the application can be reactively updated, prompting
[`srv_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md)
to rebuild the content accordingly. There are two methods for creating
interactive `teal_data`:

1.  Using a `reactive` object provided from outside the `teal`
    application. In this scenario, reactivity is controlled by an
    external module, and `srv_teal` responds to changes.

2.  Using
    [`teal_data_module()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md),
    which is embedded within the `teal` application, allowing data to be
    resubmitted by the user as needed.

Since the server of
[`teal_data_module()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
must return a `reactive` `teal_data` object, both methods (1 and 2)
produce the same reactive behavior within a `teal` application. The
distinction lies in data control: the first method involves external
control, while the second method involves control from a custom module
within the app.
