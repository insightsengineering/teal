# Add a Custom Server Logic to `teal` Application

Adds a custom server function to the `teal` app. This function can
define additional server logic.

## Usage

``` r
teal_extend_server(x, custom_server, module_id = character(0))
```

## Arguments

- x:

  (`teal_app`) A `teal_app` object created using the `init` function.

- custom_server:

  (`function(input, output, session)` or `function(id, ...)`) The custom
  server function or server module to set.

- module_id:

  (`character(1)`) The ID of the module when a module server function is
  passed.
