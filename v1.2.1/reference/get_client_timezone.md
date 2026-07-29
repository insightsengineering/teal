# Get client timezone

User timezone in the browser may be different to the one on the server.
This script can be run to register a `shiny` input which contains
information about the timezone in the browser.

## Usage

``` r
get_client_timezone(ns)
```

## Arguments

- ns:

  (`function`) namespace function passed from the `session` object in
  the `shiny` server. For `shiny` modules this will allow for proper
  name spacing of the registered input.

## Value

`NULL`, invisibly.
