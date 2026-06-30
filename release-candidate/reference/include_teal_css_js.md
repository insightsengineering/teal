# Code to include `teal` `CSS` and `JavaScript` files

This is useful when you want to use the same `JavaScript` and `CSS`
files that are used with the `teal` application. This is also useful for
running standalone modules in `teal` with the correct styles. Also
initializes `shinyjs` so you can use it.

## Usage

``` r
include_teal_css_js()
```

## Value

A `shiny.tag.list`.

## Details

Simply add `include_teal_css_js()` as one of the UI elements.
