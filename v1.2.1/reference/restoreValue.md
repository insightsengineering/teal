# Restore value from bookmark.

Get value from bookmark or return default.

## Usage

``` r
restoreValue(value, default)
```

## Arguments

- value:

  (`character(1)`) name of value to restore

- default:

  fallback value

## Value

In an application restored from a server-side bookmark, the variable
specified by `value` from the `values` environment. Otherwise `default`.

## Details

Bookmarks can store not only inputs but also arbitrary values. These
values are stored by `onBookmark` callbacks and restored by
`onBookmarked` callbacks, and they are placed in the `values`
environment in the `session$restoreContext` field. Using
`teal_data_module` makes it impossible to run the callbacks because the
app becomes ready before modules execute and callbacks are registered.
In those cases the stored values can still be recovered from the
`session` object directly.

Note that variable names in the `values` environment are prefixed with
module name space names, therefore, when using this function in modules,
`value` must be run through the name space function.
