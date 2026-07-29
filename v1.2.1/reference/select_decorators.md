# Subset decorators based on the scope

Subset decorators based on the scope

## Usage

``` r
select_decorators(decorators, scope)
```

## Arguments

- decorators:

  (named `list`) a named list of decorators to subset.

- scope:

  (`character(1)`) a decorator name to include.

## Value

A `list` of `teal_transform_module` objects matching the given `scope`
and `all`. Returns an empty list if `scope` and `all` is not found in
`decorators`.
