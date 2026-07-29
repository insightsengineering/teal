# Extract/Remove module(s) of specific class

Given a `teal_module` or a `teal_modules`, return the elements of the
structure according to `class`.

## Usage

``` r
extract_module(modules, class)

drop_module(modules, class)
```

## Arguments

- modules:

  (`teal_modules`)

- class:

  The class name of `teal_module` to be extracted or dropped.

## Value

- For `extract_module`, a `teal_module` of class `class` or
  `teal_modules` containing modules of class `class`.

- For `drop_module`, the opposite, which is all `teal_modules` of class
  other than `class`.

`teal_modules`
