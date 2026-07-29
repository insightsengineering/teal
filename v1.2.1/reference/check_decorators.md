# Check decorators list

Check if a decorators list is valid and matches the expected output
names.

## Usage

``` r
check_decorators(x, names = NULL)

assert_decorators(x, names = NULL, .var.name = checkmate::vname(x), add = NULL)
```

## Arguments

- x:

  (named `list`) of
  [`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
  objects, or nested lists thereof.

- names:

  (`character`) optional vector of valid output names. When provided,
  all names in `x` must be one of these names, and names must be unique.

- .var.name:

  \[`character(1)`\]\
  The custom name for `x` as passed to any `assert*` function. Defaults
  to a heuristic name lookup.

- add:

  If an `AssertCollection` is provided, the error message is stored in
  it. If `NULL`, an exception is raised if res is not `TRUE`.

## Value

`TRUE` if valid, otherwise a `character(1)` string describing the
problem.

## See also

[`module_transform_data()`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)

## Examples

``` r
decorator <- teal_transform_module(server = function(id, data) data)
check_decorators(decorator)
#> [1] "The named list can contain a list of 'teal_transform_module' objects created using `teal_transform_module()` or be a `teal_transform_module` object."
check_decorators(list(all = decorator))
#> [1] TRUE
check_decorators(list(all = decorator, output = decorator))
#> [1] TRUE
check_decorators(list(all = decorator, output = list(decorator, decorator)))
#> [1] TRUE
```
