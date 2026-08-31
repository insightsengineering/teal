# Subset decorators based on the scope

Subset a named list of decorators, keeping only those matching the
requested `scope` together with any decorators registered under the
`"all"` name. This is a helper for module developers to resolve the
decorators applied to a specific output when a module produces several
decorable outputs.

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

## See also

[`check_decorators()`](https://insightsengineering.github.io/teal/reference/check_decorators.md)

## Examples

``` r
plot_decorator <- teal_transform_module(server = function(id, data) data)
table_decorator <- teal_transform_module(server = function(id, data) data)
decorators <- list(all = plot_decorator, table = table_decorator)

# Decorators for the "table" output: both "all" and "table" scoped decorators.
str(select_decorators(decorators, "table"))
#> List of 2
#>  $ :List of 2
#>   ..$ ui    : NULL
#>   ..$ server:function (id, data)  
#>   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 135 16 155 7 16 7 135 155
#>   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x561abd167ac0> 
#>   ..- attr(*, "label")= chr "transform module"
#>   ..- attr(*, "datanames")= chr "all"
#>   ..- attr(*, "class")= chr [1:2] "teal_transform_module" "teal_data_module"
#>  $ :List of 2
#>   ..$ ui    : NULL
#>   ..$ server:function (id, data)  
#>   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 135 16 155 7 16 7 135 155
#>   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x561abd167ac0> 
#>   ..- attr(*, "label")= chr "transform module"
#>   ..- attr(*, "datanames")= chr "all"
#>   ..- attr(*, "class")= chr [1:2] "teal_transform_module" "teal_data_module"

# An unknown scope keeps only the "all" decorators.
str(select_decorators(decorators, "plot"))
#> List of 1
#>  $ :List of 2
#>   ..$ ui    : NULL
#>   ..$ server:function (id, data)  
#>   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 135 16 155 7 16 7 135 155
#>   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x561abd167ac0> 
#>   ..- attr(*, "label")= chr "transform module"
#>   ..- attr(*, "datanames")= chr "all"
#>   ..- attr(*, "class")= chr [1:2] "teal_transform_module" "teal_data_module"
```
