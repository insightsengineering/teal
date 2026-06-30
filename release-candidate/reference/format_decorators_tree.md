# Format decorators in a tree structure

Formats decorators showing global decorators and object-specific
decorators in a tree structure. Returns a list with 'global' and
'objects' components.

## Usage

``` r
format_decorators_tree(decorators)
```

## Arguments

- decorators:

  List of decorators (can be mixed: global and named lists)

## Value

List with 'global' (character vector) and 'objects' (named list)
components
