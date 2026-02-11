# Render decorator entries as tree nodes

Renders global decorator labels and object-specific decorator groups as
sibling entries under a common prefix. Reuses
[`format_tree_leaves()`](https://insightsengineering.github.io/teal/reference/format_tree_leaves.md)
for rendering children within object groups.

## Usage

``` r
format_decorator_entries(decorators_info, prefix)
```

## Arguments

- decorators_info:

  (`list`) with `global` (character vector) and `objects` (named list)
  components, as returned by
  [`format_decorators_tree()`](https://insightsengineering.github.io/teal/reference/format_decorators_tree.md)

- prefix:

  (`character(1)`) Prefix for each line

## Value

(`character(1)`) Formatted string with tree connectors
