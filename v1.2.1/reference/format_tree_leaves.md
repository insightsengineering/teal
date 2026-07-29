# Render labels as tree leaf nodes

Takes a character vector of labels and renders them as tree leaves with
proper `|-` / `L-` connectors.

## Usage

``` r
format_tree_leaves(labels, prefix)
```

## Arguments

- labels:

  (`character`) Labels to render as leaf nodes

- prefix:

  (`character(1)`) Prefix for each line (indentation and pipe
  characters)

## Value

(`character(1)`) Formatted string with tree connectors
