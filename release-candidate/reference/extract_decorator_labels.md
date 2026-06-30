# Extract decorator labels from a decorator structure

Recursively extracts labels from decorators, handling both single
decorators and nested list structures.

## Usage

``` r
extract_decorator_labels(dec)
```

## Arguments

- dec:

  Decorator object or list of decorators

## Value

Character vector of decorator labels, or `character(0L)` if none found
