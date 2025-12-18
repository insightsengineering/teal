# Pluralize a word depending on the size of the input

Pluralize a word depending on the size of the input

## Usage

``` r
pluralize(x, singular, plural = NULL)
```

## Arguments

- x:

  (`object`) to check length for plural.

- singular:

  (`character`) singular form of the word.

- plural:

  (optional `character`) plural form of the word. If not given an "s" is
  added to the singular form.

## Value

A `character` that correctly represents the size of the `x` argument.
