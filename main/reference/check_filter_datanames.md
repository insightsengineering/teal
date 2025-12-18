# Check `datanames` in filters

This function checks whether `datanames` in filters correspond to those
in `data`, returning character vector with error messages or `TRUE` if
all checks pass.

## Usage

``` r
check_filter_datanames(filters, datanames)
```

## Arguments

- filters:

  (`teal_slices`) object

- datanames:

  (`character`) names of datasets available in the `data` object

## Value

A `character(1)` containing error message or TRUE if validation passes.
