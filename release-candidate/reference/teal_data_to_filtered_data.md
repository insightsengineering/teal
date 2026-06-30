# Create a `FilteredData`

Create a `FilteredData` object from a `teal_data` object.

## Usage

``` r
teal_data_to_filtered_data(x, datanames = names(x))
```

## Arguments

- x:

  (`teal_data`) object

- datanames:

  (`character`) vector of data set names to include; must be subset of
  `names(x)`

## Value

A `FilteredData` object.
