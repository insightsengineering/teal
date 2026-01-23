# Copy functions to `teal` namespace

Useful when we require function from other namespace where this function
calls other functions from `teal` namespace (see `as.teal_slices`,
`c.teal_slices`).

## Usage

``` r
.copy_to_teal(fun)
```
