# Deep copy `teal_slices`

it's important to create a new copy of `teal_slices` when starting a new
`shiny` session. Otherwise, object will be shared by multiple users as
it is created in global environment before `shiny` session starts.

## Usage

``` r
deep_copy_filter(filter)
```

## Arguments

- filter:

  (`teal_slices`)

## Value

`teal_slices`
