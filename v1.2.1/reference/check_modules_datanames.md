# Check `datanames` in modules

These functions check if specified `datanames` in modules match those in
the data object, returning error messages or `TRUE` for successful
validation. Two functions return error message in different forms:

- `check_modules_datanames` returns `character(1)` for basic assertion
  usage

- `check_modules_datanames_html` returns `shiny.tag.list` to display it
  in the app.

## Usage

``` r
check_modules_datanames(modules, datanames)

check_reserved_datanames(datanames)

check_modules_datanames_html(modules, datanames)
```

## Arguments

- modules:

  (`teal_modules`) object

- datanames:

  (`character`) names of datasets available in the `data` object

## Value

`TRUE` if validation passes, otherwise `character(1)` or
`shiny.tag.list`
