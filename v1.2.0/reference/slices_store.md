# Store and restore `teal_slices` object

Functions that write a `teal_slices` object to a file in the `JSON`
format, and also restore the object from disk.

## Usage

``` r
slices_store(tss, file)

slices_restore(file)
```

## Arguments

- tss:

  (`teal_slices`) object to be stored.

- file:

  (`character(1)`) file path where `teal_slices` object will be saved
  and restored. The file extension should be `".json"`.

## Value

`slices_store` returns `NULL`, invisibly.

`slices_restore` returns a `teal_slices` object restored from the file.

## Details

Date and date time objects are stored in the following formats:

- `Date` class is converted to the `"ISO8601"` standard (`YYYY-MM-DD`).

- `POSIX*t` classes are converted to character by using
  `format.POSIX*t(usetz = TRUE, tz = "UTC")` (`YYYY-MM-DD HH:MM:SS UTC`,
  where `UTC` is the `Coordinated Universal Time` timezone short-code).

This format is assumed during `slices_restore`. All `POSIX*t` objects in
`selected` or `choices` fields of `teal_slice` objects are always
printed in `UTC` timezone as well.

## See also

[`teal_slices()`](https://insightsengineering.github.io/teal/reference/teal_slices.md)
