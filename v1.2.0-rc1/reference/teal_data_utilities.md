# `teal_data` utils

In `teal` we need to recreate the `teal_data` object due to two
operations:

- we need to append filter-data code and objects which have been
  evaluated in `FilteredData` and we want to avoid double-evaluation.

- we need to subset `teal_data` to `datanames` used by the module, to
  shorten obtainable R-code

## Usage

``` r
.append_evaluated_code(data, code, filter_states)

.append_modified_data(data, objects)

.collapse_subsequent_chunks(card)
```

## Arguments

- data:

  (`teal_data`)

- code:

  (`character`) code to append to the object's code slot.

- objects:

  (`list`) objects to append to object's environment.

## Value

modified `teal_data`

## Details

Due to above recreation of `teal_data` object can't be done simply by
using public `teal.code` and `teal.data` methods.
