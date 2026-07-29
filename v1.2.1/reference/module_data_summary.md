# Data summary

Module and its utils to display the number of rows and subjects in the
filtered and unfiltered data.

## Usage

``` r
ui_data_summary(id)

srv_data_summary(id, data)

get_filter_overview_wrapper(teal_data)

get_filter_overview(current_data, initial_data, dataname, subject_keys)

get_filter_overview_array(current_data, initial_data, dataname, subject_keys)

get_filter_overview_MultiAssayExperiment(current_data, initial_data, dataname)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- data:

  (`reactive` returning `teal_data`)

- current_data:

  (`object`) current object (after filtering and transforming).

- initial_data:

  (`object`) initial object.

- dataname:

  (`character(1)`)

- subject_keys:

  (`character`) names of the columns which determine a single unique
  subjects

## Value

`NULL`.

## Details

Handling different data classes: `get_filter_overview()` is a pseudo S3
method which has variants for:

- `array` (`data.frame`, `DataFrame`, `array`, `Matrix` and
  `SummarizedExperiment`): Method variant can be applied to any
  two-dimensional objects on which
  [`ncol()`](https://rdrr.io/r/base/nrow.html) can be used.

- `MultiAssayExperiment`: for which summary contains counts for
  `colData` and all `experiments`.

- For other data types module displays data name with warning icon and
  no more details.

Module includes also "Show/Hide unsupported" button to toggle rows of
the summary table containing datasets where number of observations are
not calculated.
