# teal 0.8.2.9000

* Enable `teal` app to initialize with custom page to be loaded first.
* New classes (`DatasetConnector`, `DataConnector`) to connect to various data sources, including:  
        * connector to `rice` API - `rice_cdisc_data` and `rice_dataset`  
        * connector to `RDS` files - `rds_data` and `rds_dataset`  
* Message appears at bottom right of Shiny app when Shiny is busy to update the views.
* Remove `labels` argument of `cdisc_data` function. Labels now should be handled in preprocessing using 
`var_relabel` function.

# teal 0.8.2

* Fixed several BUGS in `optionalSelectInput` and improved inputs look.
* Added `get_data_labels` function to `FilteredData` class
* Improved `sep` passing within `data_extract_spec`

# teal 0.8.1

* refactor `choices_labeled` and fix bug of not showing column name in data_extract_spec

# teal 0.8.0

* Added `cdisc_dataset` (and more general `dataset`) functions to properly handle dataset keys while merging
* Possibility to load custom `.css` and `.js` files
* Renamed `columns_spec` to `select_spec`
* Show number of observations on filter panel
* Add labeling functions `variable_choices` and `value_choices`

# teal 0.7.0

* Added functions `cdisc_data` and `get_code` to deal with preprocessing and moving a step towards data standard
independent teal
* Moved `teal.utils` functions to `teal`: `log_app_usage`, `stop_shiny`
* Added `*_spec` functions
* Improvements on usage of `PickerInput` and `SelectInput`

# teal 0.6.0

* Removed deprecated functions `tab*`
* Removed data generation functions including `generate_sample_data`
* Incorporate `shinyjs` package
* Added "Copy R code to clipboard" button

# teal 0.0.5

* Added limit to data_table with scrolling, preventing overlap of ui elements
* Boolean filtering

# teal 0.0.4

* Bug fix where teal crashes when a filter variable gets added that has many
decimal places

# teal 0.0.3

Note version 0.0.3 is not backwards compatible. Reading the changes and studying
the example app should, however, clarify most the changes.

## New features

 * tm_scatterplot module
 * tm_table module

## Changes 
 
 * all `tabs` arguments were renamed to `modules`
 * `tab_item` function is now called `module`
 * `tab_items` function is now called `modules`
 * `tabs` function was removed
 * `variable_browser_item` is now called `tm_variable_browser`
 * `data_table_item` is now called `tm_data_table`
 * `datasets` argument is automatically added to the server functions specified
 with `module`. Hence `teal_datasets` has been removed as a `server_args`
 element.

# teal 0.0.2

 * New '01_getting_started' vignette
 * Datanames in FilteredData are now case sensitive

# teal 0.0.1

March 28, 2016 - Initial Release
