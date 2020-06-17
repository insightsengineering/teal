
# teal 0.8.4.9000

* issues with no comments:

* progress bar for ui creation in delayed loading module
* `cdisc_dataset` and `dataset` now return R6 class objects (RelationalDataset)
* `get_raw_data` can be used to derive raw data from R6 objects e.g. (RelationalDataset)
* `mutate_dataset` allows to change the data inside R6 objects via `code` argument or an R script
* new `RawDatasetConnector` and `RelationalDatasetConnector` to execute get data from remote connections.
* new wrapper functions to manipulate `DatasetConnector` and `Dataset` such as `get_dataset`, 
`load_dataset`, `as_relational`.
* Change output of `keys` function to `keys` object. Updated checks in `DatasetConnector` and `dataset`.
* delayed version of choices_selected
* Fix an error in `choices_selected` when `selected` is not in `choices`

* Argument `filter` in the `init` function was removed and the new argument `filter_states` was added. You can no longer modify the `app$datasets`, but must instead use this argument. 
* New modules were created to create a module of nested teal modules, then another one that adds the right filter pane to each tab. The `teal::init` function stays unchanged.
* The `teal::init` function now returns a `UI` function with an optional `id` argument. This allows it to be embedded into other applications. Split view as two applications side-by-side is one such example.

## FilteredData refactored (for developers)

* `FilteredData` is now fully reactive. Now filtered data is lazy evaluated as per need. This further opens the door to bookmarking `teal` apps (bookmarking currently works for the right filtering panel, but we will make this feature more sophisticated in a future release, each module must be reviewed and adapted if it contains `reactiveValues`).
* Renamed `get_dataset` method to `get_data()`.
* Renamed `get_filter_call` method to `get_filter_expr` and returns an expression rather than a list.
* Removed argument `isolate` form `get_data()` method and similar methods. You must `isolate` it yourself as needed. If you want to temporarily deactivate Shiny errors due to missing errors, you can set `options(shiny.suppressMissingContextError = TRUE)`.
* Missing data `NA` is now explicitly addressed in the filter panel: `NA`s are excluded by default and a checkbox to include them was added.
* Statistics of the data are visually depicted in terms of histograms or bar charts overlayed onto the Shiny input elements.


# teal 0.8.4

* Progress bar for ui creation in delayed loading module.
* Change output of `keys` function to `keys` object.
* Delayed version of choices_selected.
* Fix an error in `choices_selected` when `selected` is not in `choices`.
* Fix `pickerInput` not to display column name as label if it's missing.

# teal 0.8.3

* Enable `teal` app to initialize without data. The data are then loaded from within the teal app.
* New classes (`DatasetConnector`, `DataConnector`) to connect to various data sources, including:  
        * connector to `rice` API - `rice_cdisc_data` and `rice_dataset_connector`  
        * connector to `RDS` files - `rds_data` and `rds_dataset_connector`  
* Message appears at bottom right of Shiny app when Shiny is busy to update the views.
* Remove `labels` argument of `cdisc_data` function. Labels should now already be present in the data passed to the  `cdisc_data` function. This can be achieved using the `var_relabel` function.

# teal 0.8.2

* Fixed several BUGS in `optionalSelectInput` and improved inputs look.
* Added `get_data_labels` function to `FilteredData` class.
* Improved `sep` passing within `data_extract_spec`.

# teal 0.8.1

* Refactor `choices_labeled` and fix bug of not showing column name in `data_extract_spec`.

# teal 0.8.0

* Added `cdisc_dataset` (and more general `dataset`) functions to properly handle dataset keys while merging.
* Possibility to load custom `.css` and `.js` files.
* Renamed `columns_spec` to `select_spec`.
* Show number of observations on filter panel.
* Add labeling functions `variable_choices` and `value_choices`.

# teal 0.7.0

* Added functions `cdisc_data` and `get_code` to deal with preprocessing and moving a step towards data standard independent teal.
* Moved `teal.utils` functions to `teal`: `log_app_usage`, `stop_shiny`.
* Added `*_spec` functions.
* Improvements on usage of `PickerInput` and `SelectInput`.

# teal 0.6.0

* Removed deprecated functions `tab*`.
* Removed data generation functions including `generate_sample_data`.
* Incorporate `shinyjs` package.
* Added "Copy R code to clipboard" button.

# teal 0.0.5

* Added limit to data_table with scrolling, preventing overlap of ui elements.
* Boolean filtering.

# teal 0.0.4

* Bug fix where teal crashes when a filter variable gets added that has many decimal places.

# teal 0.0.3

* Note version 0.0.3 is not backwards compatible. Reading the changes and studying the example app should, however, clarify most the changes.

#### New features

 * `tm_scatterplot` module.
 * `tm_table` module.

#### Changes 
 
 * All `tabs` arguments were renamed to `modules`.
 * `tab_item` function is now called `module`.
 * `tab_items` function is now called `modules`.
 * `tabs` function was removed.
 * `variable_browser_item` is now called `tm_variable_browser`.
 * `data_table_item` is now called `tm_data_table`.
 * `datasets` argument is automatically added to the server functions specified with `module`. Hence `teal_datasets` has been removed as a `server_args` element.

# teal 0.0.2

 * New '01_getting_started' vignette.
 * Datanames in FilteredData are now case sensitive.

# teal 0.0.1

 * March 28, 2016 - Initial Release.
