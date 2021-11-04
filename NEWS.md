# teal 0.10.0.9008

### Breaking changes
* Replaced `rcd_dataset_connector` and `rcd_cdisc_dataset_connector` with `scda_dataset_connector` and `scda_cdisc_dataset_connector` respectively.
* Removed `rcd_connection` and `rcd_data`; `scda_dataset_connectors` can be passed into `cdisc_data` and `teal_data` directly.
* Running `teal` application via `ui_teal_with_splash` and `srv_teal_with_splash` is now no longer recommended because it doesn't support some new features (e.g. logging, bookmarking). Use `init` instead.
* Updated `init` to accept `RelationalData`, `data.frame`, `Dataset`, `DatasetConnector`, `list` or a function returning a named list as data input.

### New features
* Added print methods to the `DatasetConnector`, `RelationalData`, and `RelationalDataconnector` classes and added input validation to the implementation of the print method that was already in the `Dataset` object.
* Added public facing constructor functions for `CDISCDataConnector`, `RelationalDataConnector`, and `DataConnection` classes.
* Modified `data_extract_spec` to allow both the `filter` and `select` parameters to be `NULL`, which results in the `data_extract_input` acting as if a `filter_spec` with all variables as possible choices had been supplied as the `filter` argument and a `select_spec` with the `multiple` parameter set to `TRUE` had been supplied as the `select` argument.
* Added support for logging using the `logger` package. Added a new function `register_logger`, which registers a logger in a given namespace.

### Miscellaneous
* Replaced the servers from `DataConnection`, `RelationalDataConnector`, `DatasetConnector`, and `RelationalData` with `moduleServer`.
* Updated R version requirement to >= 3.6.

# teal 0.10.0

### New features
* Allow passing `MultiAssayExperiment` to the `teal::init` using `mae_dataset` function or through
the connectors.
* Refactored filter panel to use `MultiAssayExperiment` objects. Filters can be set on a subject level
(`colData` of `MAE` object) and on a experiment level (`colData` and `rowData` of an assay).
* Added `cdse_dataset_connector` to create delayed data objects from `CDSE`.
* Added `datasetdb_dataset_connector` to create delayed data objects from `DataSetDB`.
* Added `ricepass_connection` to create delayed data objects from `entimICE` via `ricepass`.
* Refactor of the filter panel:
    * Simplified setting of initial filter state without need to specify "choices" or "range" named list element depending on the variable class.
    * `Dataset` type determines an appearance and a functionality of related filters and filters summary.
    * `Datasets` are passed (by reference) from `DDL` to `FilteredData` skipping extracting data and
    their attributes.
    * Redesigned variable filter labels in "Active Filter Variables" panel.
    * Fully testable server functions.

### Bug fixes
* Fixed the bug caused by calling `mutate_dataset` multiple times on the same `DatasetConnector` or `Dataset` object.
* Fixed a bug that caused the output of `get_code` function to not reproduce its raw data set.
* Changed `filter_spec` to allow no variable selection upon app initialization, where the first possible value was previously selected.

### Enhancements
* `modules` parameter of `teal::init` function can also receive a `list` except `root_modules`
function call.
* Added `split` and `merge` methods to the `JoinKeys` object.
* Added `all_choices()` as a possible argument to the `selected` parameter of `filter_spec`, `select_spec` and `choices_selected` indicating that all choices are selected.
* The `append` method of a `CodeClass` object has been modified to print a warning message when the argument does not result in any code being added because it is duplicated.
* Implemented delayed functionality to the mutate method of the `Dataset` and `DatasetConnector` objects.
* Modified `teal_data` to return a `CDISCData` object whenever any of its arguments is a type of `CDISCData` object.
* Updated filters to show both levels of a logical variable TRUE/FALSE even if one is missing from the original array.

### Miscellaneous
* Updated `LICENCE` and `README` with new package references.
* Added a method `get_hash` to the `Dataset` class returning the MD5 hash of the object stored inside the `Dataset` object.
* Replaced `random.cdisc.data` with `scda` in examples and tests.
* Implemented functionality to store `JoinKeys` in `Dataset` and `DatasetConnector` classes.
* Added `error_on_lintr: TRUE` to `.lintr`
* The pipe operator `%>%` is now exported such that downstream code and packages can use it.
* Removed hyperlinks to the `rice` package from the documentation.

# teal 0.9.5

### Enhancements
* Added informational stop message when using `mutate_data` with `RelationalDataConnector`.
* Modified `as_cdisc` to behave similarly to `cdisc_dataset` when called on a `Dataset` object.
* Changed the displayed format of the data name and the column name in `data_extract_spec` UI elements. Both are now compressed to `<data name>.<column name>` if they don't change during runtime of the app.
* Added `ADSAFTTE` to the list of recognized ADaM dataset names.
* Added another example to `data_extract_spec`'s doc string showcasing app users can choose a variable used for filtering in the encoding panel.
* Added CSS styling to tool tips in teal modules.

### Bug fixes
* Fixed an edge case error when creating a filter on variable with all missing values crashed the app.
* Fixed a bug that crashes app whenever a `Date` or `datetime` column is selected from a `filter_spec`.

# teal 0.9.4

### Enhancements
* Released `snowflake` connection and connectors.
* Changed ordering of datasets to be more intuitive (topologically first for CDISC datasets only and then according to input datasets order).
* When closing a teal app (ending a user shiny session), all `DataConnection`s will now try to close their connections.
* Added ADHY keys to configuration file.
* Extended the `filter_spec` function: the parameter `choices` is no longer mandatory (the function will take all possible choices by default) and the `vars` parameter additionally accepts the `choices_selected` and allows to change the variables for filtering using the UI elements in the encoding panel.

### Bug fixes
* Cleaned up imports in the package.
* Modified `value_choices` function to handle edge case when `"NA"` and  `NA` values exist in the `character` column that choices are derived from.\
* Fixed issue with cloning `Callable` class.

# teal 0.9.3

### New Features
* Support for data-standard independent input and filtering. That includes refactor of the all data and dataset structures together with refactor of `FilteredData` class.
* New `JoinKeys` class (with `join_keys()` constructors and `join_key()` constructor for its elements) to store joining key columns between datasets.
* Refactored the most basic `dataset()` constructor, added `cdisc_dataset()` constructor and `as_cdisc()` conversion function.
* Soft-deprecate removed class constructors and obsolete functions (e.g. `keys()`).
* Added `get_keys()` and `set_keys()` functions to extract and manipulate datasets primary keys respectively.
* Unexported `filtered_data_new`, `filtered_data_set` and `filtered_data_set_filters`.

### Bug fixes
* Duplicated lines of code passed to `teal::cdisc_dataset` and other `teal::RelationalDataset` constructors should now be shown when getting the code from `teal::cdisc_data` objects and other `teal::RelationalData` objects.
* Added ability to press "Enter" key without having to set focus to the Submit button for delayed data loading.
* Allow `variable_choices` to use datasets with missing labels.
* Fixed bug that ignores input of `NULL` to `selected` argument of `select_spec` function.

### Enhancements
* Added button to remove all active filters from the Filter Panel.

# teal 0.9.2

### New Features
* Added `python_dataset_connector` to create delayed data objects from python scripts or directly from python code.
* NOTE: `python_dataset_connector` is not yet ready to be deployed on RSConnect because it does not contain `numpy` and `pandas`, which are `Python` libraries used in `python_dataset_connector`.
* Added support for filtering on `Date` and `Datetime` variables in the Filter Panel.
* Added buttons for `date` and `datetime` filter widgets to reset the value to the original.
* Added new function `check_key_duplicates`, which creates a short summary about rows with duplicated primary key (row numbers and the number of duplicates)

### Enhancements
* Fixed lack of labels for `character` and `factor` variables in the Filter Panel.
* All variables are now displayed in `module_filter_panel`, not only those of types `numeric`, `logical`, `factor`, `character` and `Date`
* Fixed `mutate_data` to accept the whole scope of objects for `vars`.
* Clarified `teal::init` function documentation to state that custom css loading code with `htmltools::htmlDependency` should be included in the `header` argument rather than inside `ui` arguments of modules.
* Enabled empty select field inside `data_extract_spec`.
* Added new argument `drop_keys` to `filter_spec` to decide whether to drop or keep keys columns on single filter on those columns.
* Added a new optional argument `keys` to `variable_choices`. `keys` specifies the names of the variables, which should have the new key icon shown next to them in the variable drop down menus in the left-hand side encoding panels instead of the icon appropriate for their original R variable type. `variable_choices` now also works with `RelationalDataset` and `RelationalDatasetConnector` objects.

### Miscellaneous
* Removed `include_factors` option in `get_class_colnames` in `RawDataset`.

# teal 0.9.1

* Adds method to resolve nested lists containing delayed data objects, which can be used for `arm_ref_comp` objects.
* Nested tabs module now has better alignment with the filter panel on the page.
* Allow `width` argument in `optionalSelectInput`.
* Added lifecycle badges to all exported functions.
* Added new `code_dataset_connector` and `code_cdisc_dataset_connector` functions which enable the creation of new delayed data objects given a string of code.
* Added new functions `csv_dataset_connector` and `csv_cdisc_dataset_connector`.
* Updated `set_ui_input` method of `RawDatasetConnector` and `NamedDatasetConnector` to handle user defined shiny inputs.
* Include `Keep Inf` checkbox for numerical filter items. `Keep NA` and `Keep Inf` checkbox doesn't appear if there are no missing or infinite values.
* Replace existing `RelationalData` class with abstract class `RelationalDataCollection` and rename `RelationalDataList` class as `RelationalData`. The `data` argument to `teal::init` is now always a `RelationalData` object.
* Added `fun_cdisc_dataset_connector` to enable providing a custom function which returning a dataset.
* Removed `code` and `script` arguments from `as_relational` wrapper. This is intended to be done with `mutate_dataset` functionality.
* `filer` argument in `init` has added a validation step to ensure compatibility with the rest of the app. Variables inherited from ADSL have to be specified only for ADSL dataset.
* Fixes the issue with connection close code not being present in `get_code` results.
* Fixes the issue of occasional incorrect ordering of bar charts on the filter panel.
* More informative error displayed when `pull_fun` of `DataConnection` produces an error.

# teal 0.9.0

* `cdisc_dataset` and `dataset` now return R6 class objects (`RelationalDataset`).
* A new `teal_data` function to include datasets and connectors into teal application.
* `cdisc_data` function to include datasets and connectors into teal application where a `check` argument still could be used and other consistency tests are performed.
* `get_raw_data` can be used to derive raw data from R6 objects e.g. (`RelationalDataset`).
* `RawDatasetConnector`, `NamedDatasetConnector` and `RelationalDatasetConnector` to execute custom function call in order to get data from connection.
* `CodeClass` to manage reproducibility of the data and relationships between datasets. Not directly exposed to the public interface.
* `mutate_dataset` allows to modify dataset or connector via `code` argument or an R script.
* `mutate_data` allows to change any dataset in `RelationalData`, `RelationalDataConnector` or `RelationalDataList`.
* New wrapper functions to manipulate `RelationalDatasetConnector` and `RelationalDataset` such as `get_dataset`, `load_dataset`, `as_relational`.
* New wrapper functions to manipulate `RelationalDataConnector`, `RelationalData` and `RelationalDataList` such as `get_datasets`, `load_datasets`.
* `choices_labeled`, `filter_spec`, `select_spec`, `data_extract_spec`, `value_choices`,
`variable_choices` as S3 class applied on `data.frame` and also on delayed data.

* You can no longer modify the `app$datasets`, but must instead use argument `filter` in the `init` function.
* New modules were created to create a module of nested teal modules, then another one that adds the right filter pane to each tab. The `teal::init` function stays unchanged.
* The `teal::init` function now returns a `UI` function with an optional `id` argument. This allows to embed it into other applications. A split view of two teal applications side-by-side is one such example and shown in a vignette. `teal::init` was turned into a wrapper function around `module_teal_with_splash.R` and developers that want to embed teal as a Shiny module should directly work with these functions (`ui_teal_with_splash` and `srv_teal_with_splash`) instead of `teal::init`.
* The `teal::init` function now has a title parameter to set the title of the browser window.
* Missing data `NA` is now explicitly addressed in the filter panel: `NA`s are excluded by default and a checkbox to include them was added.
* Statistics of the data are visually depicted in terms of histograms or bar charts overlayed onto the Shiny input elements.
* Added buttons to remove all filters applied to a dataset.
* Restored the functionality to hide the filter panel for a module when it was constructed with `filters = NULL`.
* Moved helper functions into `utils.nest` and removed unused functions `set_labels_df` and `get_labels_df`.
* `optionalSelectInput` now allows for grouped choices.

## Refactor of FilteredData (for developers)

* `FilteredData` is now fully reactive. Now filtered data is lazy evaluated as per need. This further opens the door to bookmarking `teal` apps (bookmarking currently works for the right filtering panel, but we will make this feature more sophisticated in a future release, each module must be reviewed and adapted if it contains `reactiveValues`).
* Datasets and materialized connectors are provided to `FilteredData` by `set_datasets_data` function located in `init_datasets.R` file.
* Renamed `get_dataset()` method to `get_data()`.
* Renamed `get_filter_call()` method to `get_filter_expr()`; returns an expression rather than a list.
* Removed argument `isolate` from `get_data()` method and similar methods. You must `isolate` it yourself as needed. If you want to temporarily deactivate Shiny errors due to missing errors, you can set `options(shiny.suppressMissingContextError = TRUE)`. In general, avoid `isolate` as this breaks reactivity.
* We added a development module to add several filters at once, e.g. safety filters. This is to be evaluated before it is converted into a proper module and made available to end-users.


# teal 0.8.5

* UI bug fix to hide filter elements for not used datasets.

# teal 0.8.4

* Progress bar for ui creation in delayed loading module.
* Change output of `keys` function to `keys` object.
* Delayed version of `choices_selected`.
* Fix an error in `choices_selected` when `selected` is not in `choices`.
* Fix `pickerInput` not to display column name as label if it's missing.

# teal 0.8.3

* Enable `teal` app to initialize without data. The data are then loaded from within the teal app.
* New classes (`DatasetConnector`, `DataConnector`) to connect to various data sources, including:
        * connector to `rice` API - `rice_data` and `rice_dataset_connector`
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
