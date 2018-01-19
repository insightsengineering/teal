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

 * new '01_getting_started' vignette
 * datanames in FilteredData are now case sensitive

# teal 0.0.1

March 28, 2016 - Initial Release
