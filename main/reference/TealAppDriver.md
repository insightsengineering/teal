# Drive a `teal` application

Drive a `teal` application

Drive a `teal` application

## Details

Extension of the
[`shinytest2::AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
class with methods for driving a teal application for performing
interactions for `shinytest2` tests.

## Super class

[`shinytest2::AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
-\> `TealAppDriver`

## Methods

### Public methods

- [`TealAppDriver$new()`](#method-TealAppDriver-new)

- [`TealAppDriver$click()`](#method-TealAppDriver-click)

- [`TealAppDriver$expect_no_shiny_error()`](#method-TealAppDriver-expect_no_shiny_error)

- [`TealAppDriver$expect_no_validation_error()`](#method-TealAppDriver-expect_no_validation_error)

- [`TealAppDriver$expect_validation_error()`](#method-TealAppDriver-expect_validation_error)

- [`TealAppDriver$set_input()`](#method-TealAppDriver-set_input)

- [`TealAppDriver$navigate_teal_tab()`](#method-TealAppDriver-navigate_teal_tab)

- [`TealAppDriver$namespaces()`](#method-TealAppDriver-namespaces)

- [`TealAppDriver$get_active_module_input()`](#method-TealAppDriver-get_active_module_input)

- [`TealAppDriver$get_active_module_output()`](#method-TealAppDriver-get_active_module_output)

- [`TealAppDriver$get_active_module_table_output()`](#method-TealAppDriver-get_active_module_table_output)

- [`TealAppDriver$get_active_module_plot_output()`](#method-TealAppDriver-get_active_module_plot_output)

- [`TealAppDriver$set_active_module_input()`](#method-TealAppDriver-set_active_module_input)

- [`TealAppDriver$get_active_filter_vars()`](#method-TealAppDriver-get_active_filter_vars)

- [`TealAppDriver$get_active_data_summary_table()`](#method-TealAppDriver-get_active_data_summary_table)

- [`TealAppDriver$is_visible()`](#method-TealAppDriver-is_visible)

- [`TealAppDriver$expect_visible()`](#method-TealAppDriver-expect_visible)

- [`TealAppDriver$expect_hidden()`](#method-TealAppDriver-expect_hidden)

- [`TealAppDriver$get_active_data_filters()`](#method-TealAppDriver-get_active_data_filters)

- [`TealAppDriver$add_filter_var()`](#method-TealAppDriver-add_filter_var)

- [`TealAppDriver$remove_filter_var()`](#method-TealAppDriver-remove_filter_var)

- [`TealAppDriver$set_active_filter_selection()`](#method-TealAppDriver-set_active_filter_selection)

- [`TealAppDriver$get_attr()`](#method-TealAppDriver-get_attr)

- [`TealAppDriver$get_html_rvest()`](#method-TealAppDriver-get_html_rvest)

- [`TealAppDriver$open_url()`](#method-TealAppDriver-open_url)

- [`TealAppDriver$wait_for_active_module_value()`](#method-TealAppDriver-wait_for_active_module_value)

Inherited methods

- [`shinytest2::AppDriver$expect_download()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-expect_download)
- [`shinytest2::AppDriver$expect_html()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-expect_html)
- [`shinytest2::AppDriver$expect_js()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-expect_js)
- [`shinytest2::AppDriver$expect_screenshot()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-expect_screenshot)
- [`shinytest2::AppDriver$expect_text()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-expect_text)
- [`shinytest2::AppDriver$expect_unique_names()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-expect_unique_names)
- [`shinytest2::AppDriver$expect_values()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-expect_values)
- [`shinytest2::AppDriver$get_chromote_session()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_chromote_session)
- [`shinytest2::AppDriver$get_dir()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_dir)
- [`shinytest2::AppDriver$get_download()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_download)
- [`shinytest2::AppDriver$get_html()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_html)
- [`shinytest2::AppDriver$get_js()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_js)
- [`shinytest2::AppDriver$get_logs()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_logs)
- [`shinytest2::AppDriver$get_screenshot()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_screenshot)
- [`shinytest2::AppDriver$get_text()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_text)
- [`shinytest2::AppDriver$get_url()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_url)
- [`shinytest2::AppDriver$get_value()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_value)
- [`shinytest2::AppDriver$get_values()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_values)
- [`shinytest2::AppDriver$get_variant()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_variant)
- [`shinytest2::AppDriver$get_window_size()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-get_window_size)
- [`shinytest2::AppDriver$log_message()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-log_message)
- [`shinytest2::AppDriver$run_js()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-run_js)
- [`shinytest2::AppDriver$set_inputs()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-set_inputs)
- [`shinytest2::AppDriver$set_window_size()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-set_window_size)
- [`shinytest2::AppDriver$stop()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-stop)
- [`shinytest2::AppDriver$upload_file()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-upload_file)
- [`shinytest2::AppDriver$view()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-view)
- [`shinytest2::AppDriver$wait_for_idle()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-wait_for_idle)
- [`shinytest2::AppDriver$wait_for_js()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-wait_for_js)
- [`shinytest2::AppDriver$wait_for_value()`](https://rstudio.github.io/shinytest2/reference/AppDriver.html#method-wait_for_value)

------------------------------------------------------------------------

### Method `new()`

Initialize a `TealAppDriver` object for testing a `teal` application.

#### Usage

    TealAppDriver$new(
      app,
      options = list(),
      timeout = rlang::missing_arg(),
      load_timeout = rlang::missing_arg(),
      ...
    )

#### Arguments

- `app`:

  (`teal_app`)

- `options`:

  (`list`) passed to `shinyApp(options)`. See
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- `timeout`:

  (`numeric`) Default number of milliseconds for any timeout or
  timeout\_ parameter in the `TealAppDriver` class. Defaults to 20s.

  See
  [`shinytest2::AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
  `new` method for more details on how to change it via options or
  environment variables.

- `load_timeout`:

  (`numeric`) How long to wait for the app to load, in ms. This includes
  the time to start R. Defaults to 100s.

  See
  [`shinytest2::AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
  `new` method for more details on how to change it via options or
  environment variables

- `...`:

  Additional arguments to be passed to `shinytest2::AppDriver$new`

#### Returns

Object of class `TealAppDriver`

------------------------------------------------------------------------

### Method `click()`

Append parent
[`shinytest2::AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
`click` method with a call to `waif_for_idle()` method.

#### Usage

    TealAppDriver$click(...)

#### Arguments

- `...`:

  arguments passed to parent
  [`shinytest2::AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
  `click()` method.

------------------------------------------------------------------------

### Method `expect_no_shiny_error()`

Check if the app has shiny errors. This checks for global shiny errors.
Note that any shiny errors dependent on shiny server render will only be
captured after the teal module tab is visited because shiny will not
trigger server computations when the tab is invisible. So, navigate to
the module tab you want to test before calling this function. Although,
this catches errors hidden in the other module tabs if they are already
rendered.

#### Usage

    TealAppDriver$expect_no_shiny_error()

------------------------------------------------------------------------

### Method `expect_no_validation_error()`

Check if the app has no validation errors. This checks for global shiny
validation errors.

#### Usage

    TealAppDriver$expect_no_validation_error()

------------------------------------------------------------------------

### Method `expect_validation_error()`

Check if the app has validation errors. This checks for global shiny
validation errors.

#### Usage

    TealAppDriver$expect_validation_error()

------------------------------------------------------------------------

### Method `set_input()`

Set the input in the `teal` app.

#### Usage

    TealAppDriver$set_input(input_id, value, ...)

#### Arguments

- `input_id`:

  (character) The shiny input id with it's complete name space.

- `value`:

  The value to set the input to.

- `...`:

  Additional arguments to be passed to
  `shinytest2::AppDriver$set_inputs`

#### Returns

The `TealAppDriver` object invisibly.

------------------------------------------------------------------------

### Method `navigate_teal_tab()`

Navigate the teal tabs in the `teal` app.

#### Usage

    TealAppDriver$navigate_teal_tab(tab)

#### Arguments

- `tab`:

  (character) Labels of tabs to navigate to. Note: Make sure to provide
  unique labels for the tabs.

#### Returns

The `TealAppDriver` object invisibly.

------------------------------------------------------------------------

### Method `namespaces()`

`NS` in different sections of `teal` app

#### Usage

    TealAppDriver$namespaces(is_selector = FALSE)

#### Arguments

- `is_selector`:

  (`logical(1)`) whether `ns` function should prefix with `#`.

#### Returns

list of `ns`.

------------------------------------------------------------------------

### Method `get_active_module_input()`

Get the input from the module in the `teal` app. This function will only
access inputs from the name space of the current active teal module.

#### Usage

    TealAppDriver$get_active_module_input(input_id)

#### Arguments

- `input_id`:

  (character) The shiny input id to get the value from.

#### Returns

The value of the shiny input.

------------------------------------------------------------------------

### Method `get_active_module_output()`

Get the output from the module in the `teal` app. This function will
only access outputs from the name space of the current active teal
module.

#### Usage

    TealAppDriver$get_active_module_output(output_id)

#### Arguments

- `output_id`:

  (character) The shiny output id to get the value from.

#### Returns

The value of the shiny output.

------------------------------------------------------------------------

### Method `get_active_module_table_output()`

Get the output from the module's
[`teal.widgets::table_with_settings`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/table_with_settings.html)
or [`DT::DTOutput`](https://rdrr.io/pkg/DT/man/dataTableOutput.html) in
the `teal` app. This function will only access outputs from the name
space of the current active teal module.

#### Usage

    TealAppDriver$get_active_module_table_output(table_id, which = 1)

#### Arguments

- `table_id`:

  (`character(1)`) The id of the table in the active teal module's name
  space.

- `which`:

  (integer) If there is more than one table, which should be extracted.
  By default it will look for a table that is built using
  [`teal.widgets::table_with_settings`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/table_with_settings.html).

#### Returns

The data.frame with table contents.

------------------------------------------------------------------------

### Method `get_active_module_plot_output()`

Get the output from the module's
[`teal.widgets::plot_with_settings`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/plot_with_settings.html)
in the `teal` app. This function will only access plots from the name
space of the current active teal module.

#### Usage

    TealAppDriver$get_active_module_plot_output(plot_id)

#### Arguments

- `plot_id`:

  (`character(1)`) The id of the plot in the active teal module's name
  space.

#### Returns

The `src` attribute as `character(1)` vector.

------------------------------------------------------------------------

### Method `set_active_module_input()`

Set the input in the module in the `teal` app. This function will only
set inputs in the name space of the current active teal module.

#### Usage

    TealAppDriver$set_active_module_input(input_id, value, ...)

#### Arguments

- `input_id`:

  (character) The shiny input id to get the value from.

- `value`:

  The value to set the input to.

- `...`:

  Additional arguments to be passed to
  `shinytest2::AppDriver$set_inputs`

#### Returns

The `TealAppDriver` object invisibly.

------------------------------------------------------------------------

### Method `get_active_filter_vars()`

Get the active datasets that can be accessed via the filter panel of the
current active teal module.

#### Usage

    TealAppDriver$get_active_filter_vars()

------------------------------------------------------------------------

### Method `get_active_data_summary_table()`

Get the active data summary table

#### Usage

    TealAppDriver$get_active_data_summary_table()

#### Returns

`data.frame`

------------------------------------------------------------------------

### Method `is_visible()`

Test if `DOM` elements are visible on the page with a JavaScript call.

#### Usage

    TealAppDriver$is_visible(
      selector,
      content_visibility_auto = FALSE,
      opacity_property = FALSE,
      visibility_property = FALSE
    )

#### Arguments

- `selector`:

  (`character(1)`) `CSS` selector to check visibility. A `CSS` id will
  return only one element if the UI is well formed.

- `content_visibility_auto, opacity_property, visibility_property`:

  (`logical(1)`) See more information on
  <https://developer.mozilla.org/en-US/docs/Web/API/Element/checkVisibility>.

#### Returns

Logical vector with all occurrences of the selector.

------------------------------------------------------------------------

### Method `expect_visible()`

#### Usage

    TealAppDriver$expect_visible(
      selector,
      content_visibility_auto = FALSE,
      opacity_property = FALSE,
      visibility_property = FALSE,
      timeout
    )

------------------------------------------------------------------------

### Method `expect_hidden()`

#### Usage

    TealAppDriver$expect_hidden(
      selector,
      content_visibility_auto = FALSE,
      opacity_property = FALSE,
      visibility_property = FALSE,
      timeout
    )

------------------------------------------------------------------------

### Method `get_active_data_filters()`

Get the active filter variables from a dataset in the `teal` app.

#### Usage

    TealAppDriver$get_active_data_filters(dataset_name = NULL)

#### Arguments

- `dataset_name`:

  (character) The name of the dataset to get the filter variables from.
  If `NULL`, the filter variables for all the datasets will be returned
  in a list.

------------------------------------------------------------------------

### Method `add_filter_var()`

Add a new variable from the dataset to be filtered.

#### Usage

    TealAppDriver$add_filter_var(dataset_name, var_name, ...)

#### Arguments

- `dataset_name`:

  (character) The name of the dataset to add the filter variable to.

- `var_name`:

  (character) The name of the variable to add to the filter panel.

- `...`:

  Additional arguments to be passed to
  `shinytest2::AppDriver$set_inputs`

#### Returns

The `TealAppDriver` object invisibly.

------------------------------------------------------------------------

### Method `remove_filter_var()`

Remove an active filter variable of a dataset from the active filter
variables panel.

#### Usage

    TealAppDriver$remove_filter_var(dataset_name = NULL, var_name = NULL)

#### Arguments

- `dataset_name`:

  (character) The name of the dataset to remove the filter variable
  from. If `NULL`, all the filter variables will be removed.

- `var_name`:

  (character) The name of the variable to remove from the filter panel.
  If `NULL`, all the filter variables of the dataset will be removed.

#### Returns

The `TealAppDriver` object invisibly.

------------------------------------------------------------------------

### Method `set_active_filter_selection()`

Set the active filter values for a variable of a dataset in the active
filter variable panel.

#### Usage

    TealAppDriver$set_active_filter_selection(dataset_name, var_name, input, ...)

#### Arguments

- `dataset_name`:

  (character) The name of the dataset to set the filter value for.

- `var_name`:

  (character) The name of the variable to set the filter value for.

- `input`:

  The value to set the filter to.

- `...`:

  Additional arguments to be passed to
  `shinytest2::AppDriver$set_inputs`

#### Returns

The `TealAppDriver` object invisibly.

------------------------------------------------------------------------

### Method `get_attr()`

Extract `html` attribute (found by a `selector`).

#### Usage

    TealAppDriver$get_attr(selector, attribute)

#### Arguments

- `selector`:

  (`character(1)`) specifying the selector to be used to get the content
  of a specific node.

- `attribute`:

  (`character(1)`) name of an attribute to retrieve from a node
  specified by `selector`.

#### Returns

The `character` vector.

------------------------------------------------------------------------

### Method `get_html_rvest()`

Wrapper around `get_html` that passes the output directly to
[`rvest::read_html`](http://xml2.r-lib.org/reference/read_xml.md).

#### Usage

    TealAppDriver$get_html_rvest(selector)

#### Arguments

- `selector`:

  `(character(1))` passed to `get_html`.

#### Returns

An XML document. Wrapper around `get_url()` method that opens the app in
the browser.

------------------------------------------------------------------------

### Method `open_url()`

#### Usage

    TealAppDriver$open_url()

#### Returns

Nothing. Opens the underlying teal app in the browser.

------------------------------------------------------------------------

### Method `wait_for_active_module_value()`

Waits until a specified input, output, or export value. This function
serves as a wrapper around the `wait_for_value` method, providing a more
flexible interface for waiting on different types of values within the
active module namespace.

#### Usage

    TealAppDriver$wait_for_active_module_value(
      input = rlang::missing_arg(),
      output = rlang::missing_arg(),
      export = rlang::missing_arg(),
      ...
    )

#### Arguments

- `input, output, export`:

  A name of an input, output, or export value. Only one of these
  parameters may be used.

- `...`:

  Must be empty. Allows for parameter expansion. Parameter with
  additional value to passed in `wait_for_value`.
