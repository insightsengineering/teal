# Modifying a teal Application With R Options

## Motivation

Some `R` packages use `options` to modify their runtime behavior. They
usually specify sensible default values for internal function arguments
or determine responses to users actions. For example, `testthat` uses an
option `testthat.progress.max_fails` to define a default number of
failed expectations before the testing functions terminate execution.
While some of these adjustable values can be exposed as function
parameters, some are confined to an option. This vignette details the
options available in the package `teal` and its supporting packages
`teal.logger`, `teal.widgets`, and `teal.slice`.

## Setting an option

At any time during an interactive session, you can change an option
using:

``` r

options(option_to_set = "value")
```

A way to change options for only the execution of a specific block of
code is with the `withr` package like so:

``` r

withr::with_options(list(digits = 3), print(pi))
```

    ## [1] 3.14

After the line above is run the option, `digits`, will go back to its
value before the line was run.

The function `getOption` allows to inspect the value of an option:

``` r

getOption("option_to_set")
```

    ## [1] "value"

Once set, the value of an option persists during a session, but it
returns to the default value in a new session. Make sure to change the
options after all the `teal`-related packages are loaded because some of
them initialize the options themselves and will overwrite your custom
values.

## Options used in a `teal` application

#### `teal.bs_theme` (`bslib::bs_theme` object)

This option controls the bootstrap theme and version used in `teal`
apps. Achieve better UX with the customized UI of an app. Please see the
[vignette on Bootstrap
themes](https://insightsengineering.github.io/teal/articles/bootstrap-themes-in-teal.md)
to read more about the functionality.

Default:
[`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)

#### `teal.load_nest_code` (`character`)

The value of this option is appended to the top of the code rendered
when using the `Show R Code` modal button.

Default: `"# Add any code to install/load your NEST environment here"`.

#### `teal.threshold_slider_vs_checkboxgroup` (`numeric`)

This is the threshold that determines if a variable is treated as a
factor in the filter panel. If the number of unique values of a variable
is less than this threshold the variable will be treated as a factor
instead of its original class. As an example, imagine
`teal.threshold_slider_vs_checkboxgroup` equals to 2. Then a numeric
variable `c(1, 1, 1)`, which has only one unique value, is treated as a
factor in the filter panel (and in the filter panel only!). The filter
panel creates a checkbox widget to filter values from this variable, as
it would for a factor variable, instead of the usual numeric range
selector.

Default: 5.

#### `teal.basic_table_args` (`basic_table_args` object)

This specifies the list of arguments passed to every call to
[`rtables::basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)
made in a `teal` application. This can be used to format `rtables`
without making any changes to the application code. See the
documentation of
[`teal.widgets::basic_table_args`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
for more information.

Default:
[`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html).

#### `teal.ggplot2_args` (`ggplot2_args` object)

This option allows modifying labels and themes of all `ggplot2` plots in
a `teal` application. See the documentation of
[`teal.widgets::ggplot2_args`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
for more information.

Default:
[`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html).

#### `teal.plot_dpi` (integer value 24 or larger)

This option controls the dots per inch of the graphs rendered and
downloaded when using the module `plot_with_settings` from the
`teal.widgets` package.

Default: 72

#### `teal.log_layout` (`character`)

This defines the layout of a log message used in a `teal` application.
`teal` uses this layout to format the emitted log messages. Read the
documentation of
[`teal.logger::register_logger`](https://insightsengineering.github.io/teal.logger/latest-tag/reference/register_logger.html)
for more information. This must be set before loading any `teal`
package(s). In case the option is set after attaching the packages,
please re-attach the packages again to use it correctly.

Default:
`"[{level}] {format(time, \"%Y-%m-%d %H:%M:%OS4\")} pid:{pid} token:[{token}] {ans} {msg}"`.

Note that this layout is formatted by the `glue` package.

#### `teal.log_level` (`character`)

This is the logging level threshold used in a `teal` application. A
`teal` application will not emit logs below this level. See the
documentation of
[`logger::TRACE`](https://daroczig.github.io/logger/reference/log_levels.html)
for all possible values of logging threshold and more information on
what it does. This must be set before loading any `teal` package(s). In
case the option is set after attaching the packages, please re-attach
the packages again to use it correctly.

Default: `"INFO"`.

Note that there are two levels considered less severe than `"INFO"`:
`"DEBUG"` and `"TRACE"`. In order to see the log messages for these two
levels as well, change the log level from the default to `"TRACE"`, the
least severe log level.

#### `teal.show_js_log` (`logical`)

This indicates whether to print the `JavaScript` console logs to the `R`
console. If set to `TRUE`, the logs will be printed; otherwise, they
won’t.

Default: `FALSE`.

#### `teal.lockfile.mode` (`character`)

This enables to compute `renv` lockfile and shows a button to
`"download lockfile"` in the footer.

Values:

- `"auto"` - auto detect whether to compute `lockfile`
- `"enabled"` - compute `lockfile` and show `"download lockfile"` in the
  footer
- `"disabled"` - do not compute `lockfile` and do not show
  `"download lockfile"` in the footer

Default: `"auto"`.

To read more about lockfile usage creation check
[`?teal::module_teal_lockfile`](https://insightsengineering.github.io/teal/reference/module_teal_lockfile.md).

#### `teal.sidebar.position` (`character`)

This sets the position of the sidebar in the teal app. Possible values
are `"left"` and `"right"`.

Default: `left`

#### `teal.sidebar.width` (`numeric`)

This sets the sidebar width in pixels in the teal app.

Default: `250`

#### `teal.reporter.nav_buttons` (`character`)

This option controls which of the four reporter navigation buttons
(Preview Report, Download Report, Load Report, and Reset Report) should
be displayed.

Default: `c("preview", "download", "load", "reset")`.

#### `teal.reporter.rmd_output` (`character`)

This option allows customizing the R Markdown output types when a report
is downloaded. It should be a subset of “html_document”, “pdf_document”,
“powerpoint_presentation”, “word_document”.

Default:

``` r

c(
  "html" = "html_document", "pdf" = "pdf_document",
  "powerpoint" = "powerpoint_presentation",
  "word" = "word_document"
)
```

#### `teal.reporter.rmd_yaml_args` (`character`)

It allows customizing the R Markdown YAML arguments for the report. It
should be a named vector and the names should be a subset of the
supported YAML args: “author”, “title”, “date”, “output”, “toc”.

Default:

``` r

list(
  author = "NEST",
  title = "Report",
  date = as.character(Sys.Date()),
  output = "html_document",
  toc = FALSE
)
```

#### `teal.reporter.global_knitr` (`list`)

It allows customizing the global `knitr` parameters which are passed to
`knitr::opts_chunk$set` for customizing the rendering process.

Default:

``` r

list(
  echo = TRUE,
  tidy.opts = list(width.cutoff = 60),
  tidy = requireNamespace("formatR", quietly = TRUE)
)
```

#### `teal.reporter.max_request_size` (`numeric`)

Allows to control the maximum size of a
[`shiny::fileInput`](https://rdrr.io/pkg/shiny/man/fileInput.html).
Meant for large report file uploads. Default: `10 * 1024^2` which stands
for 10MB. \### `teal.show_src` (`logical`)

It allows customizing the visibility of the “Show R Code” button in the
module title bar.

Default: `TRUE`.

## Deprecated options

#### `teal_logging`

Deprecated in favor of using the `teal.logger` package for logging.

#### `teal_show_js_log`

Deprecated in favor of `teal.show_js_log` (see above).
