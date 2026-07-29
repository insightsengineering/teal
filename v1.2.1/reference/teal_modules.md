# Create `teal_module` and `teal_modules` objects

Create a nested tab structure to embed modules in a `teal` application.

## Usage

``` r
module(
  label = "module",
  server = function(id, data, ...) moduleServer(id, function(input, output, session)
    NULL),
  ui = function(id, ...) tags$p(paste0("This module has no UI (id: ", id, " )")),
  filters,
  datanames = "all",
  server_args = NULL,
  ui_args = NULL,
  transformators = list()
)

modules(..., label = character(0))

# S3 method for class 'teal_module'
format(
  x,
  is_last = FALSE,
  parent_prefix = "",
  what = c("datasets", "properties", "arguments", "transformators"),
  ...
)

# S3 method for class 'teal_modules'
format(x, is_root = TRUE, is_last = FALSE, parent_prefix = "", ...)

# S3 method for class 'teal_module'
print(x, ...)

# S3 method for class 'teal_modules'
print(x, ...)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- server:

  (`function`) `shiny` module with following arguments:

  - `id` - `teal` will set proper `shiny` namespace for this module (see
    [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)).

  - `input`, `output`, `session` - (optional; not recommended) When
    provided, then
    [`shiny::callModule()`](https://rdrr.io/pkg/shiny/man/callModule.html)
    will be used to call a module. From `shiny` 1.5.0, the recommended
    way is to use
    [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
    instead which doesn't require these arguments.

  - `data` (optional) If the server function includes a `data` argument,
    it will receive a reactive expression containing the `teal_data`
    object.

  - `datasets` (optional) When provided, the module will be called with
    `FilteredData` object as the value of this argument. (See
    [`teal.slice::FilteredData`](https://insightsengineering.github.io/teal.slice/latest-tag/reference/FilteredData.html)).

  - `reporter` (optional) When provided, the module will be called with
    `Reporter` object as the value of this argument. (See
    [`teal.reporter::Reporter`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/Reporter.html)).

  - `filter_panel_api` (optional) When provided, the module will be
    called with `FilterPanelAPI` object as the value of this argument.
    (See
    [`teal.slice::FilterPanelAPI`](https://insightsengineering.github.io/teal.slice/latest-tag/reference/FilterPanelAPI.html)).

  - `...` (optional) When provided, `server_args` elements will be
    passed to the module named argument or to the `...`.

- ui:

  (`function`) `shiny` UI module function with following arguments:

  - `id` - `teal` will set proper `shiny` namespace for this module.

  - `...` (optional) When provided, `ui_args` elements will be passed to
    the module named argument or to the `...`.

- filters:

  (`character`) Deprecated. Use `datanames` instead.

- datanames:

  (`character`) Names of the datasets relevant to the item. There are 2
  reserved values that have specific behaviors:

  - The keyword `"all"` includes all datasets available in the data
    passed to the teal application.

  - `NULL` hides the sidebar panel completely.

  - If `transformators` are specified, their `datanames` are
    automatically added to this `datanames` argument.

- server_args:

  (named `list`) with additional arguments passed on to the server
  function.

- ui_args:

  (named `list`) with additional arguments passed on to the UI function.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/articles/transform-input-data.md).

- ...:

  - For `modules()`: (`teal_module` or `teal_modules`) Objects to wrap
    into a tab.

  - For [`format()`](https://rdrr.io/r/base/format.html) and
    [`print()`](https://rdrr.io/r/base/print.html): Arguments passed to
    other methods.

- x:

  (`teal_module` or `teal_modules`) Object to format/print.

- is_last:

  (`logical(1)`) Whether this is the last item in its parent's children
  list. Affects the tree branch character used (L- vs \|-)

- parent_prefix:

  (`character(1)`) The prefix inherited from parent nodes, used to
  maintain the tree structure in nested levels

- what:

  (`character`) Specifies which metadata to display. Possible values:
  "datasets", "properties", "arguments", "transformators"

- is_root:

  (`logical(1)`) Whether this is the root node of the tree. Only used in
  format.teal_modules(). Determines whether to show "TEAL ROOT" header

## Value

`module()` returns an object of class `teal_module`.

`modules()` returns an object of class `teal_modules`.

## Details

`module()` creates an instance of a `teal_module` that can be placed in
a `teal` application. `modules()` shapes the structure of a the
application by organizing `teal_module` within the navigation panel. It
wraps `teal_module` and `teal_modules` objects in a `teal_modules`
object, which results in a nested structure corresponding to the nested
tabs in the final application.

Note that for `modules()` `label` comes after `...`, so it must be
passed as a named argument, otherwise it will be captured by `...`.

The labels `"global_filters"` and `"Report previewer"` are reserved
because they are used by the `mapping` argument of
[`teal_slices()`](https://insightsengineering.github.io/teal/reference/teal_slices.md)
and the report previewer module
[`reporter_previewer_module()`](https://insightsengineering.github.io/teal/reference/reporter_previewer_module.md),
respectively.

## Restricting datasets used by `teal_module`:

The `datanames` argument controls which datasets are used by the
module's server. These datasets, passed via server's `data` argument,
are the only ones shown in the module's tab.

When `datanames` is set to `"all"`, all datasets in the data object are
treated as relevant. However, this may include unnecessary datasets,
such as:

- Proxy variables for column modifications

- Temporary datasets used to create final ones

- Connection objects

Datasets which name is prefixed in `teal_data` by the dot (`.`) are not
displayed in the `teal` application. Please see the *"Hidden datasets"*
section in \`vignette("including-data-in-teal-applications").

## `datanames` with `transformators`

When transformators are specified, their `datanames` are added to the
module's `datanames`, which changes the behavior as follows:

- If `module(datanames)` is `NULL` and the `transformators` have defined
  `datanames`, the sidebar will appear showing the `transformators`'
  datasets, instead of being hidden.

- If `module(datanames)` is set to specific values and any
  `transformator` has `datanames = "all"`, the module may receive extra
  datasets that could be unnecessary

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIF0mLDgGcAFkNb8IAmEQAmK6nAD6ARjmKXd092AWlpaih6OGppAF5pPhRpQI84RNxQ6SsRKUY45TUNLVpXXGlXKFIoHRBMsJTPAGUckRCIMI7pEoz2zuS3VPzVdXFiiFQVUjKiSYmprLgrK00IGrq+mdI5gBIKqv9pRgpXEQARSqhdSOC9qG1gYETaRlorRIBdd8c+6QBfde+fx6YRUtCGhVGEHYJTWvTCECsBwAco1oa5AWEqgBzAAyr1I7FuV3ongA8rNJuwESEwLdEjwGZlfsDyhdoPBEfFEtxqIkBI5nANPF4AEwHBpwNphCJRGJclAQIikGwifpBNJ4TLZRi5cEjFZo2GdCXNHWtdZhboWtWDeLDIpQoRzaYU+bZJYrI0-aSbHbkAAepAORwgJ0YujggZpAEFFcrVQBZIUajEdf5w6SA5mZUF6h2G6S1DMI5GomHrbF4qwEgOkclbSnUxK1+mM3rZ3q3dmLfJIgw4nH80wQCWIhQ2zxWKXhSLRfKJUfpTKj6fS2dyhKQRbkVwTxZLjMS3yZHgso8iodOaDoA5CWgEzK3fJcaheW7Ql6crqf0-L5NfxdVmHWglGkaFhFEcQpG0L1bHsaN0HYNBUG2UEymQ7ZtVyRxfjAX53iAA)

## Examples

``` r
library(shiny)

module_1 <- module(
  label = "a module",
  server = function(id, data) {
    moduleServer(
      id,
      module = function(input, output, session) {
        output$data <- renderDataTable(data()[["iris"]])
      }
    )
  },
  ui = function(id) {
    ns <- NS(id)
    tagList(dataTableOutput(ns("data")))
  },
  datanames = "all"
)

module_2 <- module(
  label = "another module",
  server = function(id) {
    moduleServer(
      id,
      module = function(input, output, session) {
        output$text <- renderText("Another Module")
      }
    )
  },
  ui = function(id) {
    ns <- NS(id)
    tagList(textOutput(ns("text")))
  },
  datanames = NULL
)

modules <- modules(
  label = "modules",
  modules(
    label = "nested modules",
    module_1
  ),
  module_2
)

app <- init(
  data = teal_data(iris = iris),
  modules = modules
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
mod <- module(
  label = "My Custom Module",
  server = function(id, data, ...) {},
  ui = function(id, ...) {},
  datanames = c("ADSL", "ADTTE"),
  transformators = list(),
  ui_args = list(a = 1, b = "b"),
  server_args = list(x = 5, y = list(p = 1))
)
cat(format(mod))
#> |- My Custom Module
#> |  |- Datasets: ADSL, ADTTE
#> |  |- Properties:
#> |  |  |- Bookmarkable: FALSE
#> |  |  L- Reportable: FALSE
#> |  L- Arguments:
#> |     |- a (numeric)
#> |     |- b (character)
#> |     |- x (numeric)
#> |     L- y (list)
custom_module <- function(
  label = "label", ui_args = NULL, server_args = NULL,
  datanames = "all", transformators = list(), bk = FALSE
) {
  ans <- module(
    label,
    server = function(id, data, ...) {},
    ui = function(id, ...) {},
    datanames = datanames,
    transformators = transformators,
    ui_args = ui_args,
    server_args = server_args
  )
  attr(ans, "teal_bookmarkable") <- bk
  ans
}

dummy_transformator <- teal_transform_module(
  label = "Dummy Transform",
  ui = function(id) div("(does nothing)"),
  server = function(id, data) {
    moduleServer(id, function(input, output, session) data)
  }
)

plot_transformator <- teal_transform_module(
  label = "Plot Settings",
  ui = function(id) div("(does nothing)"),
  server = function(id, data) {
    moduleServer(id, function(input, output, session) data)
  }
)

static_decorator <- teal_transform_module(
  label = "Static decorator",
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        req(data())
        within(data(), {
          plot <- plot +
            ggtitle("This is title") +
            xlab("x axis")
        })
      })
    })
  }
)

complete_modules <- modules(
  custom_module(
    label = "Data Overview",
    datanames = c("ADSL", "ADAE", "ADVS"),
    ui_args = list(
      view_type = "table",
      page_size = 10,
      filters = c("ARM", "SEX", "RACE"),
      decorators = list(static_decorator)
    ),
    server_args = list(
      cache = TRUE,
      debounce = 1000,
      decorators = list(static_decorator)
    ),
    transformators = list(dummy_transformator),
    bk = TRUE
  ),
  modules(
    label = "Nested 1",
    custom_module(
      label = "Interactive Plots",
      datanames = c("ADSL", "ADVS"),
      ui_args = list(
        plot_type = c("scatter", "box", "line"),
        height = 600,
        width = 800,
        color_scheme = "viridis"
      ),
      server_args = list(
        render_type = "svg",
        cache_plots = TRUE
      ),
      transformators = list(dummy_transformator, plot_transformator),
      bk = TRUE
    ),
    modules(
      label = "Nested 2",
      custom_module(
        label = "Summary Statistics",
        datanames = "ADSL",
        ui_args = list(
          stats = c("mean", "median", "sd", "range"),
          grouping = c("ARM", "SEX")
        )
      ),
      modules(
        label = "Labeled nested modules",
        custom_module(
          label = "Subgroup Analysis",
          datanames = c("ADSL", "ADAE"),
          ui_args = list(
            subgroups = c("AGE", "SEX", "RACE"),
            analysis_type = "stratified"
          ),
          bk = TRUE
        )
      ),
      modules(custom_module(label = "Subgroup Analysis in non-labled modules"))
    )
  ),
  custom_module("Non-nested module")
)

cat(format(complete_modules))
#> TEAL ROOT
#>   |- Data Overview
#>   |  |- Datasets: ADSL, ADAE, ADVS, all
#>   |  |- Properties:
#>   |  |  |- Bookmarkable: TRUE
#>   |  |  L- Reportable: FALSE
#>   |  |- Arguments:
#>   |  |  |- view_type (character)
#>   |  |  |- page_size (numeric)
#>   |  |  |- filters (character)
#>   |  |  |- cache (logical)
#>   |  |  |- debounce (numeric)
#>   |  |  L- Decorators:
#>   |  |     L- Static decorator
#>   |  L- Transformators:
#>   |     L- Dummy Transform
#>   |- Nested 1
#>   |  |- Interactive Plots
#>   |  |  |- Datasets: ADSL, ADVS, all
#>   |  |  |- Properties:
#>   |  |  |  |- Bookmarkable: TRUE
#>   |  |  |  L- Reportable: FALSE
#>   |  |  |- Arguments:
#>   |  |  |  |- plot_type (character)
#>   |  |  |  |- height (numeric)
#>   |  |  |  |- width (numeric)
#>   |  |  |  |- color_scheme (character)
#>   |  |  |  |- render_type (character)
#>   |  |  |  L- cache_plots (logical)
#>   |  |  L- Transformators:
#>   |  |     |- Dummy Transform
#>   |  |     L- Plot Settings
#>   |  L- Nested 2
#>   |     |- Summary Statistics
#>   |     |  |- Datasets: ADSL
#>   |     |  |- Properties:
#>   |     |  |  |- Bookmarkable: FALSE
#>   |     |  |  L- Reportable: FALSE
#>   |     |  L- Arguments:
#>   |     |     |- stats (character)
#>   |     |     L- grouping (character)
#>   |     |- Labeled nested modules
#>   |     |  L- Subgroup Analysis
#>   |     |     |- Datasets: ADSL, ADAE
#>   |     |     |- Properties:
#>   |     |     |  |- Bookmarkable: TRUE
#>   |     |     |  L- Reportable: FALSE
#>   |     |     L- Arguments:
#>   |     |        |- subgroups (character)
#>   |     |        L- analysis_type (character)
#>   |     L- 
#>   |        L- Subgroup Analysis in non-labled modules
#>   |           |- Datasets: all
#>   |           L- Properties:
#>   |              |- Bookmarkable: FALSE
#>   |              L- Reportable: FALSE
#>   L- Non-nested module
#>      |- Datasets: all
#>      L- Properties:
#>         |- Bookmarkable: FALSE
#>         L- Reportable: FALSE
cat(format(complete_modules, what = c("ui_args", "server_args", "transformators")))
#> TEAL ROOT
#>   |- Data Overview
#>   |  L- Transformators:
#>   |     L- Dummy Transform
#>   |- Nested 1
#>   |  |- Interactive Plots
#>   |  |  L- Transformators:
#>   |  |     |- Dummy Transform
#>   |  |     L- Plot Settings
#>   |  L- Nested 2
#>   |     |- Summary Statistics
#>   |     |- Labeled nested modules
#>   |     |  L- Subgroup Analysis
#>   |     L- 
#>   |        L- Subgroup Analysis in non-labled modules
#>   L- Non-nested module
cat(format(complete_modules, what = c("decorators", "transformators")))
#> TEAL ROOT
#>   |- Data Overview
#>   |  L- Transformators:
#>   |     L- Dummy Transform
#>   |- Nested 1
#>   |  |- Interactive Plots
#>   |  |  L- Transformators:
#>   |  |     |- Dummy Transform
#>   |  |     L- Plot Settings
#>   |  L- Nested 2
#>   |     |- Summary Statistics
#>   |     |- Labeled nested modules
#>   |     |  L- Subgroup Analysis
#>   |     L- 
#>   |        L- Subgroup Analysis in non-labled modules
#>   L- Non-nested module
```
