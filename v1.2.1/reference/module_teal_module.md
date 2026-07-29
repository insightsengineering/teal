# Calls all `modules`

Modules create navigation bar with drop-down menu and tab content. Each
`teal_module` is called recursively according to the structure of
`modules` argument. This is a custom module which utilizes
shiny/Bootstrap `.nav` class. `modules` are called with an `id` derived
from `teal_module`'s label and labels of its ancestors (if any).

## Usage

``` r
ui_teal_module(id, modules)

srv_teal_module(
  id,
  data,
  modules,
  datasets = NULL,
  slices_global,
  reporter = teal.reporter::Reporter$new(),
  data_load_status = reactive("ok")
)

.teal_navbar_append(navbar, child)

.teal_navbar_insert_ui(
  ui,
  where = "afterBegin",
  session = getDefaultReactiveDomain()
)

.teal_navbar_menu(..., id = NULL, label = NULL, class = NULL, icon = NULL)

.ui_teal_module(id, modules, active_module_id)

# Default S3 method
.ui_teal_module(id, modules, active_module_id)

# S3 method for class 'teal_modules'
.ui_teal_module(id, modules, active_module_id)

# S3 method for class 'teal_module'
.ui_teal_module(id, modules, active_module_id)

.srv_teal_module(
  id,
  data,
  modules,
  datasets = NULL,
  slices_global,
  reporter = teal.reporter::Reporter$new(),
  data_load_status = reactive("ok"),
  active_module_id = reactive(TRUE)
)

# Default S3 method
.srv_teal_module(
  id,
  data,
  modules,
  datasets = NULL,
  slices_global,
  reporter = teal.reporter::Reporter$new(),
  data_load_status = reactive("ok"),
  active_module_id = reactive(TRUE)
)

# S3 method for class 'teal_modules'
.srv_teal_module(
  id,
  data,
  modules,
  datasets = NULL,
  slices_global,
  reporter = teal.reporter::Reporter$new(),
  data_load_status = reactive("ok"),
  active_module_id = reactive(TRUE)
)

# S3 method for class 'teal_module'
.srv_teal_module(
  id,
  data,
  modules,
  datasets = NULL,
  slices_global,
  reporter = teal.reporter::Reporter$new(),
  data_load_status = reactive("ok"),
  active_module_id = reactive(TRUE)
)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- modules:

  (`teal_modules`) `teal_modules` object. These are the specific output
  modules which will be displayed in the `teal` application. See
  [`modules()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  and
  [`module()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  for more details.

- data:

  (`reactive` returning `teal_data`)

- datasets:

  (`reactive` returning `FilteredData` or `NULL`) When `datasets` is
  passed from the parent module (`srv_teal`) then `dataset` is a
  singleton which implies the filter-panel to be "global". When `NULL`
  then filter-panel is "module-specific".

- slices_global:

  (`reactiveVal` returning `modules_teal_slices`) see
  [`module_filter_manager`](https://insightsengineering.github.io/teal/reference/module_filter_manager.md)

- reporter:

  (`Reporter`, singleton) Stores reporter-cards appended in the server
  of `teal_module`.

- data_load_status:

  (`reactive` returning `character(1)`) Determines action dependent on a
  data loading status:

  - `"ok"` when `teal_data` is returned from the data loading.

  - `"teal_data_module failed"` when
    [`teal_data_module()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
    didn't return `teal_data`. Disables tab buttons.

  - `"external failed"` when a `reactive` passed to `srv_teal(data)`
    didn't return `teal_data`. Hides the whole tab panel.

- active_module_id:

  (`reactive` returning `character(1)`) `id` of the currently active
  module. This helps to determine which module can listen to reactive
  events.

## Value

Output of currently active module.

- `srv_teal_module.teal_module` returns `reactiveVal` containing output
  of the called module.

- `srv_teal_module.teal_modules` returns output of modules in a list
  following the hierarchy of `modules`

## Details

### Functions

- `ui/srv_teal_module` - wrapper module which links drop-down buttons
  with modules panels. Here `input$active_module_id` is instantiated.

- `.ui/srv_teal_module` - recursive S3 method which calls each module

- `.teal_navbar_append` - wrapper for
  [`htmltools::tagAppendChild()`](https://rstudio.github.io/htmltools/reference/tagAppendChild.html)
  to add any element to navigation bar.

- `.teal_navbar_insert_ui` - wrapper for
  [`shiny::insertUI()`](https://rdrr.io/pkg/shiny/man/insertUI.html) to
  insert any element to navigation bar.

- `.teal_navbar_menu` - UI function to create a drop-down menu for
  navigation bar.

### Utilizing `.nav` class

No extra `javascript` or server functionality were introduced to have
navigation buttons toggle between tab panels. This works thanks to
`.nav` container which links `.nav-link` buttons `href = #<module id>`
attribute with `.tab-pane`'s `id = <module id>` (see
“.ui_teal_module.teal_module\`).

### Initialization and isolation of the `teal_module`(s)

Modules are initialized only when they are active. This speeds up app
initialization and on startup only the first module is activated and its
outputs are calculated. Only the active module is listening to reactive
events. This way, modules are isolated and only one can run at any given
time. This makes the app more efficient by reducing unnecessary
computations on server side.
