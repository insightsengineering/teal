# Transform Input Data

## Introduction

`teal` version `0.16` introduced a new, optional argument in
[`teal::module`](https://insightsengineering.github.io/teal/reference/teal_modules.md),
`transformators`. This argument accepts a `list` of
`teal_transform_module` objects, which are created using the
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
function.
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
takes `ui` and `server` arguments to create a `shiny` module that
encodes data transformations.

When transformators are passed to a module, `teal` will execute data
transformations when that module is loaded as well as whenever the
original data changes. The transformations are applied to the data
*before* it reaches the module.

The `ui` elements of the transform module will be added to the filter
panel, while the server function provides the data manipulation logic.

This vignette describes how to manage custom data transformations in
`teal` apps.

![Transforming
teal_data](images/teal-transform-module-transformators.svg)

In this vignette we will focus on using the `teal_transform_module` for
transforming the input data using the `transformators` argument in
[`teal::module`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
function.

## Creating a data transformation module

Let us initialize a simple `teal` app by providing `iris` and `mtcars`
as input datasets.

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal`](https://insightsengineering.github.io/teal/)`)`` `` ``data`` ``<-`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``)``, ``{`` `` ``iris`` ``<-`` ``iris`` `` ``mtcars`` ``<-`` ``mtcars`` ``}``)`` `` ``app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` ``data``,`` `` modules ``=`` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

### Single transformator

Now let us create a simple `teal_transform_module` that returns the
first `n` number of rows of `iris` based on user input.

We will achieve this by creating a UI function with a `numericInput` for
the user to specify the number of rows to be displayed. The server
function will take a reactive expression holding `data` as argument and
return a reactive expression holding transformed `data`.

*Note*: It is recommended to return
[`reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html) with
[`teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
in `server` code of a `teal_transform_module` as this is more robust for
maintaining the reactivity of Shiny. If you are planning on using
[`eventReactive()`](https://rdrr.io/pkg/shiny/man/observeEvent.html) in
the server, the event should include
[`data()`](https://rdrr.io/r/utils/data.html) *(example
`eventReactive(list(input$a, data()), {...})`)*. More in [this
discussion](https://github.com/insightsengineering/teal/issues/1303#issuecomment-2286239832).

`data`` ``<-`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``)``, ``{`` `` ``iris`` ``<-`` ``iris`` `` ``mtcars`` ``<-`` ``mtcars`` ``}``)`` `` ``transformator_iris`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Custom transformator for iris"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` ``tags``$``div``(`` `` `[`numericInput`](https://rdrr.io/pkg/shiny/man/numericInput.html)`(``ns``(``"n_rows"``)``, ``"Number of rows to display"``, value ``=`` ``6``, min ``=`` ``1``, max ``=`` ``150``, step ``=`` ``1``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``iris`` ``<-`` `[`head`](https://rdrr.io/r/utils/head.html)`(``iris``, ``num_rows``)``,`` `` num_rows ``=`` ``input``$``n_rows`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` ``app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` ``data``,`` `` modules ``=`` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``transformators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``transformator_iris``)``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

*Note*: The `server` function of a `teal_transform_module` must return a
reactive expression with a `teal_data` object. In order to maintain full
reactivity, we recommended using
[`reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html) over
[`eventReactive()`](https://rdrr.io/pkg/shiny/man/observeEvent.html). If
you do use
[`eventReactive()`](https://rdrr.io/pkg/shiny/man/observeEvent.html) or
[`bindEvent()`](https://rdrr.io/pkg/shiny/man/bindEvent.html), the
trigger event should include
[`data()`](https://rdrr.io/r/utils/data.html) (*e.g.*
`eventReactive(list(input$a, data()), {...})`). See [this
discussion](https://github.com/insightsengineering/teal/issues/1303#issuecomment-2286239832)
for a detailed explanation.

### Multiple transformators

`module(transformators)` accepts a list, so we can use multiple
`teal_transform_module`s at the same time.

#### Targeting multiple datasets

Let us add another transformation that creates a column with `rownames`
in `mtcars`. Note that this module does not have interactive UI
elements.

`data`` ``<-`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``)``, ``{`` `` ``iris`` ``<-`` ``iris`` `` ``mtcars`` ``<-`` ``mtcars`` ``}``)`` `` ``transformator_iris`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Custom transformator for iris"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` ``tags``$``div``(`` `` `[`numericInput`](https://rdrr.io/pkg/shiny/man/numericInput.html)`(``ns``(``"n_rows"``)``, ``"Number of rows to subset"``, value ``=`` ``6``, min ``=`` ``1``, max ``=`` ``150``, step ``=`` ``1``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``iris`` ``<-`` `[`head`](https://rdrr.io/r/utils/head.html)`(``iris``, ``num_rows``)``,`` `` num_rows ``=`` ``input``$``n_rows`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` ``transformator_mtcars`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Custom transformator for mtcars"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` ``tags``$``div``(`` `` ``"Adding rownames column to mtcars"`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``, ``{`` `` ``mtcars``$``rownames`` ``<-`` `[`rownames`](https://rdrr.io/r/base/colnames.html)`(``mtcars``)`` `` `[`rownames`](https://rdrr.io/r/base/colnames.html)`(``mtcars``)`` ``<-`` ``NULL`` `` ``}``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` ``my_transformators`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` ``transformator_iris``,`` `` ``transformator_mtcars`` ``)`` `` ``app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` ``data``,`` `` modules ``=`` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``transformators ``=`` ``my_transformators``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

#### Targeting one dataset

It is also possible to have multiple transformator modules act on one
dataset. In such cases, transformations will be executed in the same
order in which the transformator modules are passed to the module.

`data`` ``<-`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``)``, ``{`` `` ``iris`` ``<-`` ``iris`` `` ``mtcars`` ``<-`` ``mtcars`` ``}``)`` `` ``transformator_iris_scale`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Scaling transformator for iris"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`uiOutput`](https://rdrr.io/pkg/shiny/man/htmlOutput.html)`(``ns``(``"scaled_columns_container"``)``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` ``ns`` ``<-`` ``session``$``ns`` `` `` ``scalable_columns`` ``<-`` `[`names`](https://rdrr.io/r/base/names.html)`(`[`Filter`](https://rdrr.io/r/base/funprog.html)`(``is.numeric``, `[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``"iris"``]``]``)``)`` ``|>`` `[`isolate`](https://rdrr.io/pkg/shiny/man/isolate.html)`(``)`` `` `` ``output``$``scaled_columns_container`` ``<-`` `[`renderUI`](https://rdrr.io/pkg/shiny/man/renderUI.html)`(``{`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(`` `` inputId ``=`` ``ns``(``"scaled_columns"``)``,`` `` label ``=`` ``"Columns to scale"``,`` `` choices ``=`` ``scalable_columns``,`` `` selected ``=`` ``input``$``scaled_columns``,`` `` multiple ``=`` ``TRUE`` `` ``)`` `` ``}``)`` `` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``{`` `` ``iris``[``scaled_columns``]`` ``<-`` `[`scale`](https://rdrr.io/r/base/scale.html)`(``iris``[``scaled_columns``]``)`` `` ``}``,`` `` scaled_columns ``=`` ``input``$``scaled_columns`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` ``transformator_iris`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Custom transformator for iris"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` ``tags``$``div``(`` `` `[`numericInput`](https://rdrr.io/pkg/shiny/man/numericInput.html)`(``ns``(``"n_rows"``)``, ``"Number of rows to subset"``, value ``=`` ``6``, min ``=`` ``1``, max ``=`` ``150``, step ``=`` ``1``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``iris`` ``<-`` `[`head`](https://rdrr.io/r/utils/head.html)`(``iris``, ``num_rows``)``,`` `` num_rows ``=`` ``input``$``n_rows`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` ``transformator_mtcars`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Custom transformator for mtcars"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` ``tags``$``div``(`` `` ``"Adding rownames column to mtcars"`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``, ``{`` `` ``mtcars``$``rownames`` ``<-`` `[`rownames`](https://rdrr.io/r/base/colnames.html)`(``mtcars``)`` `` `[`rownames`](https://rdrr.io/r/base/colnames.html)`(``mtcars``)`` ``<-`` ``NULL`` `` ``}``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` ``my_transformators`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` ``transformator_iris``,`` `` ``transformator_iris_scale``,`` `` ``transformator_mtcars`` ``)`` `` ``app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` ``data``,`` `` modules ``=`` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``transformators ``=`` ``my_transformators``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

This approach provides greater flexibility in data preprocessing,
allowing transformations to be tailored to specific datasets for a
specific module.
