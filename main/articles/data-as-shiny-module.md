# Data as shiny Module

## Introduction

Proper functioning of any `teal` application requires the presence of a
`teal_data` object. Typically, a `teal_data` object created in the
global environment will be passed to the `data` argument in `init`. This
`teal_data` object should contain all elements necessary for successful
execution of the application’s modules.

In some scenarios, however, application developers may opt to postpone
some data operations until the application runtime. This can be done by
passing a special *`shiny` module* to the `data` argument. The
`teal_data_module` function is used to build such a module from the
following components:

- a UI function; accepts only one argument, `id`; defines user interface
  elements for the data module
- a server function: accepts only one argument, `id`; defines server
  logic for the data module, including data creation; must return a
  reactive expression containing a `teal_data` object

`teal` will run this module when the application starts and the
resulting `teal_data` object that will be used throughout all `teal`
(analytic) modules.

## Creating data in-app

One case for postponing data operations is datasets that are dynamic,
frequently updated. Such data cannot be created once and kept in the
global environment. Using `teal_data_module` enables creating a dataset
from scratch every time the user starts the application.

*See
[`?qenv`](https://insightsengineering.github.io/teal.code/latest-tag/reference/qenv.html)
for a detailed explanation of how to use the `within` method.*

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal`](https://insightsengineering.github.io/teal/)`)`` `` ``data_module`` ``<-`` `[`teal_data_module`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` ui ``=`` ``function``(``id``)`` ``tags``$``div``(``)``,`` `` server ``=`` ``function``(``id``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` ``data`` ``<-`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``)``,`` `` ``{`` `` ``dataset1`` ``<-`` ``iris`` `` ``dataset2`` ``<-`` ``mtcars`` `` ``}`` `` ``)`` `` ``data`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` `` ``app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` ``data_module``,`` `` modules ``=`` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

## Modification of data in-app

Another reason to postpone data operations is to involve the application
user in the preprocessing stage. An initial, constant form of the data
can be created in the global environment and then modified once the app
starts.

The following example illustrates how `teal_data_module` can be utilized
to subset data based on the user inputs:

`data`` ``<-`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``)``, ``{`` `` ``dataset1`` ``<-`` ``iris`` `` ``dataset2`` ``<-`` ``mtcars`` ``}``)`` `` ``data_module`` ``<-`` `[`teal_data_module`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` ``tags``$``div``(`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"species"``)``, ``"Select species to keep"``,`` `` choices ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``iris``$``Species``)``, multiple ``=`` ``TRUE`` `` ``)``,`` `` `[`actionButton`](https://rdrr.io/pkg/shiny/man/actionButton.html)`(``ns``(``"submit"``)``, ``"Submit"``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`eventReactive`](https://rdrr.io/pkg/shiny/man/observeEvent.html)`(``input``$``submit``, ``{`` `` ``data_modified`` ``<-`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` ``data``,`` `` ``dataset1`` ``<-`` `[`subset`](https://rdrr.io/r/base/subset.html)`(``dataset1``, ``Species`` `[`%in%`](https://rdrr.io/r/base/match.html)` ``selected``)``,`` `` selected ``=`` ``input``$``species`` `` ``)`` `` ``data_modified`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` ``app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` ``data_module``,`` `` modules ``=`` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` ``shiny``::`[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``ui ``=`` ``app``$``ui``, server ``=`` ``app``$``server``)`` ``}`

Note that running preprocessing code in a module as opposed to the
global environment will increase app loading times. It is recommended to
keep the constant code in the global environment and to move only the
dynamic parts to a data module.

###### WARNING

When using `teal_data_module` to modify a pre-existing `teal_data`
object, it is crucial that the server function and the data object are
defined in the same environment, otherwise the server function will not
be able to access the data object. This means server functions defined
in packages cannot be used.

### Extending existing `teal_data_modules`

The server logic of a `teal_data_module` can be modified before it is
used in an app, using the `within` function. This allows the `teal_data`
object that is created in the `teal_data_module` to be processed
further.

In the previous example, `data_module` takes a predefined `teal_data`
object and allows the app user to select a subset. The following example
modifies `data_module` so that new columns are added once the data is
retrieved.

`data_module_2`` ``<-`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` ``data_module``,`` `` ``{`` `` ``# Create new column with Ratio of Sepal.Width and Petal.Width`` `` ``dataset1``$``Ratio.Sepal.Petal.Width`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``dataset1``$``Sepal.Width`` ``/`` ``dataset1``$``Petal.Width``, digits ``=`` ``2L``)`` `` ``# Create new column that converts Miles per Galon to Liter per 100 Km`` `` ``dataset2``$``lp100km`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``dataset2``$``mpg`` ``*`` ``0.42514371``, digits ``=`` ``2L``)`` `` ``}`` ``)`` `` ``app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` ``data_module_2``,`` `` modules ``=`` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` ``shiny``::`[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``ui ``=`` ``app``$``ui``, server ``=`` ``app``$``server``)`` ``}`
