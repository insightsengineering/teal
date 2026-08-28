# Teal as a Shiny Module

## Introduction

A `shiny` developer can embed a `teal` application into their own
`shiny` app by using `shiny` module components of `teal`:
[`ui_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md)
and
[`srv_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md).
This approach differs from using
[`init()`](https://insightsengineering.github.io/teal/reference/init.md)
and offers greater flexibility. While
[`init()`](https://insightsengineering.github.io/teal/reference/init.md)
includes a session info footer automatically, when using `teal` as a
`shiny` module you can optionally add it manually with
[`ui_session_info()`](https://insightsengineering.github.io/teal/reference/module_session_info.md)
and
[`srv_session_info()`](https://insightsengineering.github.io/teal/reference/module_session_info.md).
Using `teal` as a `shiny` module offers several advantages:

- Embedding one or more `teal` applications within a larger `shiny` app
- Creating `teal` applications with dynamically generated components
  (initial data, modules, filters)

## Example

The following example demonstrates embedding `teal` as a `shiny` module
within a larger `shiny` application. Users can select dataset names
which are passed to the embedded `teal` component. On the server side,
[`srv_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md)
is called with a reactive `teal_data` object passed from the parent
app’s server.

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal`](https://insightsengineering.github.io/teal/)`)`` `` ``data`` ``<-`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``)`` ``|>`` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(``{`` `` ``iris`` ``<-`` ``iris`` `` ``mtcars`` ``<-`` ``mtcars`` `` ``df`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``a ``=`` ``1``:``10``, b ``=`` ``letters``[``1``:``10``]``)`` ``}``)`` `` ``mods`` ``<-`` `[`modules`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``"mod1"``)``,`` `` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``"mod2"``)`` ``)`` `` ``ui_app`` ``<-`` `[`fluidPage`](https://rdrr.io/pkg/shiny/man/fluidPage.html)`(`` `` title ``=`` ``"Your app with teal as a module"``,`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``"datasets"``, ``"Select datasets"``, choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"iris"``, ``"mtcars"``, ``"df"``)``, selected ``=`` ``"iris"``, multiple ``=`` ``TRUE``)``,`` `` `[`ui_teal`](https://insightsengineering.github.io/teal/reference/module_teal.md)`(``"teal"``, ``mods``)``,`` `` `[`ui_session_info`](https://insightsengineering.github.io/teal/reference/module_session_info.md)`(``"session_info"``)`` ``)`` `` ``srv_app`` ``<-`` ``function``(``input``, ``output``, ``session``)`` ``{`` `` ``data_subset`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``data``[``input``$``datasets``]``)`` `` `[`srv_teal`](https://insightsengineering.github.io/teal/reference/module_teal.md)`(``"teal"``, data ``=`` ``data_subset``, modules ``=`` ``mods``)`` `` `[`srv_session_info`](https://insightsengineering.github.io/teal/reference/module_session_info.md)`(``"session_info"``)`` ``}`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``ui_app``, ``srv_app``)`` ``}`
