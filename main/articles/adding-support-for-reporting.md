# Adding Support for Reporting to Custom Modules

## Introduction

The `teal` package offers an integrated reporting feature utilizing the
`teal.reporter` package. For a comprehensive explanation of the
reporting functionality itself, please refer to the documentation
therein.

This article is *intended for module developers* and aims to provide
guidance on enhancing a custom `teal` module with an automatic reporting
feature. This enhancement enables users to incorporate snapshots of the
module outputs into a report which can then be reviewed in another
module automatically provided by `teal`. Thus the app user can interact
with the report.

The responsibilities of a module developer include:

- Choosing whether reporting of their module is needed.
- Specifying the outputs that constitute a snapshot of their module.

The entire life cycle of objects involved in creating the report and
configuring the module to preview the report is handled by `teal`.

## Custom module

Let us consider an example module, based on the example module from
`teal`:

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal`](https://insightsengineering.github.io/teal/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.reporter`](https://github.com/insightsengineering/teal.reporter)`)`` `` ``my_module`` ``<-`` ``function``(``label`` ``=`` ``"example teal module"``)`` ``{`` `` `[`module`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` label ``=`` ``label``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` ``checkmate``::`[`assert_class`](https://mllg.github.io/checkmate/reference/checkClass.html)`(`[`isolate`](https://rdrr.io/pkg/shiny/man/isolate.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)``, ``"teal_data"``)`` `` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``session``, ``"dataname"``, choices ``=`` `[`isolate`](https://rdrr.io/pkg/shiny/man/isolate.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)``)``)`` `` ``output``$``dataset`` ``<-`` `[`renderPrint`](https://rdrr.io/pkg/shiny/man/renderPrint.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``dataname``)`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``input``$``dataname``]``]`` `` ``}``)`` `` ``}``)`` `` ``}``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`sidebarLayout`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`` `` `[`sidebarPanel`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"dataname"``)``, ``"Choose a dataset"``, choices ``=`` ``NULL``)``)``,`` `` `[`mainPanel`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`[`verbatimTextOutput`](https://rdrr.io/pkg/shiny/man/textOutput.html)`(``ns``(``"dataset"``)``)``)`` `` ``)`` `` ``}`` `` ``)`` ``}`

Using `teal`, you can launch this example module with the following:

`app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``IRIS ``=`` ``iris``, MTCARS ``=`` ``mtcars``)``,`` `` modules ``=`` ``my_module``(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

## Add support for reporting

### Modify the declaration of the server function

First we need to prepare the code inside the module to be added to the
report. See below:

`my_module_with_card`` ``<-`` ``function``(``label`` ``=`` ``"example teal module"``)`` ``{`` `` `[`module`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` label ``=`` ``label``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``session``, ``"dataname"``, choices ``=`` `[`isolate`](https://rdrr.io/pkg/shiny/man/isolate.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)``)``)`` `` `` ``# Prepare the report:`` `` ``report`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``obj`` ``<-`` `[`data`](https://rdrr.io/r/utils/data.html)`(``)``)`` `` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``obj``)`` ``<-`` `` `[`c`](https://rdrr.io/r/base/c.html)`(`` `` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``"# Module with reporting"``)``,`` `` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``obj``)``,`` `` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``"## Module's code"``)`` `` ``)`` `` ``obj`` `` ``}``)`` `` `` ``# Add to the report the code of the module`` `` ``data_r`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``teal_data`` ``<-`` ``report``(``)``, ``input``$``dataname``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(``teal_data``, ``table``, table ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``input``$``dataname``)``)`` `` ``}``)`` `` `` ``output``$``dataset`` ``<-`` `[`renderPrint`](https://rdrr.io/pkg/shiny/man/renderPrint.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``teal_data`` ``<-`` ``data_r``(``)``)`` `` ``teal_data``[[``input``$``dataname``]``]`` `` ``}``)`` `` ``}``)`` `` ``}``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`sidebarLayout`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`` `` `[`sidebarPanel`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"dataname"``)``, ``"Choose a dataset"``, choices ``=`` ``NULL``)``)``,`` `` `[`mainPanel`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`[`verbatimTextOutput`](https://rdrr.io/pkg/shiny/man/textOutput.html)`(``ns``(``"dataset"``)``)``)`` `` ``)`` `` ``}`` `` ``)`` ``}`

With these modifications, the module is now ready to be launched with
`teal`:

`app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``IRIS ``=`` ``iris``, MTCARS ``=`` ``mtcars``)``,`` `` modules ``=`` ``my_module_with_card``(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

The output hasn’t changed (yet). The final step is to have the server
return the reporter object, enabling the module to be reported.

### Return the reporter object

`my_module_with_reporting`` ``<-`` ``function``(``label`` ``=`` ``"example teal module"``)`` ``{`` `` `[`module`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` label ``=`` ``label``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``session``, ``"dataname"``, choices ``=`` `[`isolate`](https://rdrr.io/pkg/shiny/man/isolate.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)``)``)`` `` `` ``# Prepare the report:`` `` ``report`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``obj`` ``<-`` `[`data`](https://rdrr.io/r/utils/data.html)`(``)``)`` `` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``obj``)`` ``<-`` `` `[`c`](https://rdrr.io/r/base/c.html)`(`` `` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``"# Module with reporting"``)``,`` `` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``obj``)``,`` `` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``"## Module's code"``)`` `` ``)`` `` ``obj`` `` ``}``)`` `` `` ``# Add to the report the code of the module`` `` ``data_r`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``rtd`` ``<-`` ``report``(``)``, ``input``$``dataname``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(``rtd``, ``table``, table ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``input``$``dataname``)``)`` `` ``}``)`` `` `` ``output``$``dataset`` ``<-`` `[`renderPrint`](https://rdrr.io/pkg/shiny/man/renderPrint.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``dr`` ``<-`` ``data_r``(``)``)`` `` ``dr``[[``input``$``dataname``]``]`` `` ``}``)`` `` `` ``# the reactive teal_report is returned by the module`` `` ``data_r`` `` ``}``)`` `` ``}``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`sidebarLayout`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`` `` `[`sidebarPanel`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"dataname"``)``, ``"Choose a dataset"``, choices ``=`` ``NULL``)``)``,`` `` `[`mainPanel`](https://rdrr.io/pkg/shiny/man/sidebarLayout.html)`(`[`verbatimTextOutput`](https://rdrr.io/pkg/shiny/man/textOutput.html)`(``ns``(``"dataset"``)``)``)`` `` ``)`` `` ``}`` `` ``)`` ``}`

With these modifications, the module is now ready to be launched with
`teal`:

`app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``IRIS ``=`` ``iris``, MTCARS ``=`` ``mtcars``)``,`` `` modules ``=`` ``my_module_with_reporting``(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

The key step is to return a reactive `teal_report` object containing
everything. This informs `teal` that the module provides a `reporter`,
and teal will add a button `+ Add to Report` to add the modules’ content
to the report. The user can now add a card to the report with the
current state of the module. The report can be seen after clicking
`Preview report` under the `Report` button.

### Add content to the card

The user can modify the text of a card or add new text with the button
`+ Add text block` present at the bottom of the card. Text can also be
added inside the module by appending a
[`teal_card()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)
to the card of the report.

As the module writer, you can also add any other content to the report
you’d like: titles, text.

### Add non-text content to the card

`teal.reporter` supports the addition of tables, charts, and more. For
more information, explore the API of
[`teal_report()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_report.html)
to learn about the supported content types.

## Removing support for displaying reproducible code

If your module supports a report but you want to disable the button that
allows to display the module’s reproducible code (“Show R code”), use
[`disable_src()`](https://insightsengineering.github.io/teal/reference/disable_src.md):

`app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``IRIS ``=`` ``iris``, MTCARS ``=`` ``mtcars``)``,`` `` modules ``=`` ``my_module_with_reporting``(``)`` ``|>`` `[`disable_src`](https://insightsengineering.github.io/teal/reference/disable_src.md)`(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

You can use
[`disable_src()`](https://insightsengineering.github.io/teal/reference/disable_src.md)
on multiple modules at the same time and nested modules too. For example
on:

`app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``IRIS ``=`` ``iris``, MTCARS ``=`` ``mtcars``)``,`` `` modules ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`` `` `[`modules`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` label ``=`` ``"One nested module disabled"``,`` `` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``label ``=`` ``"Module 1"``)``,`` `` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``label ``=`` ``"Module 2"``)``,`` `` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``label ``=`` ``"Module 3"``)`` ``|>`` `[`disable_src`](https://insightsengineering.github.io/teal/reference/disable_src.md)`(``)`` `` ``)``,`` `` `[`modules`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` label ``=`` ``"Nested modules without source"``,`` `` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``label ``=`` ``"Module 1"``)``,`` `` `[`example_module`](https://insightsengineering.github.io/teal/reference/example_module.md)`(``label ``=`` ``"Module 2"``)`` `` ``)`` ``|>`` `[`disable_src`](https://insightsengineering.github.io/teal/reference/disable_src.md)`(``)`` `` ``)`` ``)`

## Removing reporting

If a module has the reporter functionality the teal app developer can
disable it with
[`disable_report()`](https://insightsengineering.github.io/teal/reference/disable_report.md).

`app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``IRIS ``=`` ``iris``, MTCARS ``=`` ``mtcars``)``,`` `` modules ``=`` ``my_module_with_reporting``(``)`` ``|>`` `[`disable_report`](https://insightsengineering.github.io/teal/reference/disable_report.md)`(``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

To remove reporting from the whole application, set `reporter = NULL` in
[`init()`](https://insightsengineering.github.io/teal/reference/init.md).
This will completely disable all the reporter related buttons on the
application:

![Screenshot of an app without the reporter
options](images/custom_module_without_reporter.png)

Screenshot of an app without the reporter options

## Customizing the reporter

A template can be set for the report; when a template is used each card
added to the report contains the template’s default content.
Additionally, cards can be added to the report before the application
starts.

`reporter`` ``<-`` `[`Reporter`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/Reporter.html)`$``new``(``)`` ``template_fun`` ``<-`` ``function``(``document``)`` ``{`` `` ``header`` ``<-`` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``"Here comes header text."``)`` `` ``logo_url`` ``<-`` ``"https://raw.githubusercontent.com/insightsengineering/teal/refs/heads/main/man/figures/logo.svg"`` `` ``footer`` ``<-`` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(`[`paste0`](https://rdrr.io/r/base/paste.html)`(`` `` ``"Here comes footer text. Report generated with teal ![logo](%s 'teal logo'){height=70}"``,`` `` ``logo_url`` `` ``)``)`` `` `[`c`](https://rdrr.io/r/base/c.html)`(``header``, ``document``, ``footer``)`` ``}`` ``reporter``$``set_template``(``template_fun``)`` `` ``card1`` ``<-`` `[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``"## Header 2 text"``, ``"Regular text"``)`` `[`metadata`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/metadata.html)`(``card1``, ``"title"``)`` ``<-`` ``"Welcome card"`` ``reporter``$``append_cards``(``card1``)`

Once the reporter is created we can use in the teal application.
