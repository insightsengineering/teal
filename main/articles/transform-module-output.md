# Transform Module Output

## Introduction

The outputs produced by `teal` modules, like graphs or tables, are
created by the module developer and look a certain way. It is hard to
design an output that will satisfy every possible user, so the form of
the output should be considered a default value that can be customized.
In [Transform Input
Data](https://insightsengineering.github.io/teal/articles/transform-input-data.md)
we described how `teal_module`’s input data can be modified using
`teal_transform_module`. Here we present how to utilize
`teal_transform_module` to modify an output created by a `teal_module`,
enabling you to tailor outputs to your specific requirements without
rewriting the original module code.

![Transforming teal_data](images/teal-transform-module-decorators.svg)

## How to Transform outputs?

Custom transformations for the output objects can be created with
[`teal_transform_module()`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)
and thus they are `shiny` modules. They are passed to `teal_module`
constructors as arguments (see below). Their server logic will be used
to modify objects such as plots or tables that exist in the server
function of a `teal_module`. A `ui` function can provide interactivity
but that is optional, an app developer is free to transform outputs
objects of a `teal` module that do not require user input.

### Requirements and Limitations

Transforming `teal` module output requires the following:

1.  **Module Support**:\
    `teal` will apply transformations to `teal_module` outputs, but the
    module in question must explicitly support this functionality. It is
    the responsibility of to the module developer to accept and consume
    the list of `teal_transform_module`.
2.  **Matching Object Names**:\
    Transformations have to reference variables that already exist in
    the `teal_module` server function and therefore must use the
    appropriate variable names. Think of it as extending the plot/table
    code that already exists in the module. Module developers are
    encouraged to provide the relevant names in the module’s
    documentation, otherwise the person writing the output
    transformation must follow the source code.
3.  **Maintaining Object Classes**:\
    A transformation must not alter the class of the object that it
    modifies. This is because a different class may require a different
    rendering function and that is part of the module structure, which
    beyond the control of decorators. If change of this magnitude is
    required, it is recommended to create a new module.

## Building Output Transformations (Decorators)

For simplicity, we will refer to the output transformers as
**decorators** in the code examples below.

### Server

Here we create a simple transformator that does not provide any user
input. Knowing that the module contains an object of class `ggplot2`
named `plot`, we will modify its title and x-axis title:

`static_decorator`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Static decorator"``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``, ``{`` `` ``plot`` ``<-`` ``plot`` ``+`` `` ``ggtitle``(``"This is a better title"``)`` ``+`` `` ``xlab``(``"the real x axis"``)`` `` ``}``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`

### UI

If the transformation requires a user input, a `ui` function can be
added. Here, the x-axis title is obtained from a `textInput` widget,
giving the user some flexibility. Note how the input values are passed
to the
[`within()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
function using its `...` argument. See
[`?teal.code::within.qenv`](https://insightsengineering.github.io/teal.code/latest-tag/reference/within.qenv.html)
for more examples.

`interactive_decorator`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Interactive decorator"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`div`](https://rstudio.github.io/htmltools/reference/builder.html)`(`` `` `[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``ns``(``"x_axis_title"``)``, ``"X axis title"``, value ``=`` ``"the suggested x axis"``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``{`` `` ``plot`` ``<-`` ``plot`` ``+`` `` ``ggtitle``(``"This is a better title"``)`` ``+`` `` ``xlab``(``my_title``)`` `` ``}``,`` `` my_title ``=`` ``input``$``x_axis_title`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`

### Variable Names as Arguments

The server function of a transforming `teal_transform_module` must
conform to the names of the variables that exist in the server function
of the transformed `teal_module`. Writing a universal transformator that
applies to any module is impossible because different modules may use
different variable names for their output elements. It is possible,
however, to create a transformator that will take the relevant variable
names as arguments. Here, the `output_name` variable name is passed to a
transformator, allowing it to work with multiple modules.

`dynamic_decorator`` ``<-`` ``function``(``output_name``)`` ``{`` `` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Dynamic decorator"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`div`](https://rstudio.github.io/htmltools/reference/builder.html)`(`` `` `[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``ns``(``"x_axis_title"``)``, ``"X axis title"``, value ``=`` ``"the syggested x axis"``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``{`` `` ``output_name`` ``<-`` ``output_name`` ``+`` `` ``xlab``(``x_axis_title``)`` `` ``}``,`` `` output_name ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``output_name``)``,`` `` x_axis_title ``=`` ``input``$``x_axis_title`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` `` ``)`` ``}`

Note that when the function is used, `output_name` will be passed a
character string but the expression passed to `within` needs a
`name`/`symbol`, a language object, hence the argument value must be
converted to a `name`.

## Using Output Transformations (Decorators)

Transformations are applied to a `teal` module as follows:

1.  A list of transformations is passed to the module constructor
    function (*e.g.* `tm_my_module`).
2.  The module constructor calls the module generator function
    ([`teal::module`](https://insightsengineering.github.io/teal/reference/teal_modules.md))
    and passes the transformations to the `ui_args` and `server_args`
    arguments.
3.  The module functions, UI and server, take a list of transformations
    as arguments and resolve them using `ui_transform_teal_data` and
    `srv_transform_teal_data`, respectively.

Here is a minimal illustration:

`# styler: off`` ``pseudo_decorated_module`` ``<-`` ``function``(`` `` ``label`` ``=`` ``"Pseudo Module with Decorator Support"``,`` `` ``decorators`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`` ``# <--- added block (1)`` ``)`` ``{`` `` `[`module`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` label ``=`` ``label``,`` `` ui_args ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``decorators ``=`` ``decorators``)``, ``# <--- added block (2)`` `` server_args ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``decorators ``=`` ``decorators``)``, ``# <--- added block (2)`` `` ui ``=`` ``function``(``id``, ``decorators``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`div`](https://rstudio.github.io/htmltools/reference/builder.html)`(`` `` ``# <input widgets>,`` `` ``# <output widgets>,`` `` `[`ui_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(``ns``(``"decorate"``)``, transformators ``=`` ``decorators``)`` ``# <--- added block (3)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``, ``decorators``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` ``# <receive inputs>`` `` ``# <process data>`` `` ``data_with_output`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``, ``output_item`` ``<-`` ``generate_output``(``)``)`` `` ``}``)`` `` ``data_with_output_decorated`` ``<-`` `[`srv_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(`` ``# <--- added block (3)`` `` ``"decorate"``, ``# <-`` `` data ``=`` ``data_with_output``, ``# <-`` `` transformators ``=`` ``decorators`` ``# <-`` `` ``)`` ``# <--- added block (3)`` `` ``# <render output>`` `` ``}``)`` `` ``}`` `` ``)`` ``}`` ``# styler: on`

The following examples demonstrate various uses of output
transformations.

### Single Transformation (Decoration)

In the first example we will apply one transformation to one output.

### Module

This module has one output, a plot created with `ggplot2`, and it
displays the reproducible code used to obtain the plot.

`tm_decorated_plot`` ``<-`` ``function``(``label`` ``=`` ``"module"``, ``decorators`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``)``)`` ``{`` `` ``checkmate``::`[`assert_list`](https://mllg.github.io/checkmate/reference/checkList.html)`(``decorators``, ``"teal_transform_module"``, null.ok ``=`` ``TRUE``)`` `` `` `[`module`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` label ``=`` ``label``,`` `` ui_args ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``decorators ``=`` ``decorators``)``,`` `` server_args ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``decorators ``=`` ``decorators``)``,`` `` ui ``=`` ``function``(``id``, ``decorators``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`div`](https://rstudio.github.io/htmltools/reference/builder.html)`(`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"dataname"``)``, label ``=`` ``"select dataname"``, choices ``=`` ``NULL``)``,`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"x"``)``, label ``=`` ``"select x"``, choices ``=`` ``NULL``)``,`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"y"``)``, label ``=`` ``"select y"``, choices ``=`` ``NULL``)``,`` `` `[`ui_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(``ns``(``"decorate"``)``, transformators ``=`` ``decorators``)``,`` `` `[`plotOutput`](https://rdrr.io/pkg/shiny/man/plotOutput.html)`(``ns``(``"plot"``)``)``,`` `` `[`verbatimTextOutput`](https://rdrr.io/pkg/shiny/man/textOutput.html)`(``ns``(``"text"``)``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``, ``decorators``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``, ``{`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``inputId ``=`` ``"dataname"``, choices ``=`` `[`names`](https://rdrr.io/r/base/names.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)``)`` `` ``}``)`` `` `` `[`observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)`(``input``$``dataname``, ``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``dataname``)`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``inputId ``=`` ``"x"``, choices ``=`` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``input``$``dataname``]``]``)``)`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``inputId ``=`` ``"y"``, choices ``=`` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``input``$``dataname``]``]``)``)`` `` ``}``)`` `` `` ``dataname`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(`[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``dataname``)``)`` `` ``x`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``x``, ``input``$``x`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``dataname``(``)``]``]``)``)`` `` ``input``$``x`` `` ``}``)`` `` ``y`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``y``, ``input``$``y`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``dataname``(``)``]``]``)``)`` `` ``input``$``y`` `` ``}``)`` `` `` ``# Plot is created within the teal_data object`` `` ``data_with_plot`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``dataname``(``)``, ``x``(``)``, ``y``(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``{`` `` ``plot`` ``<-`` ``ggplot2``::`[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``dataname``, ``ggplot2``::`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``x``, y ``=`` ``y``)``)`` ``+`` `` ``ggplot2``::`[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(``)`` `` ``}``,`` `` dataname ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``dataname``(``)``)``,`` `` x ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``x``(``)``)``,`` `` y ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``y``(``)``)`` `` ``)`` `` ``}``)`` `` `` ``# Decorators are applied`` `` ``data_with_plot_decorated`` ``<-`` `[`srv_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(`` `` ``"decorate"``,`` `` data ``=`` ``data_with_plot``,`` `` transformators ``=`` ``decorators`` `` ``)`` `` `` ``# (Decorated) plot object is extracted for rendering`` `` ``plot_r`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` ``data_with_plot_decorated``(``)``[[``"plot"``]``]`` `` ``}``)`` `` `` ``# Add plot printing statement to reproducible code`` `` ``## This does not affect the analysis but when the code is "replayed"`` `` ``## in an interactive session it will send the plot to a graphics device.`` `` ``reproducible_code`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(``data_with_plot_decorated``(``)``, expr ``=`` ``plot``)`` ``|>`` `` ``teal.code``::`[`get_code`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_code.html)`(``)`` `` ``}``)`` `` `` ``output``$``plot`` ``<-`` `[`renderPlot`](https://rdrr.io/pkg/shiny/man/renderPlot.html)`(``plot_r``(``)``)`` `` ``output``$``text`` ``<-`` `[`renderText`](https://rdrr.io/pkg/shiny/man/renderPrint.html)`(``reproducible_code``(``)``)`` `` ``}``)`` `` ``}`` `` ``)`` ``}`

#### Application

Note that every call to the module constructor (`tm_decorated_plot`)
takes a list containing *one* transformator.

`app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``iris ``=`` ``iris``, mtcars ``=`` ``mtcars``)``,`` `` modules ``=`` `[`modules`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` ``tm_decorated_plot``(``"undecorated"``)``,`` `` ``tm_decorated_plot``(``"static"``, decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``static_decorator``)``)``,`` `` ``tm_decorated_plot``(``"interactive"``, decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``interactive_decorator``)``)``,`` `` ``tm_decorated_plot``(``"dynamic"``, decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``dynamic_decorator``(``"plot"``)``)``)`` `` ``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

### Transforming Multiple Outputs (Decorators)

Here we will apply transformation to two outputs in one module.

#### Transformators

The plot transformators adds a user-provided title to a `ggplot2`
object.

`plot_decorator`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Decorate plot"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``ns``(``"plot_title"``)``, ``"Plot Title"``, value ``=`` ``"Title (editable)"``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``{`` `` ``plot`` ``<-`` ``plot`` ``+`` ``ggplot2``::`[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``ptitle``)`` ``+`` `` ``ggplot2``::`[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`` `` ``ggplot2``::`[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(`` `` plot.title ``=`` ``element_text``(``face ``=`` ``"bold"``, size ``=`` ``30``, color ``=`` ``"blue"``)`` `` ``)`` `` ``}``,`` `` ptitle ``=`` ``input``$``plot_title`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`

The table transformators adds a column to a `data.frame`.

`table_decorator`` ``<-`` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Decorate table"``,`` `` ui ``=`` ``function``(``id``)`` ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``p``(``"No UI needed for table decorator and could be ommited."``)``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``, ``{`` `` ``table_data``[[``"Added by decorator"``]``]`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"Row "``, `[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``table_data``)``)``)`` `` ``}``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`

#### Module

The following module uses `ggplot2` to generate a scatter plot, and
presents a simple `data.frame` as a summary table. Code for both outputs
is also displayed.

Note that the module constructor accepts one list of transformations and
the transformations are then manually separated in the module functions.

`tm_decorated_plot_table`` ``<-`` ``function``(``label`` ``=`` ``"module with two outputs"``, ``decorators`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``)``)`` ``{`` `` ``checkmate``::`[`assert_list`](https://mllg.github.io/checkmate/reference/checkList.html)`(``decorators``, ``"teal_transform_module"``, null.ok ``=`` ``TRUE``)`` `` `` `[`module`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` label ``=`` ``label``,`` `` ui_args ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``decorators ``=`` ``decorators``)``,`` `` server_args ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``decorators ``=`` ``decorators``)``,`` `` ui ``=`` ``function``(``id``, ``decorators``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`div`](https://rstudio.github.io/htmltools/reference/builder.html)`(`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"dataname"``)``, label ``=`` ``"Select dataset"``, choices ``=`` ``NULL``)``,`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"x"``)``, label ``=`` ``"Select x-axis"``, choices ``=`` ``NULL``)``,`` `` `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(``ns``(``"y"``)``, label ``=`` ``"Select y-axis"``, choices ``=`` ``NULL``)``,`` `` `` ``# Separately inject UI for plot and table decorators`` `` `[`ui_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(``ns``(``"decorate_plot"``)``, transformators ``=`` ``decorators``$``plot``)``,`` `` `[`ui_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(``ns``(``"decorate_table"``)``, transformators ``=`` ``decorators``$``table``)``,`` `` `[`plotOutput`](https://rdrr.io/pkg/shiny/man/plotOutput.html)`(``ns``(``"plot"``)``)``,`` `` `[`tableOutput`](https://rdrr.io/pkg/shiny/man/renderTable.html)`(``ns``(``"table"``)``)``,`` `` `[`verbatimTextOutput`](https://rdrr.io/pkg/shiny/man/textOutput.html)`(``ns``(``"text"``)``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``, ``decorators``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``, ``{`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``inputId ``=`` ``"dataname"``, choices ``=`` `[`names`](https://rdrr.io/r/base/names.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)``)`` `` ``}``)`` `` `` ``dataname`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(`[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``dataname``)``)`` `` `` `[`observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)`(``dataname``(``)``, ``{`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``inputId ``=`` ``"x"``, choices ``=`` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``input``$``dataname``]``]``)``)`` `` `[`updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)`(``inputId ``=`` ``"y"``, choices ``=`` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``input``$``dataname``]``]``)``)`` `` ``}``)`` `` ``x`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``x``, ``input``$``x`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``dataname``(``)``]``]``)``)`` `` ``input``$``x`` `` ``}``)`` `` ``y`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``y``, ``input``$``y`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``[[``dataname``(``)``]``]``)``)`` `` ``input``$``y`` `` ``}``)`` `` `` ``# Separately create outputs within teal_data objects in separate reactive expressions`` `` ``plot_data`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``dataname``(``)``, ``x``(``)``, ``y``(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``{`` `` ``plot`` ``<-`` ``ggplot2``::`[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``dataname``, ``ggplot2``::`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``xvar``, y ``=`` ``yvar``)``)`` ``+`` `` ``ggplot2``::`[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(``)`` `` ``}``,`` `` dataname ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``dataname``(``)``)``,`` `` xvar ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``x``(``)``)``,`` `` yvar ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``y``(``)``)`` `` ``)`` `` ``}``)`` `` ``table_data`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``dataname``(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``{`` `` ``table_data`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`[`lapply`](https://rdrr.io/r/base/lapply.html)`(``dataname``, ``mean``, na.rm ``=`` ``TRUE``)``)`` `` ``}``,`` `` dataname ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``dataname``(``)``)`` `` ``)`` `` ``}``)`` `` `` ``# Separately apply decoration to the outputs`` `` ``decorated_plot`` ``<-`` `[`srv_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(`` `` ``"decorate_plot"``,`` `` data ``=`` ``plot_data``,`` `` transformators ``=`` ``decorators``$``plot`` `` ``)`` `` ``decorated_table`` ``<-`` `[`srv_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(`` `` ``"decorate_table"``,`` `` data ``=`` ``table_data``,`` `` transformators ``=`` ``decorators``$``table`` `` ``)`` `` `` ``output``$``plot`` ``<-`` `[`renderPlot`](https://rdrr.io/pkg/shiny/man/renderPlot.html)`(``decorated_plot``(``)``[[``"plot"``]``]``)`` `` ``output``$``table`` ``<-`` `[`renderTable`](https://rdrr.io/pkg/shiny/man/renderTable.html)`(``decorated_table``(``)``[[``"table_data"``]``]``)`` `` `` ``output``$``text`` ``<-`` `[`renderText`](https://rdrr.io/pkg/shiny/man/renderPrint.html)`(``{`` `` ``plot_code`` ``<-`` ``teal.code``::`[`get_code`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_code.html)`(`[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``decorated_plot``(``)``)``)`` `` ``table_code`` ``<-`` ``teal.code``::`[`get_code`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_code.html)`(`[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``decorated_table``(``)``)``)`` `` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"# Plot Code:"``, ``plot_code``, ``"\n\n# Table Code:"``, ``table_code``)`` `` ``}``)`` `` ``}``)`` `` ``}`` `` ``)`` ``}`

#### Application

Note that a named list of transformations is passed to the module
constructor.

`app`` ``<-`` `[`init`](https://insightsengineering.github.io/teal/reference/init.md)`(`` `` data ``=`` `[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``iris ``=`` ``iris``, mtcars ``=`` ``mtcars``)``,`` `` modules ``=`` `[`modules`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`` `` ``tm_decorated_plot_table``(`` `` ``"plot_and_table"``,`` `` decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` plot ``=`` ``plot_decorator``,`` `` table ``=`` ``table_decorator`` `` ``)`` `` ``)`` `` ``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`` ``}`

## Convenience

Here we present some ways to work with transformators more conveniently.
These are purely optional.

### Reducing Boilerplate

The function `make_teal_transform_server` can be used to reduce the
amount of boilerplate code when writing new transformators. It takes
`language` as input and requires you to use `input` object names
directly in the expression. The following calls yield the same
transformator module. Note that the combination of
`my_title = input$x_axis_title` and `xlab(my_title)` is replaced by a
simple `xlab(x_axis_table)`.

[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Static decorator"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`div`](https://rstudio.github.io/htmltools/reference/builder.html)`(`` `` `[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``ns``(``"x_axis_title"``)``, ``"X axis title"``, value ``=`` ``"x axis"``)`` `` ``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` `[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(`[`data`](https://rdrr.io/r/utils/data.html)`(``)``)`` `` `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)``,`` `` ``{`` `` ``plot`` ``<-`` ``plot`` ``+`` ``ggtitle``(``"This is a better title"``)`` ``+`` ``xlab``(``x_axis_title``)`` `` ``}``,`` `` x_axis_title ``=`` ``input``$``x_axis_title`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` ``)`` `` `[`teal_transform_module`](https://insightsengineering.github.io/teal/reference/teal_transform_module.md)`(`` `` label ``=`` ``"Static decorator (language)"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``ns`` ``<-`` `[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`` `` `[`div`](https://rstudio.github.io/htmltools/reference/builder.html)`(`` `` `[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``ns``(``"x_axis_title"``)``, ``"X axis title"``, value ``=`` ``"x axis"``)`` `` ``)`` `` ``}``,`` `` server ``=`` `[`make_teal_transform_server`](https://insightsengineering.github.io/teal/reference/make_teal_transform_server.md)`(`` `` `[`expression`](https://rdrr.io/r/base/expression.html)`(`` `` ``plot`` ``<-`` ``plot`` ``+`` ``ggtitle``(``"This is a better title"``)`` ``+`` ``xlab``(``x_axis_title``)`` `` ``)`` `` ``)`` ``)`

### Multiple Transformations

Multiple decorators can be passed to
[`srv_transform_teal_data()`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)/[`ui_transform_teal_data()`](https://insightsengineering.github.io/teal/reference/module_transform_data.md).
They will be executed in sequence, errors and warnings will show up on
the appropriate decorator. Remember that they should receive a list of
`teal_transform_module`.

`# in the module UI function`` ``ui_transform_module``(``ns``(``"decorate"``)``, ``decorators``)`` `` ``# in the module server function`` `[`srv_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(`` `` ``"decorate"``,`` `` data ``=`` ``data``,`` `` transformators ``=`` ``decorators``,`` `` expr ``=`` `[`quote`](https://rdrr.io/r/base/substitute.html)`(``obj``)`` ``# Often we want to display the output of the decorator at the end`` ``)`
