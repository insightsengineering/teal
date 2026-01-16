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

``` r

library(teal)

data <- within(teal_data(), {
  iris <- iris
  mtcars <- mtcars
})

app <- init(
  data = data,
  modules = example_module()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```

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

``` r

data <- within(teal_data(), {
  iris <- iris
  mtcars <- mtcars
})

transformator_iris <- teal_transform_module(
  label = "Custom transformator for iris",
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      numericInput(ns("n_rows"), "Number of rows to display", value = 6, min = 1, max = 150, step = 1)
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        within(
          data(),
          iris <- head(iris, num_rows),
          num_rows = input$n_rows
        )
      })
    })
  }
)

app <- init(
  data = data,
  modules = example_module(transformators = list(transformator_iris))
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```

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

``` r

data <- within(teal_data(), {
  iris <- iris
  mtcars <- mtcars
})

transformator_iris <- teal_transform_module(
  label = "Custom transformator for iris",
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      numericInput(ns("n_rows"), "Number of rows to subset", value = 6, min = 1, max = 150, step = 1)
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        within(
          data(),
          iris <- head(iris, num_rows),
          num_rows = input$n_rows
        )
      })
    })
  }
)

transformator_mtcars <- teal_transform_module(
  label = "Custom transformator for mtcars",
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      "Adding rownames column to mtcars"
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        within(data(), {
          mtcars$rownames <- rownames(mtcars)
          rownames(mtcars) <- NULL
        })
      })
    })
  }
)

my_transformators <- list(
  transformator_iris,
  transformator_mtcars
)

app <- init(
  data = data,
  modules = example_module(transformators = my_transformators)
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```

#### Targeting one dataset

It is also possible to have multiple transformator modules act on one
dataset. In such cases, transformations will be executed in the same
order in which the transformator modules are passed to the module.

``` r

data <- within(teal_data(), {
  iris <- iris
  mtcars <- mtcars
})

transformator_iris_scale <- teal_transform_module(
  label = "Scaling transformator for iris",
  ui = function(id) {
    ns <- NS(id)
    uiOutput(ns("scaled_columns_container"))
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      scalable_columns <- names(Filter(is.numeric, data()[["iris"]])) |> isolate()

      output$scaled_columns_container <- renderUI({
        selectInput(
          inputId = ns("scaled_columns"),
          label = "Columns to scale",
          choices = scalable_columns,
          selected = input$scaled_columns,
          multiple = TRUE
        )
      })

      reactive({
        within(
          data(),
          {
            iris[scaled_columns] <- scale(iris[scaled_columns])
          },
          scaled_columns = input$scaled_columns
        )
      })
    })
  }
)

transformator_iris <- teal_transform_module(
  label = "Custom transformator for iris",
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      numericInput(ns("n_rows"), "Number of rows to subset", value = 6, min = 1, max = 150, step = 1)
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        within(
          data(),
          iris <- head(iris, num_rows),
          num_rows = input$n_rows
        )
      })
    })
  }
)

transformator_mtcars <- teal_transform_module(
  label = "Custom transformator for mtcars",
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      "Adding rownames column to mtcars"
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        within(data(), {
          mtcars$rownames <- rownames(mtcars)
          rownames(mtcars) <- NULL
        })
      })
    })
  }
)

my_transformators <- list(
  transformator_iris,
  transformator_iris_scale,
  transformator_mtcars
)

app <- init(
  data = data,
  modules = example_module(transformators = my_transformators)
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```

This approach provides greater flexibility in data preprocessing,
allowing transformations to be tailored to specific datasets for a
specific module.
