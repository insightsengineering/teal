# Create the server and UI function for the `shiny` app

End-users: This is the most important function for you to start a `teal`
app that is composed of `teal` modules.

## Usage

``` r
init(
  data,
  modules,
  filter = teal_slices(),
  title = lifecycle::deprecated(),
  header = lifecycle::deprecated(),
  footer = lifecycle::deprecated(),
  id = lifecycle::deprecated(),
  reporter = teal.reporter::Reporter$new()
)
```

## Arguments

- data:

  (`teal_data` or `teal_data_module`) For constructing the data object,
  refer to
  [`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)
  and
  [`teal_data_module()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md).

- modules:

  (`list` or `teal_modules` or `teal_module`) Nested list of
  `teal_modules` or `teal_module` objects or a single `teal_modules` or
  `teal_module` object. These are the specific output modules which will
  be displayed in the `teal` application. See
  [`modules()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  and
  [`module()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  for more details.

- filter:

  (`teal_slices`) Optionally, specifies the initial filter using
  [`teal_slices()`](https://insightsengineering.github.io/teal/reference/teal_slices.md).

- title:

  (`shiny.tag` or `character(1)`) **\[deprecated\]** Optionally, the
  browser window title. Defaults to a title "teal app" with the icon of
  NEST. Can be created using the
  [`build_app_title()`](https://insightsengineering.github.io/teal/reference/build_app_title.md)
  or by passing a valid `shiny.tag` which is a head tag with title and
  link tag. This parameter is no longer supported. Use
  [`modify_title()`](https://insightsengineering.github.io/teal/reference/teal_modifiers.md)
  on the teal app object instead.

- header:

  (`shiny.tag` or `character(1)`) **\[deprecated\]** Optionally, the
  header of the app. This parameter is no longer supported. Use
  [`modify_header()`](https://insightsengineering.github.io/teal/reference/teal_modifiers.md)
  on the teal app object instead.

- footer:

  (`shiny.tag` or `character(1)`) **\[deprecated\]** Optionally, the
  footer of the app. This parameter is no longer supported. Use
  [`modify_footer()`](https://insightsengineering.github.io/teal/reference/teal_modifiers.md)
  on the teal app object instead.

- id:

  **\[deprecated\]** (`character`) Optionally, a string specifying the
  `shiny` module id in cases it is used as a `shiny` module rather than
  a standalone `shiny` app. This parameter is no longer supported. Use
  [`ui_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md)
  and
  [`srv_teal()`](https://insightsengineering.github.io/teal/reference/module_teal.md)
  instead.

- reporter:

  (`Reporter`) object used to store report contents. Set to `NULL` to
  globally disable reporting.

## Value

Named list containing server and UI functions.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIFpUcxUNql2A6dIAmUUlGkBeaQHcbACyF2EA4OXNQA+s6u2rj2wSCxwdIQcF5htIy0AM6W0qTMEJlKRIww7OlZuNK0jh7SmXAAjmHUFOwQjEReZRmZPH0JwcmpMKQELNkKufmFxaUjY4yZldW19U0tEG0dXfPjffxBwQC+sTwxhzBEjiot2Z6X17eBiQ83cM+J0tRQ9HDUtXwwJE3JkiCpGAQ4IDzp86iIpIxaqp1OItEJUCpSJUwaQMVi4ZlMpoIJVgToQEcYZ8VLQkWoNGjHJUMCydK4AOaZAAkjkk7FQdjAQiKJRcxOkPxxThcILBEKhYD6VMSwOg8Du0kB3GogIGZwGcAAHrBUC0wq8Wuxvr9-p5AUaTS1cnBuNILQr9YcHO6PolAQBJHrSADKcFQ3AwABkKOzSL5pP5MqQiOzmDBoQMHPVGAi6SjiWUIHjsZjiwSiSRSTLyZngji8VzE6QcowKI4RAAFahEWy1xJN9jA7TAYCAoZpHqAgC6U65ofD1CjMbjB1hDlXn0ptZpeYZm2qzNZ0niXs+BRyADlg2VHBvYaaewB5UuYtqZQVNwH9U8OLc-6WuGqcAamOKQTlkuqnhunoOEotDUOQiKeKEYSZHQkLvgMKFobQkKDjKQEApAYHlJk0LSBILCEXaYDBqgcAELQwFfsqzrcKh6HvKqsAyDR46keRlGMNRmq0WGEbRhAsa+CxWEuuEOF4dx8BEeOuyLIJVE8URBCsDqiqsUaBDUCo7ZhEJQEanQSZtCRQaeAQgrzhGADq1RxuRgIdnAriLm5jgeYqMHBO6qH0YxcEELU+jGKxMDmEI7K1NZvb-gABg6MCmjIoRulcbxpapdlZCG4VMWReC1mlgYlc5i6SdJCZZMmqawIVfHFdkdEMeVGb-uy3b0OxcEISIIHEcMozjNIun6XqpwCActBKNIhaIVAGhSNoNaHJk-gQKwACC6DsOYXI0pUZ3ZgiBwnBAYBHFOQA)

## Examples

``` r
app <- init(
  data = within(
    teal_data(),
    {
      new_iris <- transform(iris, id = seq_len(nrow(iris)))
      new_mtcars <- transform(mtcars, id = seq_len(nrow(mtcars)))
    }
  ),
  modules = modules(
    module(
      label = "data source",
      server = function(input, output, session, data) {},
      ui = function(id, ...) tags$div(p("information about data source")),
      datanames = "all"
    ),
    example_module(label = "example teal module"),
    module(
      "Iris Sepal.Length histogram",
      server = function(input, output, session, data) {
        output$hist <- renderPlot(
          hist(data()[["new_iris"]]$Sepal.Length)
        )
      },
      ui = function(id, ...) {
        ns <- NS(id)
        plotOutput(ns("hist"))
      },
      datanames = "new_iris"
    )
  ),
  filter = teal_slices(
    teal_slice(dataname = "new_iris", varname = "Species"),
    teal_slice(dataname = "new_iris", varname = "Sepal.Length"),
    teal_slice(dataname = "new_mtcars", varname = "cyl"),
    exclude_varnames = list(new_iris = c("Sepal.Width", "Petal.Width")),
    module_specific = TRUE,
    mapping = list(
      `example teal module` = "new_iris Species",
      `Iris Sepal.Length histogram` = "new_iris Species",
      global_filters = "new_mtcars cyl"
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
