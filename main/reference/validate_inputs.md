# Send input validation messages to output

Captures messages from `InputValidator` objects and collates them into
one message passed to `validate`.

## Usage

``` r
validate_inputs(..., header = "Some inputs require attention")
```

## Arguments

- ...:

  either any number of `InputValidator` objects or an optionally named,
  possibly nested `list` of `InputValidator` objects, see `Details`

- header:

  (`character(1)`) generic validation message; set to NULL to omit

## Value

Returns NULL if the final validation call passes and a
`shiny.silent.error` if it fails.

## Details

[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html) is used
to withhold rendering of an output element until certain conditions are
met and to print a validation message in place of the output element.
[`shinyvalidate::InputValidator`](https://rstudio.github.io/shinyvalidate/reference/InputValidator.html)
allows to validate input elements and to display specific messages in
their respective input widgets. `validate_inputs` provides a hybrid
solution. Given an `InputValidator` object, messages corresponding to
inputs that fail validation are extracted and placed in one validation
message that is passed to a `validate`/`need` call. This way the input
`validator` messages are repeated in the output.

The `...` argument accepts any number of `InputValidator` objects or a
nested list of such objects. If `validators` are passed directly, all
their messages are printed together under one (optional) header message
specified by `header`. If a list is passed, messages are grouped by
`validator`. The list's names are used as headers for their respective
message groups. If neither of the nested list elements is named, a
header message is taken from `header`.

## See also

[`shinyvalidate::InputValidator`](https://rstudio.github.io/shinyvalidate/reference/InputValidator.html),
[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogMZwAjitpn2fMAGcAFkNYTutACZRyd-hDpMLBxOLn4BzGzsIRCu7l7kfgJWcopK1FYeAApQAOZwthDS0vZw1HBiAJIQqCqktmDwpI5EHna40nZudPGahY3NrXjSBPUllhTi3G0dhEQw9EJwg+12OYxEKqhLvjy4AkX2nnD0LAAyUKwbdftFxUcnjNkQpQW3tyVlldW19WWk5IxpnYPuVSNIoNI-gDENMRlCRPZgABGRAAZgAuu1TkZdLojFgAMrAAAsiAAbOieLsbm8QV8atdICp5iIgQ5SqDwdIIMz6CIYUMUWTqYU3tJSLl7AASJjaPai2kc+k-OzEahEQFDYFKsEQtUagXysW3EZ2ejUKAEADWbKEHloUAgZg8ACY2fZUIwhDk1nAKG6tbNGBA0kQAO4ic0qOC+I3G6QwFTUcSoMrSAC8ekMJgVtxFxvs3REVQZo1oAC8Y4G6WDUEQhGDDpXDTSxTAhBnpAAGDBI9owKAAD07xPaXWjnZ7LoArK2in43vnbgOhE8XqmiKQAPK1Ut2DekHY3PyJCAlRhSRgpZRqDRaIQM9pXBk6EA3ADExTgYM20gftWkLpPG8Xobkka8S1qAA1OJvA1KVnjDbQwIkKUoA8DwAH1GCTfI7HhTV2nsCRMKETCSjqbFcXxAkVkIZoiBKLkCDQWgJWoSFvwBI9RUkNCMOw3D6h5FlCJvdRxC0QdX1bWglGkdgoHsDAGzgPJGHYaTpAAUm06QXVODNMyRU4dFVBimMdaQ4CkQoRL5TUbgAXwXP9UIoKBzXyVzPxgDUZH-MEgJ6EgUMw1AWAg75SBg7o4MYBC4CQ1zJHClh+KwnCynqfUxOI7CLCsZ12Hgex7FyGRM3MohGJkPUiHVTUqTCiKEvQzKhNVBqNWmVQJN6TSZNzOSFLKCAciaQbpAAPmkEyzPomqmJIahWGkEgZFyuxnJSkjWoywTstbYEKyrOMDhIvlSAjChXjFMolDBTMe2ndovRyRwnukVFzuXOAyoqzClBgL7qtqrlxxkK6bsKEAHtIJzwQgDxpBAd7Pqc7bc1c3a0oSjyvOQiAbmfWopQPa8zGRkRMnVOo31zT9gu8GRScPXN7DDNiCEcdhAuAYA7H6Fo7HRTE5xmMZozIB1qDsTsGfjQDYPIUjovsPmJFc41mdVwKNdS1r2kcOB0JETs7AJb9pE9IgtivNY0GcFiONa2BvwRXwJac36ii6+ZFkGTtdbgNWGQNiR2kNlglzeVZ1k2bYwGDlXQ-19g6Hsa5czjsACTmAL1ekMxLGsOr-gmUDk8zSRfduS3rdt+3pEd1Bne4G2WHdgF7Hlmu9pYCXmuxgRWwPdglCgNjHFUagpRETZJLPaQAD9lCnppZ6lMMN+9OuLXsTs+xtnmj7JOu1U7fnBdmRrRcxYY4GHGvooFk7K3v1tXJcgRMeJwR5J82EKIcQUhtBDQOM4GIABBdA7ArBEREJePwf8wBOXREAA)

## Examples

``` r
library(shiny)
library(shinyvalidate)

ui <- fluidPage(
  selectInput("method", "validation method", c("sequential", "combined", "grouped")),
  sidebarLayout(
    sidebarPanel(
      selectInput("letter", "select a letter:", c(letters[1:3], LETTERS[4:6])),
      selectInput("number", "select a number:", 1:6),
      tags$br(),
      selectInput("color", "select a color:",
        c("black", "indianred2", "springgreen2", "cornflowerblue"),
        multiple = TRUE
      ),
      sliderInput("size", "select point size:",
        min = 0.1, max = 4, value = 0.25
      )
    ),
    mainPanel(plotOutput("plot"))
  )
)

server <- function(input, output) {
  # set up input validation
  iv <- InputValidator$new()
  iv$add_rule("letter", sv_in_set(LETTERS, "choose a capital letter"))
  iv$add_rule("number", function(x) {
    if (as.integer(x) %% 2L == 1L) "choose an even number"
  })
  iv$enable()
  # more input validation
  iv_par <- InputValidator$new()
  iv_par$add_rule("color", sv_required(message = "choose a color"))
  iv_par$add_rule("color", function(x) {
    if (length(x) > 1L) "choose only one color"
  })
  iv_par$add_rule(
    "size",
    sv_between(
      left = 0.5, right = 3,
      message_fmt = "choose a value between {left} and {right}"
    )
  )
  iv_par$enable()

  output$plot <- renderPlot({
    # validate output
    switch(input[["method"]],
      "sequential" = {
        validate_inputs(iv)
        validate_inputs(iv_par, header = "Set proper graphical parameters")
      },
      "combined" = validate_inputs(iv, iv_par),
      "grouped" = validate_inputs(list(
        "Some inputs require attention" = iv,
        "Set proper graphical parameters" = iv_par
      ))
    )

    plot(faithful$eruptions ~ faithful$waiting,
      las = 1, pch = 16,
      col = input[["color"]], cex = input[["size"]]
    )
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
```
