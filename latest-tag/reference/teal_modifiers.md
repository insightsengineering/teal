# Replace UI Elements in `teal` UI objects

Replace UI Elements in `teal` UI objects

## Usage

``` r
modify_title(x, title = "teal app", favicon = NULL)

modify_header(x, element = tags$p())

modify_footer(x, element = tags$p())
```

## Arguments

- x:

  (`teal_app`) A `teal_app` object created using the `init` function.

- title:

  (`shiny.tag` or `character(1)`) The new title to be used.

- favicon:

  (`character`) The path for the icon for the title. The image/icon path
  can be remote or the static path accessible by `shiny`, like the
  `www/`. If the favicon is `NULL` the `teal` logo will be used as the
  favicon.

- element:

  Replacement UI element (shiny tag or HTML)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIFpUcxUNql2A6dIAmUUlGkBeaV2oB9Z6-YASSxAgGUPaVpGWgBnXGkAWV0AYQBBLHDPGFICFhieXHtpGCJHFWo4GIiSsoqY9jgAD1hUCp8a8rhtfggdAB8APiKa2iVWH3FSCs4bCoi+MGSVGNIiGC9ZuAWegVHpdiFyUXEpbukQIpiACyFWVPR2cwASFVp455iRKUYegF8wX4AXSAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIFpUcxUNql2A6dIAmUUlGkBeaV2oB9Z6-YASSxAgGUPaVpGWgBnHlx7aRgiRxVqOBiI5NT0mPY4AA9YVHSfbLS4bX4IHQAfAD5E7NolVh8ACzgoRxF89PgyCNcAcxiAEkdJdnaAZjswAGEVGNIiGGlO7pE+MB4900ElaXYhclFxKSrpEESY9qFWAEF0dnMxlVpcaTeYkSlGaoAXzAgIAukA)

- example-3:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIFpUcxUNql2A6dIAmUUlGkBeaV2oB9Z6-YASSxAgGUPaVpGWgBnHlx7aRgiRxVqOBiI5NT0mPY4AA9YVHSfbLS4bX4IHQAfAD5E7NolVh8lIiJyRnz0+DIIvjAAYRUY0iIYZU7uoeqBFul2IW6oDSkq6RBEmIALIVYAQXR2cwASFVpcaXOYkSlGaoBfMCeAXSA)

## Examples

``` r
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = modules(example_module())
) |>
  modify_title(title = "Custom title")

if (interactive()) {
  shinyApp(app$ui, app$server)
}
app <- init(
  data = teal_data(IRIS = iris),
  modules = modules(example_module())
) |>
  modify_header(element = tags$div(h3("Custom header")))

if (interactive()) {
  shinyApp(app$ui, app$server)
}
app <- init(
  data = teal_data(IRIS = iris),
  modules = modules(example_module())
) |>
  modify_footer(element = "Custom footer")

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
