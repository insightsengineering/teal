# `teal` user session info module

Module to display the user session info popup and to download a
lockfile. Module is included when running
[`init()`](https://insightsengineering.github.io/teal/reference/init.md)
but skipped when using
[`module_teal`](https://insightsengineering.github.io/teal/reference/module_teal.md).
Please be aware that session info contains R session information, so
multiple module's calls will share the same information.

## Usage

``` r
ui_session_info(id)

srv_session_info(id)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

## Value

`NULL` invisibly

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEracxUurmAJgAUoAczjsB06eYD6AZzg+fTQgvISUiNzA-AKCQiDC+MH4IJIE-RilGS2U1DS0hVBVSXGkiQoKi6SjAkh0Qd0r0339q4NDwhKqYtoSkgF9TQSVpdiFyUXEpbVr6nwALIVYAQXR2c2K0jL6BMF6AXSA)

## Examples

``` r
ui <- fluidPage(
  ui_session_info("session_info")
)

server <- function(input, output, session) {
  srv_session_info("session_info")
}

if (interactive()) {
  shinyApp(ui, server)
}
```
