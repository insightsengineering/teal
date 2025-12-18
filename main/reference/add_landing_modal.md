# Add a Landing Popup to `teal` Application

Adds a landing popup to the `teal` app. This popup will be shown when
the app starts. The dialog must be closed by the app user to proceed to
the main application.

## Usage

``` r
add_landing_modal(
  x,
  title = NULL,
  content = NULL,
  footer = modalButton("Accept"),
  ...
)
```

## Arguments

- x:

  (`teal_app`) A `teal_app` object created using the `init` function.

- title:

  An optional title for the dialog.

- content:

  (`character(1)`, `shiny.tag` or `shiny.tag.list`) with the content of
  the popup.

- footer:

  UI for footer. Use `NULL` for no footer.

- ...:

  Additional arguments to
  [`shiny::modalDialog()`](https://rdrr.io/pkg/shiny/man/modalDialog.html).

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIFpUcxUNql2A6dIAmUUlGkBeaV2oB9Z6-YASSxAgGUPaVpGWgBnXGkAWV0AYQBBLHDPGFICFhieXHtpGCJHFWo4GIiSsoqY9jgAD1hUCp8a8rhtfggdAB8APiKoR0cfaigIRyEAc3bS7jsIBwdxUgqIvjAAdThqYngtwuWV4mEyTbBdAAtYyKq3CanZ6VQiVBVMI6KHehVSUgkKpZBbUABC-0BEDsYFSBAIcFQpC2PQcPQEtCU0nYQnIonEUm60hARRitwgrFS6HY5gAJCpaPE6TERFJGD0AL5gDkAXSAA)

## Examples

``` r
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = modules(example_module())
) |>
  add_landing_modal(
    title = "Welcome",
    content = "This is a landing popup.",
    buttons = modalButton("Accept")
  )

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
