# Build app title with favicon

A helper function to create the browser title along with a logo.

## Usage

``` r
build_app_title(
  title = "teal app",
  favicon =
    "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
)
```

## Arguments

- title:

  (`character`) The browser title for the `teal` app.

- favicon:

  (`character`) The path for the icon for the title. The image/icon path
  can be remote or the static path accessible by `shiny`, like the
  `www/`

## Value

A `shiny.tag` containing the element that adds the title and logo to the
`shiny` app.
