# App state management.

**\[experimental\]**

Capture and restore the global (app) input state.

## Usage

``` r
ui_bookmark_panel(id, modules)

srv_bookmark_panel(id, modules)

get_bookmarking_option()

need_bookmarking(modules)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- modules:

  (`teal_modules`) `teal_modules` object. These are the specific output
  modules which will be displayed in the `teal` application. See
  [`modules()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  and
  [`module()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  for more details.

## Value

Invisible `NULL`.

## Details

This module introduces bookmarks into `teal` apps: the `shiny`
bookmarking mechanism becomes enabled and server-side bookmarks can be
created.

The bookmark manager presents a button with the bookmark icon and is
placed in the tab-bar. When clicked, the button creates a bookmark and
opens a modal which displays the bookmark URL.

`teal` does not guarantee that all modules (`teal_module` objects) are
bookmarkable. Those that are, have a `teal_bookmarkable` attribute set
to `TRUE`. If any modules are not bookmarkable, the bookmark manager
modal displays a warning and the bookmark button displays a flag. In
order to communicate that a external module is bookmarkable, the module
developer should set the `teal_bookmarkable` attribute to `TRUE`.

## Server logic

A bookmark is a URL that contains the app address with a
`/?_state_id_=<bookmark_dir>` suffix. `<bookmark_dir>` is a directory
created on the server, where the state of the application is saved.
Accessing the bookmark URL opens a new session of the app that starts in
the previously saved state.

## Note

To enable bookmarking use either:

- `shiny` app by using `shinyApp(..., enableBookmarking = "server")`
  (not supported in `shinytest2`)

- set `options(shiny.bookmarkStore = "server")` before running the app
