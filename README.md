# Teal: Interactive Exploratory Data Analysis with Shiny Web-Applications


*teal* is a shiny-based interactive exploration framework for analyzing clinical
trials data. `teal` currently provides a dynamic filtering facility and diverse
data viewers. `teal` shiny applications are built using standard [shiny
modules](https://shiny.rstudio.com/articles/modules.html).

Please read more about teal on our agile-R website at [go.roche.com/agile-R](http://go.roche.com/agile-R).
 

# Acknowledgement

We would like to thank everyone who made `teal` a better analysis environment. Special thanks go to:

 * `Doug Kelkhoff` for his contributions to the styling of the filter panel, see https://github.roche.com/kelkhofd/shinyDataFilter.
 

# Notes for Developers
## Conventions
Shiny modules are implemented in files `<module>.R` with UI function `ui_<module>` and server function `srv_<module>`.

A module with a `id` should not use the id itself (`ns(character(0))`) as this id belongs to the parent module:
```
ns <- NS(id)
ns(character(0)) # should not be used as parent module may be using it to show / hide this module
ns("whole") # is okay, as long as the input to ns is not character(0)
ns("") # empty string "" is allowed
```
HTML elements can be given CSS classes even if they are not used within this package to give the end-user the possibility to modify the look-and-feel.

Here is a full example:
```
child_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("whole"), # used to show / hide itself
    class = "to_customize_by_end_user",
    # other code here
    p("Example")
  )
}
parent_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("whole"), # used to show / hide itself
    div(
      id = ns("BillyTheKid"), # used to show / hide the child
      child_ui("BillyTheKid") # this id belongs to this module, not to the child
    )
  )
}
parent_ui("PatrickMcCarty")
```

Use the `roxygen2` marker `@md` to include code-style ticks with backticks. This makes it easier to read. For example:
```
#' My function
#' 
#' A special `variable` we refer to.
#' We link to another function `\link{another_fcn}`
#' 
#' @md
#' @param arg1 `character` person's name
my_fcn <- function(arg1) {
  arg1
}
```
Note that `\link{another_fcn}` links don't work in the development version. For this, you need to install the package.

To temporarily install the package, the following code is useful:
```
.libPaths()
temp_install_dir <- tempfile(); dir.create(temp_install_dir)
.libPaths(c(temp_install_dir, .libPaths())); .libPaths()
?init # look at doc
# restore old path once done
.libPaths(.libPaths()[-1]); .libPaths()
```

Add a summary at the top of each file to describe what it does.

Shiny modules should return `reactives` along with the observers they create. An example is here:
```
srv_child <- function(input, output, session) {
  o <- observeEvent(...)
  return(list(
    values = reactive(input$name),
    observers = list(o)
  ))
}
srv_parent <- function(input, output, session) {
  output <- callModule(srv_child, "child")
  o <- observeEvent(...)
  return(list(
    values = output$values,
    observers = c(output$observers, list(o))
  ))
}
```
This makes dynamic UI creation possible. If you want to remove a module added with `insertUI`,  you can do so by calling `o$destroy()` on each of its observers, see the function `srv_filter_items` for an example.

The difference between `datanames` and `active_datanames` is that the latter is a subset of the former.

Note: Whenever you return input objects, it seems that you explicitly need to wrap them inside `reactive(..)`, e.g. `reactive(input$name)`.

Modules should respect the argument order: `function(input, output, session, datasets, ...)`, where `...` can also include further named arguments.

The idiom `shinyApp(ui, server) %>% invisible()` is used with internal Shiny modules that are not exported. Printing a `shinyApp` causes it to call `runApp` so this avoids running the app, but still checking that the ui function and server are valid. Since `teal::init` returns an `app` object and is used by end users, we don't use this trick there.
