# Teal: Interactive Exploratory Data Analysis with Shiny Web-Applications


*teal* is a shiny-based interactive exploration framework for analyzing clinical
trials data. `teal` currently provides a dynamic filtering facility and diverse
data viewers. `teal` shiny applications are built using standard [shiny
modules](https://shiny.rstudio.com/articles/modules.html).

Please read more about teal on our agile-R website at [go.roche.com/agile-R](http://go.roche.com/agile-R).
 

# Acknowledgements

We would like to thank everyone who made `teal` a better analysis environment. Special thanks go to:

 * Doug Kelkhoff for his contributions to the filter panel.
 

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
