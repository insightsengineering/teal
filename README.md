# Teal: Interactive Exploratory Data Analysis with Shiny Web-Applications <a href='https://github.com/insightsengineering/teal'><img src="man/figures/teal.png" align="right" height="139" style="max-width: 100%;"/></a  >

*teal* is a shiny-based interactive exploration framework for analyzing clinical trials data. `teal` currently provides
a dynamic filtering facility and diverse data viewers. `teal` shiny applications are built using standard
[shiny modules](https://shiny.rstudio.com/articles/modules.html).

## Installationn

### Clone and install manually

#### 1. Clone the repository  

The repository can be downloaded directly from the `github.com` site as an archive
(see [Github tutorial on cloning to learn more](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository-from-github/cloning-a-repository)).
Alternatively, Git command line tools offer the same functionality, without the need for manual downloading and
unpacking the archive, but require to authenticate to Github. You can authenticate using a key pair or a Personal
Access Token (PAT). Please refer to excellent Github tutorials on
[connecting to Github using SSH](https://docs.github.com/en/github/authenticating-to-github) or
[creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token).
Following commands create subdirectory `teal` containing cloned repository.

1. Using PAT. Input in the Git Bash console, PowerShell or any Linux shell:

    ```sh
    git clone https://github.com/insightsengineering/teal.git
    Username: your_username_goes_here
    Password: your_token_goes_here
    ```

1. Using SSH. If set up properly, the repository is ready to be cloned executing:

    ```sh
    git clone https://github.com/insightsengineering/teal.git
    ```

#### 2. Build and install  

The native R tools provide a quick way to install a package. Run in PowerShell or any Linux shell:

  ```sh
  R CMD build teal
  ```

This command builds the package and creates an archive. The name of the archive is output by the command at then of
building. Then input in the shell:

```sh
Rscript -e 'install.packages("name_of_the_archive")
```

Here is an example of a real command (with name_of_the_archive substituted by the output of the build command):

```sh
Rscript -e 'install.packages("teal_0.9.5.9000.tar.gz")'
```

## Acknowledgment

We would like to thank everyone who made `teal` a better analysis environment. Special thanks go to:

* `Doug Kelkhoff` for his contributions to the styling of the filter panel.

## Notes for Developers

### Conventions

Shiny modules are implemented in files `<module>.R` with UI function `ui_<module>` and server function `srv_<module>`.

A module with a `id` should not use the id itself (`ns(character(0))`) as this id belongs to the parent module:

```r
ns <- NS(id)
ns(character(0)) # should not be used as parent module may be using it to show / hide this module
ns("whole") # is okay, as long as the input to ns is not character(0)
ns("") # empty string "" is allowed
```

HTML elements can be given CSS classes even if they are not used within this package to give the end-user the
possibility to modify the look-and-feel.

Here is a full example:

```r
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

```r
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

```r
.libPaths()

temp_install_dir <- tempfile()
dir.create(temp_install_dir)
.libPaths(c(temp_install_dir, .libPaths()))
.libPaths()

?init # look at doc
# restore old path once done
.libPaths(.libPaths()[-1])
.libPaths()
```

Add a summary at the top of each file to describe what it does.

Shiny modules should return `reactives` along with the observers they create. An example is here:

```r
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

This makes dynamic UI creation possible. If you want to remove a module added with `insertUI`,  you can do so by calling
`o$destroy()` on each of its observers, see the function `srv_filter_items` for an example.

The difference between `datanames` and `active_datanames` is that the latter is a subset of the former.

Note: Whenever you return input objects, it seems that you explicitly need to wrap them inside `reactive(..)`, e.g.
`reactive(input$name)`.

Modules should respect the argument order: `function(input, output, session, datasets, ...)`, where `...` can also
include further named arguments.

The idiom `shinyApp(ui, server) %>% invisible()` is used with internal Shiny modules that are not exported. Printing a
`shinyApp` causes it to call `runApp`, so never print a `shinyApp`. Instead do:

```r
#' app <- shinyApp(...)
#' \dontrun{
#' runApp(app)
#' }
```

Avoid overloading return statements. For example, refactor:

```r
return(list(
  # must be a list and not atomic vector, otherwise jsonlite::toJSON gives a warning
  data_md5sums = setNames(
    lapply(self$datanames(), self$get_data_attr, "md5sum"),
    self$datanames()
  ),
  filter_states = reactiveValuesToList(private$filter_states),
  preproc_code = self$get_preproc_code()
))
```

Into the more easily debuggable form:

```r
res <- list(
  # must be a list and not atomic vector, otherwise jsonlite::toJSON gives a warning
  data_md5sums = setNames(
    lapply(self$datanames(), self$get_data_attr, "md5sum"),
    self$datanames()
  ),
  filter_states = reactiveValuesToList(private$filter_states),
  preproc_code = self$get_preproc_code()
)
res # put it to not be invisible
# or (slightly discouraged)
#return(res)
```

Note that for functions to be dispatched with S3, they need to be exported. This will not actually export the function
with the class specifier, but only make it available for S3 method dispatch, see
[stack-overflow](https://stackoverflow.com/questions/18512528/how-to-export-s3-method-so-it-is-available-in-namespace)

While working on a PR, you can add a `scratch` directory to keep scripts to test the code that were not integrated into
vignettes or examples yet. Before the PR is merged, remove this directory again. To avoid forgetting this, add a `todo`
comment in the code. The `scratch` folder is also in `.Rbuildignore`.

For `FilteredData`, when you work on a new module that directly changes it, you can check internal consistency in your
reactive expressions through code like:

```r
datasets$.__enclos_env__$private$validate()
```

#### `system.file`

We recommend against exporting functions that use `system.file` to access files in other packages as this breaks
encapsulation and leads to issues with `devtools` as explained below.

`base::system.file` is only overwritten by `pkgload:::shim_system.file` if the package is loaded by
 `devtools::load_all()`. This is done to access files transparently, independent of whether a package was loaded using
  `library(pkg)` or `devtools::load(pkg)`. The `inst` folder is in different locations for each of these cases.
If a package uses `system.file` without being loaded via `devtools`, it will fail when it tries to locate something in
another package that is loaded with `devtools` because it still uses `base::system.file`. The solution is to explicitly
use `pkgload:::system.file` whenever providing a function that relies on `system.file` from another package. Or not do
it at all (this is the current approach we take with `include_css_files`). We don't directly overwrite `system.file`,
this may have unexpected behavior.
