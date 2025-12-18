# Filter settings for `teal` applications

Specify initial filter states and filtering settings for a `teal` app.

## Usage

``` r
teal_slices(
  ...,
  exclude_varnames = NULL,
  include_varnames = NULL,
  count_type = NULL,
  allow_add = TRUE,
  module_specific = FALSE,
  mapping,
  app_id = NULL
)

as.teal_slices(x)

# S3 method for class 'teal_slices'
c(...)
```

## Arguments

- ...:

  any number of `teal_slice` objects.

- include_varnames, exclude_varnames:

  (`named list`s of `character`) where list names match names of data
  sets and vector elements match variable names in respective data sets;
  specify which variables are allowed to be filtered; see `Details`.

- count_type:

  *This is a new feature. Do kindly share your opinions on
  [`teal.slice`'s GitHub
  repository](https://github.com/insightsengineering/teal.slice/).*

  (`character(1)`) string specifying how observations are tallied by
  these filter states. Possible options:

  - `"none"` (default) to have counts of single `FilterState` to show
    unfiltered number only.

  - `"all"` to have counts of single `FilterState` to show number of
    observation in filtered and unfiltered dataset. Note, that issues
    were reported when using this option with `MultiAssayExperiment`.
    Please make sure that adding new filters doesn't fail on target
    platform before deploying for production.

- allow_add:

  (`logical(1)`) logical flag specifying whether the user will be able
  to add new filters

- module_specific:

  (`logical(1)`) optional,

  - `FALSE` (default) when one filter panel applied to all modules. All
    filters will be shared by all modules.

  - `TRUE` when filter panel module-specific. Modules can have different
    set of filters specified - see `mapping` argument.

- mapping:

  **\[experimental\]** *This is a new feature. Do kindly share your
  opinions on [`teal`'s GitHub
  repository](https://github.com/insightsengineering/teal/).*

  (named `list`) specifies which filters will be active in which modules
  on app start. Elements should contain character vector of `teal_slice`
  `id`s (see
  [`teal.slice::teal_slice`](https://insightsengineering.github.io/teal.slice/latest-tag/reference/teal_slice.html)).
  Names of the list should correspond to `teal_module` `label` set in
  [`module()`](https://insightsengineering.github.io/teal/reference/teal_modules.md)
  function.

  - `id`s listed under `"global_filters` will be active in all modules.

  - If missing, all filters will be applied to all modules.

  - If empty list, all filters will be available to all modules but will
    start inactive.

  - If `module_specific` is `FALSE`, only `global_filters` will be
    active on start.

- app_id:

  (`character(1)`) For internal use only, do not set manually. Added by
  `init` so that a `teal_slices` can be matched to the app in which it
  was used. Used for verifying snapshots uploaded from file. See
  `snapshot`.

- x:

  (`list`) of lists to convert to `teal_slices`

## Value

A `teal_slices` object.

## Details

Produces a `teal_slices` object. The `teal_slice` components will
specify filter states that will be active when the app starts.
Attributes (created with the named arguments) will configure the way the
app applies filters. See argument descriptions for details.

## See also

[`teal.slice::teal_slices`](https://insightsengineering.github.io/teal.slice/latest-tag/reference/teal_slices.html),
[`teal.slice::teal_slice`](https://insightsengineering.github.io/teal.slice/latest-tag/reference/teal_slice.html),
[`slices_store()`](https://insightsengineering.github.io/teal/reference/slices_store.md)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIFLa1cozmKu1APoBnOgTiP2A6dLtOXcdgAmUKRQ0PDSALzSfGC0jLSOMbjSEixhMlExAMqocAS0bknStAGR0WCOufmFYDy4nt5w3L60roHBobAZ5XEJRamM6WXZcKjcGAAyFADmpAAWRSXDFaPN1DPzMXUNPs6t-g1eQSFDmbHxiXjFpWfUJNP2uSHUl8nipOvLE-fST9yv0jgAA9UNYzgAFODPSYbObSAB80gArDEGtsIF5dn52icussYKQCCwAQNTuUYKhposbuTCcT7BSqbV6hjpDA0KghNMynRHKQPKyvDAiAEVOsAIxlAgeCpVAoAmKOVYOdYQWYLZmHNkisVwABMUplBKJjEcDMpWxZXi80zu9Ga5ksIkcyzuaseUP+qNZ-AgvoEHJsxUE-IaxygZR84fYvRdUVjyWNxLKSdN6KFOvWcekvNDgu1ovWRszcHFlq1wsLBzAld1eq2aKtygsVjKjqsAn9giU0hjwlE4ik2h0IAajjmQlYAEF0OwOQASFS0ZILpWMKSMX0AXwEYC3AF0gA)

## Examples

``` r
filter <- teal_slices(
  teal_slice(dataname = "iris", varname = "Species", id = "species"),
  teal_slice(dataname = "iris", varname = "Sepal.Length", id = "sepal_length"),
  teal_slice(
    dataname = "iris", id = "long_petals", title = "Long petals", expr = "Petal.Length > 5"
  ),
  teal_slice(dataname = "mtcars", varname = "mpg", id = "mtcars_mpg"),
  mapping = list(
    module1 = c("species", "sepal_length"),
    module2 = c("mtcars_mpg"),
    global_filters = "long_petals"
  )
)

app <- init(
  data = teal_data(iris = iris, mtcars = mtcars),
  modules = list(
    module("module1"),
    module("module2")
  ),
  filter = filter
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
