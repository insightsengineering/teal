# Generate lockfile for application's environment reproducibility

Generate lockfile for application's environment reproducibility

## Usage

``` r
ui_teal_lockfile(id)

srv_teal_lockfile(id)

.teal_lockfile_process_invoke(lockfile_path)

.renv_snapshot(lockfile_path)

.is_lockfile_deps_installed()

.is_disabled_lockfile_scenario()
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- lockfile_path:

  (`character`) path to the lockfile.

## Value

`NULL`

## Different ways of creating lockfile

`teal` leverages
[`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html),
which offers multiple methods for lockfile creation.

- **Working directory lockfile**: `teal`, by default, will create an
  `implicit` type lockfile that uses
  [`renv::dependencies()`](https://rstudio.github.io/renv/reference/dependencies.html)
  to detect all R packages in the current project's working directory.

- **`DESCRIPTION`-based lockfile**: To generate a lockfile based on a
  `DESCRIPTION` file in your working directory, set
  `renv::settings$snapshot.type("explicit")`. The naming convention for
  `type` follows
  [`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html).
  For the `"explicit"` type, refer to
  `renv::settings$package.dependency.fields()` for the `DESCRIPTION`
  fields included in the lockfile.

- **Custom files-based lockfile**: To specify custom files as the basis
  for the lockfile, set `renv::settings$snapshot.type("custom")` and
  configure the `renv.snapshot.filter` option.

## lockfile usage

After creating the lockfile, you can restore the application's
environment using
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html).

## See also

[`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html),
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html).
