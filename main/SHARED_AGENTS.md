# NA

This package is part of the teal framework. The following configuration
applies to all packages within the teal framework.

## Package Structure and Organization

### Key Directories

Follow the standard R package structure with teal-specific conventions:

```
package_name/
├── .github           # CI/CD workflows
├── R/                # R source code
├── tests/testthat/   # Unit tests using testthat
├── vignettes/        # Long-form documentation
├── inst/             # Package assets
├── AGENTS.md         # Development guide for AI agents (this file)
├── DESCRIPTION       # Package metadata
├── NAMESPACE         # Exports and imports
├── NEWS.md           # Change log
├── README.md         # Package overview
├── _pkgdown.yml      # Documentation website config
├── .lintr            # Linting configuration
└── .Rbuildignore     # Build exclusions
```

### Naming Conventions

- **Function names**: Use `snake_case` consistently
- **Class names**: Use `PascalCase` (e.g., `TealAppDriver`)
- **Module functions**: Prefix UI functions with `ui_` and server
  functions with `srv_`
- **Internal functions**: Use descriptive names without export

### File Organization

- **One main function per file** when the function is substantial
- **Group related utilities** in shared files (e.g., `utils.R`,
  `validations.R`)
- **Module files**: Use pattern `tm_<name>.R` for teal modules
- **Helper functions**: Prefix with the main function they support

## Code Style and Standards

### Code Quality

- **Run `pre-commit` hooks**: Always run `pre-commit run --all-files`
  before committing, Fix any issues it reports - the error messages are
  informative and will guide you. It automatically checks code style,
  documentation and other quality issues. If pre-commit is not
  available, run the checks manually. Lint the R code manually as well
  if not called by pre-commit.
- **Follow `tidyverse` style**: General R code style follows the
  `tidyverse` style guide.
- **Documentation**: All exported functions must have `roxygen2`
  documentation with `@returns` and `@examples` fields.
- **Formatting** rules are configured in the `.lintr` file.

## Dependencies and Imports

### Dependency Management

- **Minimize dependencies**: Only add dependencies that provide
  significant value
- **Version constraints**: Specify minimum versions for critical
  dependencies
- **Ecosystem coherence**: Prefer packages already used within teal
  ecosystem

### Import Best Practices

Avoid importing package functions via roxygen2 (`#' @import pkg`)tags in
favor of explicit namespacing for clarity when appropriate. When needed
prefer specific imports over full package imports.

## Modules Development

### Module features

Each module should produce one or more Table, Listing, or Graph (TLG):

- **Reproducibility**: All code being executed to generate TLGs should
  be run using `teal_data` and
  [`within()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
  /
  [`teal.code::eval_code()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/eval_code.html)
  - At the end of the module this object should be returned to enable
    Reporter and “Show R code” functionalities
- **User Parameters**: Configurable inputs via
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  for flexible data selection
- **Transformators**: Optional pre-processing functions that derive
  variables and validate data before analysis
- **Decorators**: Optional post-processing functions that customize
  output presentation (titles, legends, annotations)

### Module Architecture

Teal modules follow a specific pattern with UI and server components:

\
`# UI Function`\
`ui_example_module`` ``<-`` ``function``(``id``, ``var_x``, ``var_y``, ``decorators``)`` ``{`\
`  ``ns`` ``<-`` ``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``)`\
`  ``select_decorators`` ``<-`` `[`getFromNamespace`](https://rdrr.io/r/utils/getFromNamespace.html)`(``"select_decorators"``, ``"teal"``)`` ``# import from teal internal functions`\
\
`  ``shiny``::`[`tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)`(`\
`    ``# Input controls`\
`    ``teal.widgets``::`[`standard_layout`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/standard_layout.html)`(`\
`      ``# Output displays`\
`      output ``=`` ``teal.widgets``::`[`white_small_well`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/white_small_well.html)`(`\
`        ``teal``::`[`ui_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(``"decorator_table"``, `[`select_decorators`](https://insightsengineering.github.io/teal/reference/select_decorators.md)`(``decorators``, ``"plot"``)``)``,`\
`        ``teal``::`[`ui_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(``"decorator_table"``, `[`select_decorators`](https://insightsengineering.github.io/teal/reference/select_decorators.md)`(``decorators``, ``"table"``)``)``,`\
`        ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``h4``(``"Results"``)``,`\
`        ``shiny``::`[`plotOutput`](https://rdrr.io/pkg/shiny/man/plotOutput.html)`(``ns``(``"plot"``)``)``,`\
`        ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``h4``(``"Summary data"``)``,`\
`        ``gt``::`[`gt_output`](https://gt.rstudio.com/reference/gt_output.html)`(``ns``(``"table"``)``)`\
`      ``)``,`\
`      ``# Encoding panel`\
`      encoding ``=`` ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``div``(`\
`        ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``label``(``"Encodings"``, class ``=`` ``"text-primary"``)``,`\
`        ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``br``(``)``,`\
`        ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``div``(`\
`          ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``strong``(``"Select X-Axis Variable"``)``,`\
`          ``teal.picks``::`[`picks_ui`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks_module.html)`(``ns``(``"var_x"``)``, ``var_x``)`\
`        ``)``,`\
`        ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``div``(`\
`          ``shiny``::`[`tags`](https://rstudio.github.io/htmltools/reference/builder.html)`$``strong``(``"Select Y-Axis Variable"``)``,`\
`          ``teal.picks``::`[`picks_ui`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks_module.html)`(``ns``(``"var_y"``)``, ``var_y``)`\
`        ``)`\
`      ``)`\
`    ``)`\
`  ``)`\
`}`\
\
`# Server Function`\
`srv_example_module`` ``<-`` ``function``(``id``, ``data``, ``var_x``, ``var_y``, ``decorators``)`` ``{`\
`  ``checkmate``::`[`assert_string`](https://mllg.github.io/checkmate/reference/checkString.html)`(``id``)`\
`  ``checkmate``::`[`assert_class`](https://mllg.github.io/checkmate/reference/checkClass.html)`(``data``, ``"reactive"``)`\
\
`  ``select_decorators`` ``<-`` `[`getFromNamespace`](https://rdrr.io/r/utils/getFromNamespace.html)`(``"select_decorators"``, ``"teal"``)`` ``# import from teal internal functions`\
`  ``shiny``::`[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)`(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`    ``selectors`` ``<-`` ``teal.picks``::`[`picks_srv`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks_module.html)`(``"picks"``, picks ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``var_x ``=`` ``var_x``, var_y ``=`` ``var_y``)``, data ``=`` ``data``)`\
`    ``merged`` ``<-`` ``teal.picks``::`[`merge_srv`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/merge_srv.html)`(`\
`      ``"merge_picks"``,`\
`      data ``=`` ``data``,`\
`      selectors ``=`` ``selectors``,`\
`      output_name ``=`` ``"anl"``,`\
`      join_fun ``=`` ``"dplyr::inner_join"`\
`    ``)`\
`    ``# Data preparation`\
`    ``validated_q`` ``<-`` ``shiny``::`[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`\
`      ``shiny``::`[`validate`](https://rdrr.io/pkg/shiny/man/validate.html)`(`\
`        ``teal``::`[`need_input`](https://insightsengineering.github.io/teal/reference/validate_input.md)`(`\
`          inputId ``=`` ``"var_x-variables-selected"``,`\
`          condition ``=`` `[`length`](https://rdrr.io/r/base/length.html)`(``selectors``$``var_x``(``)``$``variables``$``selected``)`` ``>`` ``0``,`\
`          message ``=`` ``"X-Axis Variable must be selected"`\
`        ``)``,`\
`        ``teal``::`[`need_input`](https://insightsengineering.github.io/teal/reference/validate_input.md)`(`\
`          inputId ``=`` ``"var_y-variables-selected"``,`\
`          condition ``=`` `[`length`](https://rdrr.io/r/base/length.html)`(``selectors``$``var_y``(``)``$``variables``$``selected``)`` ``>`` ``0``,`\
`          message ``=`` ``"Y-Axis Variable must be selected"`\
`        ``)`\
`      ``)`\
`      ``shiny``::`[`validate`](https://rdrr.io/pkg/shiny/man/validate.html)`(`\
`        ``teal``::`[`need_input`](https://insightsengineering.github.io/teal/reference/validate_input.md)`(`\
`          inputId ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"var_x-variables-selected"``, ``"var_y-variables-selected"``)``,`\
`          condition ``=`` ``!`[`any`](https://rdrr.io/r/base/any.html)`(``selectors``$``var_x``(``)``$``variables``$``selected`` `[`%in%`](https://rdrr.io/r/base/match.html)` ``selectors``$``var_y``(``)``$``variables``$``selected``)``,`\
`          message ``=`` ``"X-axis variable and Y-axis variable must be different"`\
`        ``)`\
`      ``)`\
`      ``q`` ``<-`` ``merged``$``data``(``)`\
`      ``teal.reporter``::`[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``q``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``teal.reporter``::`[`teal_card`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)`(``q``)``, ``"## Module's output"``)`\
`      ``q`\
`    ``}``)`\
\
`    ``# Generate plot inside qenv`\
`    ``qenv_plot`` ``<-`` ``reactive``(``{`\
`      `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(``validated_q``(``)``, ``{`\
`        ``plot`` ``<-`` ``ggplot2``::`[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``anl``)`` ``+`\
`          ``ggplot2``::`[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(``ggplot2``::`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``env_var_x``, y ``=`` ``env_var_y``)``)`\
`      ``}``, env_var_x ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``merged``$``variables``(``)``$``var_x``)``, env_var_y ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``merged``$``variables``(``)``$``var_y``)``)`\
`    ``}``)`\
`    ``decorated_plot`` ``<-`` ``teal``::`[`srv_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(`\
`      ``"decorator_table"``,`\
`      ``qenv_plot``,`\
`      `[`select_decorators`](https://insightsengineering.github.io/teal/reference/select_decorators.md)`(``decorators``, ``"plot"``)``,`\
`      expr ``=`` `[`quote`](https://rdrr.io/r/base/substitute.html)`(``plot``)`\
`    ``)`\
\
`    ``qenv_table`` ``<-`` ``reactive``(``{`\
`      `[`within`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)`(``validated_q``(``)``, ``{`\
`        ``table`` ``<-`` ``gtsummary``::`[`tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)`(``anl``, by ``=`` ``env_var_x``, missing ``=`` ``"no"``)`\
`      ``}``, env_var_x ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``merged``$``variables``(``)``$``var_x``)``, env_var_y ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``merged``$``variables``(``)``$``var_y``)``)`\
`    ``}``)`\
`    ``decorated_table`` ``<-`` ``teal``::`[`srv_transform_teal_data`](https://insightsengineering.github.io/teal/reference/module_transform_data.md)`(`\
`      ``"decorator_table"``,`\
`      ``qenv_table``,`\
`      `[`select_decorators`](https://insightsengineering.github.io/teal/reference/select_decorators.md)`(``decorators``, ``"table"``)``,`\
`      expr ``=`` `[`quote`](https://rdrr.io/r/base/substitute.html)`(``table``)`\
`    ``)`\
\
`    ``# Output rendering: use ggplot2 for visualizations`\
`    ``output``$``plot`` ``<-`` ``shiny``::`[`renderPlot`](https://rdrr.io/pkg/shiny/man/renderPlot.html)`(``decorated_plot``(``)``[[``"plot"``]``]``)`\
`    ``output``$``table`` ``<-`` ``gt``::`[`render_gt`](https://gt.rstudio.com/reference/render_gt.html)`(``expr ``=`` ``gtsummary``::`[`as_gt`](https://www.danieldsjoberg.com/gtsummary/reference/as_gt.html)`(``decorated_table``(``)``[[``"table"``]``]``)``)`\
`     ``# Return reactive`\
\
`    ``reactive``(`[`c`](https://rdrr.io/r/base/c.html)`(``decorated_plot``(``)``, ``decorated_table``(``)``)``)`\
`  ``}``)`\
`}`\
\
`tm_example_module`` ``<-`` ``function``(`\
`  ``label`` ``=`` ``"Example Module"``,`\
`  ``var_x`` ``=`` ``teal.picks``::`[`picks`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)`(``teal.picks``::`[`datasets`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)`(``)``, ``teal.picks``::`[`variables`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)`(``is.numeric``, selected ``=`` ``1L``)``)``,`\
`  ``var_y`` ``=`` ``teal.picks``::`[`picks`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)`(``teal.picks``::`[`datasets`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)`(``)``, ``teal.picks``::`[`variables`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)`(``is.numeric``, selected ``=`` ``2L``)``)``,`\
`  ``decorators`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``)``,`\
`  ``transformators`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`)`` ``{`\
`  ``checkmate``::`[`assert_string`](https://mllg.github.io/checkmate/reference/checkString.html)`(``label``)`\
`  ``checkmate``::`[`assert_class`](https://mllg.github.io/checkmate/reference/checkClass.html)`(``var_x``, ``"picks"``)`\
`  ``checkmate``::`[`assert_class`](https://mllg.github.io/checkmate/reference/checkClass.html)`(``var_y``, ``"picks"``)`\
`  ``checkmate``::`[`assert_list`](https://mllg.github.io/checkmate/reference/checkList.html)`(``transformators``, types ``=`` ``"teal_transform_module"``)`\
`  ``args`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``var_x ``=`` ``var_x``, var_y ``=`` ``var_y``, decorators ``=`` ``decorators``)`\
`  ``teal``::`[`module`](https://insightsengineering.github.io/teal/reference/teal_modules.md)`(`\
`    label ``=`` ``label``,`\
`    server ``=`` ``srv_example_module``,`\
`    ui ``=`` ``ui_example_module``,`\
`    ui_args ``=`` ``args``[`[`names`](https://rdrr.io/r/base/names.html)`(``args``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`names`](https://rdrr.io/r/base/names.html)`(`[`formals`](https://rdrr.io/r/base/formals.html)`(``ui_example_module``)``)``]``,`\
`    server_args ``=`` ``args``[`[`names`](https://rdrr.io/r/base/names.html)`(``args``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`names`](https://rdrr.io/r/base/names.html)`(`[`formals`](https://rdrr.io/r/base/formals.html)`(``srv_example_module``)``)``]``,`\
`    transformators ``=`` ``transformators`\
`  ``)`\
`}`

### Code Style for Modules

- **Use `tidyverse` style**: Write clear, readable code using `dplyr`,
  `ggplot2` patterns
- **Use `magrittr` pipes in reproducible execution**: For code executed
  for `teal_data`/`qenv` data objects with
  [`eval_code()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
  and
  [`within()`](https://insightsengineering.github.io/teal/reference/teal_data_module.md)
- **Use crane and gtsummary**: For statistical tables and summaries
- **Error handling**: Implement proper validation using `checkmate` and
  `shiny::validate(teal::need_input(...))`

\
`# Good: Clear data manipulation`\
`plot_data`` ``<-`` ``data`` ``%>%`\
`  ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``variable``)``)`` ``%>%`\
`  ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``category``)`` ``%>%`\
`  ``dplyr``::`[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(`\
`    mean_value ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``value``)``,`\
`    n ``=`` ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``,`\
`    .groups ``=`` ``"drop"`\
`  ``)`\
\
`# Good: Descriptive ggplot2 code`\
`ggplot2``::`[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``plot_data``, ``ggplot2``::`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``category``, y ``=`` ``mean_value``)``)`` ``+`\
`  ``ggplot2``::`[`geom_col`](https://ggplot2.tidyverse.org/reference/geom_bar.html)`(``fill ``=`` ``"steelblue"``)`` ``+`\
`  ``ggplot2``::`[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    title ``=`` ``"Mean Values by Category"``,`\
`    x ``=`` ``"Category"``,`\
`    y ``=`` ``"Mean Value"`\
`  ``)`` ``+`\
`  ``ggplot2``::`[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`

## Testing Framework

### Testing Philosophy

- **Test public functions only**: Internal utilities should be tested
  through public interfaces
- **Precise, focused tests**: Each test should verify one specific
  behavior
- **High coverage**: Maintain at least 80% test coverage as measured by
  `covr`
- **Integration over units**: Test realistic usage patterns
- **Test Dependencies**.: Add
  `testthat::skip_if_not_installed(package_name)` only for dependencies
  in `Suggests` or related to tests cases

### Shiny Module Testing

- **Server functions**: Test with
  [`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
- **UI functions**: Test basic usage with regular testing (class checks,
  error generation, snapshots, regexp search). Test UI scenarios and
  interactions with `teal:::TealAppDriver` (based on
  [`shinytest2::AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.html))
  for integration testing
- **Reactive behavior**: Test reactive chains and side effects

### Test Organization and Naming

- **One test file per R file**: `test-module_example.R` for
  `module_example.R`
- **Descriptive test names**: Clearly describe what is being tested
- **End to end test names**: `test-shinytest2-module_example.R` for
  `module_example.R`
- **Logical grouping**: Group related tests using `describe()` and
  individual tests with `it()` when beneficial
- **Test data**: Create minimal test datasets, avoid external
  dependencies

## Documentation and Communication

### Package Documentation

- **`README.md`**: Clear overview, installation, basic usage examples
- **Vignettes**: Comprehensive guides for complex functionality
- **Function documentation**: All exported functions must have
  `roxygen2` documentation
- **`NEWS.md`**: Detailed changelog of features, bugs and miscellanea
  changes affecting the users

### Package Version Management

Do not change versions on your own. There is a CI/CD workflow that
manages the versions automatically on the `main` branch.

## CI/CD and Development Workflow

Prefer to reuse templates from r.pkg.template. Main checks in place are:

- `check.yaml`: R CMD check, unit tests, coverage
- `docs.yaml`: Documentation building and deployment
- `audit.yaml`: Security and dependency auditing
- `pkgdown.yaml`: Website generation

## Quality Assurance

### Code Quality Metrics

- **Test Coverage**: ≥80% line coverage
- **Linting**: No lint violations using configured `.lintr`
- **Documentation**: 100% of exports documented

### Code Review Process

- **Pull Request Reviews**: All changes require human review and
  approval
- **Automated Checks**: CI must pass before merging
- **Breaking Changes**: Require special consideration and communication
- **Documentation Updates**: Must accompany functional changes

### Performance Considerations

- **Shiny Reactivity**: Minimize unnecessary reactive computations
- **Data Processing**: Use efficient data manipulation patterns
- **Memory Usage**: Consider memory implications for large datasets
- **Loading Time**: Optimize package loading and module initialization

## Maintenance Guidelines

- **Long-term Support**: Maintain backward compatibility when possible
- **Dependencies**: Minimal and justified dependencies only
- **Deprecation**: Use `lifecycle` package for function deprecation
