
# teal R Package Development Guide

## Package Overview

`teal` is the main package of the framework to provide shiny web apps
for analyzing clinical trials data. It provides a unified user interface
and features for the different modules that analyze the data.

The most important functions are:

- `init`: Creates a teal application.
- `modules`: Combines analyses implemented as Shiny or teal modules.
- `teal_slices`: Filters datasets.

Other important helpers:

- `example_module`: To show and test framework features.
- `modify_*` and `disable_*` functions to alter the UI of the app and
  remove some features respectively.
- `*_decorators`: Modules can be extended with decorators that modify
  the plots, tables and listings; these helpers make it easier to build
  custom modules with decorators functionality
- `validate*`: Functions to validate user’s input or objects used in
  modules.

## Development Context

The main feature of this package is to generate `teal` apps and
integrate their features for the users.

### Direct dependencies

- **shiny** - Web app generation and isolating framework.
- **teal.code** - Bare code generation and evaluation ensuring
  reproducibility
- **teal.data** - Data management and relationships between datasets via
  join_keys (contains sample data for ADaM datasets and default keys to
  merge ADaM datasets)
- **teal.reporter** - Report generation functionality
- **teal.slice** - Data filtering capabilities for application
- **teal.widgets** - Reusable UI components for user input and output
- **teal.logger** - Standardized logging across the framework

### Supporting Packages

The `teal` package is used by module packages in examples and functions:

- **teal.modules.general** (tmg) - General-purpose analysis modules
- **teal.modules.clinical** (tmc) - Clinical trial specific modules
- **teal.modules.hermes** - analysis modules using MultiAssayExperiment
  from Bioconductor.
- **teal.goshawk** - Pharmacokinetics analysis modules
- **teal.osprey** - Advanced clinical analysis modules

In addition to help the analysis these two packages might be used on
modules:

- **teal.picks** - Data selection and merging utilities using
  `teal.data` objects
- **teal.transform** - Data transformation utilities (deprecated in
  favor of teal.picks)

## Workflows

- Before fixing an issue consider that this package is part of teal
  framework, so a bug that manifests in this package might have a
  different origin.
- Before changing code in the `init()` function, consider the
  compatibility of this change with the module packages.
- The internal `TealAppDriver` object is used for end to end test the
  modules packages. If any change is done check the compatibility with
  the module packages.
- If something is needed for more than one module it might be needed on
  this package to be exported to all of them or in one of the
  dependencies.
- Before making a change to an API exported in `teal`, please ensure
  that it will be backwards compatible
- Use the pattern `module_<name>.R` for shiny modules.
- Prefix helper functions with the main function they support
- Group related utilities in shared files (e.g., `utils.R`,
  `validations.R`).

# Teal Framework Agents Instructions

This package is part of the teal framework. The following configuration
applies to all packages within the teal framework.

## Package Structure and Organization

### Key Directories

Follow the standard R package structure with teal-specific conventions:

``` text
package_name/
├── .gitlab-ci.yml    # CI/CD workflows (if package hosted in Gitlab)
├── .github           # CI/CD workflows (if package hosted in GitHub)
├── R/                # R source code
├── tests/testthat/   # Unit tests using testthat
├── vignettes/        # Long-form documentation
├── inst/             # Package assets
├── AGENTS.md         # Development guide for AI agents (this file)
├── DESCRIPTION       # Package metadata
├── NAMESPACE         # Exports and imports automa
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

### Code Style for Modules

- **Use `tidyverse` style**: Write clear, readable code using `dplyr`,
  `ggplot2` patterns
- **Use `magrittr` pipes in reproducible execution**: For code executed
  for `teal_data`/`qenv` data objects with `eval_code()` and `within()`
- **Use crane and gtsummary**: For statistical tables and summaries
- **Error handling**: Implement proper validation using `checkmate` and
  `shiny::validate(teal::need_input(...))`

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

- **Server functions**: Test with `shiny::testServer()`
- **UI functions**: Test basic usage with regular testing (class checks,
  error generation, snapshots, regexp search). Test UI scenarios and
  interactions with `teal:::TealAppDriver` (based on
  `shinytest2::AppDriver`) for integration testing
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

### Gitlab Workflows (if package hosted in Gitlab)

`.gitlab-ci.yml` reuses CI/CD tasks, such as running all unit tests,
`R CMD check`, code quality checks, style checks and website generation.

### GitHub Workflows (if package hosted in GitHub)

Use r.pkg.template workflows for consistency:

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
