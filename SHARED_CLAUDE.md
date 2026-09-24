This package is part of the teal framework. The following configuration applies to all packages within the teal framework:

## Package Structure and Organization

### Key Directories

Follow the standard R package structure with teal-specific conventions:

```text
package_name/
├── .gitlab-ci.yml    # CI/CD workflows
├── R/                # R source code
├── tests/testthat/   # Unit tests using testthat
├── vignettes/        # Long-form documentation
├── inst/             # Package assets
├── CLAUDE.md         # Development guide for AI agents (this file)
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
- **Module functions**: Prefix UI functions with `ui_` and server functions with `srv_`
- **Internal functions**: Use descriptive names without export

### File Organization

- **One main function per file** when the function is substantial
- **Group related utilities** in shared files (e.g., `utils.R`, `validations.R`)
- **Module files**: Use pattern `tm_<name>.R` for teal modules
- **Helper functions**: Prefix with the main function they support

## Code Style and Standards

### Code Quality

- **Run `pre-commit` hooks**: Always run `pre-commit run --all-files` before committing. Fix any issues it reports - the error messages are informative and will guide you. It automatically checks code style, documentation, linting, and other quality issues.
- **Follow `tidyverse` style**: General R code style follows the `tidyverse` style guide.
- **Documentation**: All exported functions must have `roxygen2` documentation. Run `devtools::document()` to update documentation.
- **Formatting** rules are configured in the `.lintr` file.

## Dependencies and Imports

### Dependency Management

- **Minimize dependencies**: Only add dependencies that provide significant value
- **Version constraints**: Specify minimum versions for critical dependencies
- **Ecosystem coherence**: Prefer packages already used within teal ecosystem

### Import Best Practices

Avoid importing package functions via roxygen2 (`#' @import pkg`)tags in favor of explicit namespacing for clarity when appropriate.
When needed prefer specific imports over full package imports.

### Code Style for Modules

- **Use `tidyverse` style**: Write clear, readable code using `dplyr`, `ggplot2` patterns
- **Use `magrittr` pipes in reproducible execution**: For code executed for `teal_data`/`qenv` data objects with `eval_code()` and `within()`
- **Use crane and gtsummary**: For statistical tables and summaries
- **Error handling**: Implement proper validation using `checkmate` and `shiny::validate(teal::need_input(...))`

## Testing Framework

### Testing Philosophy

- **Test public functions only**: Internal utilities should be tested through public interfaces
- **Precise, focused tests**: Each test should verify one specific behavior
- **High coverage**: Maintain at least 80% test coverage as measured by `covr`
- **Integration over units**: Test realistic usage patterns
- **Test Dependencies**.: Add `testthat::skip_if_not_installed(package_name)` only for dependencies in SUGGESTS or related to tests cases

### Shiny Module Testing

- **Server functions**: Test with `shiny::testServer()`
- **UI functions**: Test basic usage with regular testing (class checks, error generation, snapshots, regexp search). Test UI scenarios and interactions with `teal::TealAppDriver` (based on `shinytest2::AppDriver`) for integration testing
- **Reactive behavior**: Test reactive chains and side effects

### Test Organization and Naming

- **One test file per R file**: `test-module_example.R` for `module_example.R`
- **Descriptive test names**: Clearly describe what is being tested
- **End to end test names**: `test-shinytest2-module_example.R` for `module_example.R`
- **Logical grouping**: Group related tests using `describe()` when beneficial
- **Test data**: Create minimal test datasets, avoid external dependencies

## Documentation and Communication

### Package Documentation

- **`README.md`**: Clear overview, installation, basic usage examples
- **Vignettes**: Comprehensive guides for complex functionality
- **Function documentation**: All exported functions must have `roxygen2` documentation
- **`NEWS.md`**: Detailed changelog following semantic versioning

### Package Version Management

Do not change versions on your own.
There is a CI/CD workflow that manages the versions automatically on the `main` branch.

## CI/CD and Development Workflow

### Gitlab Workflows

`.gitlab-ci.yml` reuses CI/CD tasks, such as running all unit tests, `R CMD check`, code quality checks, style checks and website generation.

### GitHub Workflows

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
- **Dependencies**: Minimal and justified dependencies only

### Code Review Process

- **Pull Request Reviews**: All changes require review
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
- **Deprecation**: Use `lifecycle` package for function deprecation
