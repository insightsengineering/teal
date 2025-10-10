# Copilot Instructions for teal

## Project Overview

`teal` is a Shiny-based interactive exploration framework for analyzing clinical trial data. It provides a modular architecture for building data analysis applications with dynamic filtering, data visualization, and reporting capabilities.

## Language and Framework

- **Primary Language**: R (>= 4.1)
- **Framework**: Shiny (>= 1.8.1)
- **Package Type**: R package following CRAN standards
- **Architecture**: Modular Shiny applications using `teal_module` and `teal_modules` objects

## Key Dependencies

- `teal.data` - for creating and loading data
- `teal.slice` - for filtering panel functionality
- `teal.reporter` - for generating reports
- `teal.widgets` - for Shiny components
- `teal.code` - for code tracking and reproducibility
- `bslib` - for modern UI components
- `shinyjs` - for JavaScript interactions

## Code Style and Standards

### R Code Style

- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Maximum line length: 120 characters (configured in `.lintr`)
- Use `lintr` for code quality checks
- Use roxygen2 comments for documentation
- Use lifecycle badges (`lifecycle::badge()`) for deprecation notices

### Documentation Requirements

- All exported functions must have roxygen2 documentation with `@param`, `@return`, and `@export` tags
- Include `@examples` or `@examplesShinylive` for public functions
- Use `@keywords internal` for internal functions
- Document S3 methods and classes appropriately

### Testing

- Use `testthat` (>= 3.2.0) for unit tests
- Use `shinytest2` for interactive Shiny module testing
- Test files are in `tests/testthat/` directory
- Follow naming convention: `test-*.R` for unit tests, `test-shinytest2-*.R` for Shiny tests
- Aim for comprehensive test coverage

## Development Workflow

### Building and Checking

- Run `R CMD check --as-cran` for CRAN-compliant checks
- Use `roxygen2` to generate documentation
- Environment variable `NOT_CRAN=true` enables additional tests
- The package uses `staged.dependencies` for managing cross-repository dependencies

### Linting

- Configuration in `.lintr` file
- Run linter checks before committing
- SuperLinter runs on pull requests

### Pre-commit Hooks

- Pre-commit hooks are configured in `.pre-commit-config.yaml`
- Recommended to use `pre-commit` tool with R hooks

## Architecture Patterns

### Teal Modules

- Modules are created using `module()` function with `label`, `server`, `ui`, and `datanames` parameters
- Server functions use `moduleServer(id, function(input, output, session) {...})`
- UI functions use `NS(id)` for proper namespacing
- The `datanames` parameter controls which datasets are shown in the module

### Data Handling

- Use `teal_data` objects for data management
- Use `teal_data_module()` for dynamic data loading
- Hidden datasets (prefixed with `.`) are not displayed in the application
- Support for CDISC data, independent datasets, and MultiAssayExperiment objects

### Filtering

- Filtering is managed via `teal_slices()` objects
- Module-specific filters can be configured
- Snapshot functionality for capturing/restoring filter states

### Reporting

- Reporting functionality via `teal.reporter::Reporter` objects
- Modules can implement report card generation
- Reporter can be disabled globally by setting `reporter = NULL`

## Common Patterns

### Creating a Teal Module

```r
my_module <- module(
  label = "Module Label",
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      # Module server logic
    })
  },
  ui = function(id) {
    ns <- NS(id)
    # Module UI
  },
  datanames = c("dataset1", "dataset2")
)
```

### Teal Application Initialization

```r
app <- init(
  data = teal_data(...),
  modules = modules(
    module(...),
    module(...)
  ),
  filter = teal_slices(...)
)
```

### Transform Modules (Decorators)

- Use `teal_transform_module()` for output transformations
- Can modify plot or table outputs from modules
- Use `make_teal_transform_server()` to reduce boilerplate

## File Organization

- **R/**: Source code files
  - Module files: `module_*.R`
  - Main entry point: `init.R`
  - Data structures: `teal_slices.R`, `teal_data_module.R`, etc.
  - Utilities: `utils.R`, `validate_inputs.R`, `validations.R`
- **tests/testthat/**: Test files
- **man/**: Generated documentation (auto-generated, do not edit manually)
- **vignettes/**: Package vignettes and tutorials
- **inst/**: Installation files including CSS/JS assets

## Important Notes

### Reserved Labels

- `"global_filters"` - reserved for the mapping argument in `teal_slices()`
- `"Report previewer"` - reserved for the report previewer module

### Deprecation

- Use `lifecycle::badge("deprecated")` in documentation
- Deprecated functions should include guidance on alternatives
- Follow semantic versioning for breaking changes

### Dependencies

- Minimize dependencies following [tinyverse](https://www.tinyverse.org/) recommendations
- Specify minimum versions in DESCRIPTION when compatibility requires it
- Format: `package (>= version)` or `package (>= version.9000)` for development versions

## Security

- Do not commit sensitive information (PII, PHI, PCI)
- Report security vulnerabilities to vulnerability.management[@]roche.com
- Do not use public GitHub issues for security concerns

## Contributing

- Branch naming: `<issue_id>_<short_description>` (use underscores)
- For multi-repo changes: `<issue_id>_<issue_repo>_<short_description>`
- Use `staged.dependencies` for cross-repository dependencies
- Minimum 5% code contribution or top 5 contributor status for author attribution

## Useful Resources

- [Project Documentation](https://insightsengineering.github.io/teal/)
- [CONTRIBUTING.md](.github/CONTRIBUTING.md) - Detailed contribution guidelines
- [CODE_OF_CONDUCT.md](.github/CODE_OF_CONDUCT.md) - Community standards
- [pharmaverse Slack](https://pharmaverse.slack.com) - `teal` channel for questions
