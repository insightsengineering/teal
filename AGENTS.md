# Teal Ecosystem R Package Development Guide

## Introduction

The teal framework uses shiny to create reproducible environments for analysts. The ecosystem comprises several interconnected packages with specific roles:

### Core Packages

- **teal** - The main framework package providing the application structure
- **teal.code** - Bare code generation and evaluation ensuring reproducibility
- **teal.data** - Data management and relationships between datasets
- **teal.reporter** - Report generation functionality
- **teal.slice** - Data filtering capabilities for application
- **teal.widgets** - Reusable UI components
- **teal.logger** - Standardized logging across the framework
- **teal.picks** - Data selection and merging utilities using `teal.data` objects
- **teal.transform** - Data transformation utilities (deprecated)

### Module Packages

- **teal.modules.general** (tmg) - General-purpose analysis modules
- **teal.modules.clinical** (tmc) - Clinical trial specific modules
- **teal.modules.hermes** - MultiAssayExperiment analysis modules
- **teal.goshawk** - Pharmacokinetics analysis modules
- **teal.osprey** - Advanced clinical analysis modules

### Supporting Packages

- **tern** - Statistical analysis functions
- **rtables** - Table creation and formatting
- **formatters** - Output formatting utilities
- **gtsummary** - Table creation and formatting

**Key Principle**: Balance dependency value with features. Minimize dependencies to packages not already in use within the ecosystem.

## Package Structure and Organization

### Standard Package Layout

Follow the standard R package structure with teal-specific conventions:

```text
package_name/
├── .github/workflows/     # CI/CD workflows (use r.pkg.template)
├── R/                     # R source code
├── tests/testthat/        # Unit tests
├── man/                   # Documentation
├── vignettes/             # Long-form documentation
├── inst/                  # Package assets
├── DESCRIPTION            # Package metadata
├── NAMESPACE              # Exports and imports
├── NEWS.md                # Change log
├── README.md              # Package overview
├── _pkgdown.yml          # Documentation website config
├── .lintr                # Linting configuration
└── .Rbuildignore         # Build exclusions
```

### Naming Conventions

- **Package names**: Use `teal.` prefix for ecosystem packages (e.g., `teal.widgets`)
- **Function names**: Use `snake_case` consistently
- **Class names**: Use `PascalCase` (e.g., `TealAppDriver`)
- **Module functions**: Prefix UI functions with `ui_` and server functions with `srv_`
- **Internal functions**: Use descriptive names without export

### File Organization

- **One main function per file** when the function is substantial
- **Group related utilities** in shared files (e.g., `utils.R`, `validations.R`)
- **Module files**: Use pattern `module_<name>.R` for shiny modules
- **Helper functions**: Prefix with the main function they support

## Code Style and Standards

### Code Quality

- **Run pre-commit hooks**: Always run `pre-commit run --all-files` before committing. Fix any issues it reports - the error messages are informative and will guide you.
- **Follow tidyverse style**: General R code style follows the tidyverse style guide.
- **Documentation**: All exported functions must have roxygen2 documentation. Run `devtools::document()` to update documentation.

### Formatting

Formatting rules are configured in the `.lintr` file.

### Teal-Specific Conventions

- **Naming**: Follow the naming conventions outlined in the Package Structure section

## Dependencies and Imports

### Dependency Management

- **Minimize dependencies**: Only add dependencies that provide significant value
- **Version constraints**: Specify minimum versions for critical dependencies
- **Ecosystem coherence**: Prefer packages already used within teal ecosystem

```r
# DESCRIPTION example
Depends:
    R (>= 4.1),
    shiny (>= 1.8.1)
Imports:
    checkmate (>= 2.1.0),
    rlang (>= 1.0.0),
    teal.widgets (>= 0.5.0)
Suggests:
    testthat (>= 3.2.0),
    knitr,
    rmarkdown
```

### Import Best Practices

```r
# In NAMESPACE, prefer specific imports over full package imports
#' @importFrom shiny moduleServer NS tagList
#' @importFrom checkmate assert_character assert_function
#' @import teal.data  # Only for core teal packages

# In code, use explicit namespacing for clarity when appropriate
checkmate::assert_string(label)
```

## Modules Development

### Module Architecture

Teal modules follow a specific pattern with UI and server components:

```r
# UI Function
ui_example_module <- function(id, var_x, var_y) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Input controls
    teal.widgets::standard_layout(
      # Output displays
      output = teal.widgets::white_small_well(
        shiny::tags$h4("Results"),
        shiny::plotOutput(ns("plot")),
        shiny::tags$h4("Summary data"),
        gt::gt_output(ns("table"))
      ),
      # Encoding panel
      encoding = shiny::tags$div(
        shiny::tags$label("Encodings", class = "text-primary"),
        shiny::tags$br(),
        shiny::tags$div(
          shiny::tags$strong("Select X-Axis Variable"),
          teal.picks::picks_ui(ns("var_x"), var_x)
        ),
        shiny::tags$div(
          shiny::tags$strong("Select Y-Axis Variable"),
          teal.picks::picks_ui(ns("var_y"), var_y)
        )
      )
    )
  )
}

# Server Function
srv_example_module <- function(id, data, var_x, var_y) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "reactive")

  shiny::moduleServer(id, function(input, output, session) {
    selectors <- teal.picks::picks_srv("picks", picks = list(var_x = var_x, var_y = var_y), data = data)
    merged <- teal.picks::merge_srv(
      "merge_picks",
      data = data,
      selectors = selectors,
      output_name = "anl",
      join_fun = "dplyr::inner_join"
    )
    # Data preparation
    validated_q <- shiny::reactive({
      shiny::validate(
        teal::need_input(
          inputId = "var_x-variables-selected",
          condition = length(selectors$var_x()$variables$selected) > 0,
          message = "X-Axis Variable must be selected"
        ),
        teal::need_input(
          inputId = "var_y-variables-selected",
          condition = length(selectors$var_y()$variables$selected) > 0,
          message = "Y-Axis Variable must be selected"
        )
      )
      shiny::validate(
        teal::need_input(
          inputId = c("var_x-variables-selected", "var_y-variables-selected"),
          condition = !any(selectors$var_x()$variables$selected %in% selectors$var_y()$variables$selected),
          message = "X-axis variable and Y-axis variable must be different"
        )
      )
      q <- merged$data()
      teal.reporter::teal_card(q) <- c(teal.reporter::teal_card(q), "## Module's output")
      q
    })

    # Generate plot inside qenv
    qenv <- reactive({
      within(validated_q(), {
        plot <- ggplot2::ggplot(anl) +
          ggplot2::geom_point(ggplot2::aes(x = env_var_x, y = env_var_y))
        plot

        table <- gtsummary::tbl_summary(anl, by = env_var_x, missing = "no")
        table
      }, env_var_x = as.name(merged$variables()$var_x), env_var_y = as.name(merged$variables()$var_y))
    })
    # Output rendering: use ggplot2 for visualizations
    output$plot <- shiny::renderPlot(qenv()[["plot"]])
    output$table <- gt::render_gt(expr = gtsummary::as_gt(qenv()[["table"]]))
     # Return reactive
    qenv
  })
}

tm_example_module <- function(label, var_x, var_y) {
  args <- list(var_x = var_x, var_y = var_y)
  teal::module(
    label = label,
    server = srv_example_module,
    ui = ui_example_module,
    ui_args = args[names(args) %in% names(formals(ui_example_module))],
    server_args = args[names(args) %in% names(formals(srv_example_module))]
  )
}
```

### Code Style for Modules

- **Use tidyverse style**: Write clear, readable code using dplyr, ggplot2 patterns and maggritr pipes
- **Prefer ggplot2**: For all visualizations over base R plotting
- **Use gt and gtsummary**: For statistical tables and summaries
- **Error handling**: Implement proper validation using `checkmate` and `shiny::validate(teal::need_input(...))`

```r
# Good: Clear data manipulation
plot_data <- data %>%
  dplyr::filter(!is.na(variable)) %>%
  dplyr::group_by(category) %>%
  dplyr::summarise(
    mean_value = mean(value),
    n = dplyr::n(),
    .groups = "drop"
  )

# Good: Descriptive ggplot2 code
ggplot2::ggplot(plot_data, ggplot2::aes(x = category, y = mean_value)) +
  ggplot2::geom_col(fill = "steelblue") +
  ggplot2::labs(
    title = "Mean Values by Category",
    x = "Category",
    y = "Mean Value"
  ) +
  ggplot2::theme_minimal()
```

## Testing Framework

### Testing Philosophy

- **Test public functions only**: Internal utilities should be tested through public interfaces
- **Precise, focused tests**: Each test should verify one specific behavior
- **High coverage**: Maintain at least 80% test coverage as measured by `covr`
- **Integration over units**: Test realistic usage patterns
- **Test Dependencies**.: Add `testthat::skip_if_not_installed(package_name)` only for dependencies in SUGGESTS or related to tests cases

### Test Structure

Follow the established patterns from `test-module_teal.R`:

```r
# Test organization
testthat::test_that("function_name works with valid inputs", {
  # Setup
  test_data <- data.frame(x = 1:10, y = rnorm(10))

  # Execution
  result <- function_name(test_data)

  # Verification - one expectation per test preferably
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("function_name handles edge cases", {
  # Test empty input
  testthat::expect_error(
    function_name(data.frame()),
    "Input data cannot be empty"
  )
})

testthat::test_that("function_name validates input types", {
  # Test invalid input type
  testthat::expect_error(
    function_name("not a data frame"),
    class = "checkmate_error"
  )
})
```

### Shiny Module Testing

- **Server functions**: Test with `shiny::testServer()`
- **UI functions**: Test basic usage with regular testing (class checks, error generation, snapshots, regexp search). Test UI scenarios and interactions with `TealAppDriver` (based on `shinytest2::AppDriver`) for integration testing
- **Reactive behavior**: Test reactive chains and side effects

```r
testthat::test_that("srv_my_module processes data correctly", {
  # Test server logic
  shiny::testServer(
    app = srv_my_module,
    args = list(
      data = reactive(test_data),
      filter_panel_api = NULL
    ),
    expr = {
      # Test reactive computations
      result <- processed_data()
      testthat::expect_s3_class(result, "teal_data")
    }
  )
})

testthat::test_that("my_module UI renders correctly", {
  # Integration test with TealAppDriver
  app <- init(
    data = teal_data(mtcars = mtcars),
    modules = my_module()
  )

  driver <- TealAppDriver$new(app)
  driver$navigate_teal_tab("My Module")

  # Test UI elements are present
  driver$expect_visible("#plot")

  driver$stop()
})
```

### Test Organization and Naming

- **One test file per R file**: `test-module_example.R` for `module_example.R`
- **Descriptive test names**: Clearly describe what is being tested
- **End to end test names**: `test-shinytest2-module_example.R` for `module_example.R`
- **Logical grouping**: Group related tests using `describe()` when beneficial
- **Test data**: Create minimal test datasets, avoid external dependencies

## Documentation and Communication

### Package Documentation

- **README.md**: Clear overview, installation, basic usage examples
- **Vignettes**: Comprehensive guides for complex functionality
- **Function documentation**: All exported functions must have roxygen2 documentation
- **NEWS.md**: Detailed changelog following semantic versioning

### Website Generation

Use `_pkgdown.yml` for documentation websites (`nesttemplate` is optional):

```yaml
url: https://insightsengineering.github.io/package.name

template:
  package: nesttemplate

reference:
  - title: "Main Functions"
    contents:
      - init
      - module
  - title: "Helper Functions"
    contents:
      - starts_with("validate_")
```

### Version Management

Do not change versions on your own.

## CI/CD and Development Workflow

### GitHub Workflows

Use `r.pkg.template` workflows for consistency:

- **check.yaml**: R CMD check, unit tests, coverage
- **docs.yaml**: Documentation building and deployment
- **audit.yaml**: Security and dependency auditing
- **pkgdown.yaml**: Website generation

### Pre-commit Hooks

**Always run pre-commit before committing code**:

```bash
pre-commit run --all-files
```

Fix any issues that pre-commit reports. The error messages are informative and will guide you on what needs to be fixed. Pre-commit automatically checks code style, documentation, linting, and other quality issues.

### Dependency Management with Staged Dependencies

`staged_dependencies.yaml` is an old artifact. Ignore it.

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

This guide ensures consistency, quality, and maintainability across the teal ecosystem while following R community best practices.
