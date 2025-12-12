Introduction

The teal framework uses shiny to create reproducible environment for analyst. It uses several packages: teal.widgets, teal.logger, teal.transform, teal.slice, teal.code, teal.data.
The key package is teal
Packages that create modules for users are teal.modules.general and teal.modules.clinical.
Balance dependencies value and features.

Modules

Should create simple code using tidyverse style and be simple.
Use tern and rtables related packages to create tables
Use ggplot2 for plots. 
teal.modules.clinical (sometimes abbreviated as tmc) and teal.modules.general (abbreviated as tmg) have too many dependencies: do not depend something that is not already in use.

Testing

- only public functions are tested.
- utilities should be tested via public methods.
  For example look at test-module_teal.R how snapshot manager is tested, filter panel, teal data module.
- Short tests with precise information - please have a look at test-module_teal.R to follow a convention you found there
- Each tests should be encapsulated in single test_that()/it(), ideally one expectation per test_that/it unless test-description says otherwise.
- Test server functionality with `shiny::testServer`
- Test UI modules only via `TealAppDriver`
- Test coverage should be at least 80% as measured by covr
