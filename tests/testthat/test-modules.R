dataset_1 <- TealDataset$new("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl_dataset <- CDISCTealDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))

call_module_server_fun <- function(input, output, session, datasets) {
}

module_server_fun <- function(id, datasets) {
}

ui_fun1 <- function(id, ...) {
  tags$p(paste0("id: ", id))
}

ui_fun2 <- function(id, datasets) {
  tags$p(paste0("id: ", id))
}

testthat::test_that("module requires label argument to be a string", {
  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  ), NA)

  testthat::expect_error(module(
    label = NULL,
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  ), "Assertion on 'label' failed.+'NULL'")

  testthat::expect_error(module(
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  ), "is missing, with no default")

  testthat::expect_error(module(
    label = c("label", "label"),
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  ), "Assertion on 'label' failed: Must have length 1.")

  testthat::expect_error(module(
    label = 1L,
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  ), "Assertion on 'label' failed.+not 'integer'")
})

testthat::test_that("module expects server being a shiny server module with datasets argument", {
  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  ), NA)
  testthat::expect_error(module(
    label = "label",
    server = function(id, datasets) NULL,
    ui = ui_fun1,
    filters = ""
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = function(input, output, session, ...) NULL,
    ui = ui_fun1,
    filter = ""
  ), "module\\(\\) server.+input, output, session, and datasets \\(callModule\\)")

  testthat::expect_error(module(
    label = "label",
    server = ui_fun1,
    ui = ui_fun1,
    filter = ""
  ), "module\\(\\) server.+id and datasets \\(moduleServer\\)")
})

testthat::test_that("module expects ui being a shiny ui module with id and datasets or ... arguments", {
  testthat::expect_error(module(
    label = "label",
    server = function(id, datasets) NULL,
    ui = function(id, datasets) NULL,
    filters = ""
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = function(id, datasets) NULL,
    ui = ui_fun1,
    filters = ""
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = function(id, datasets) NULL,
    ui = function(id, datasets, ...) NULL,
    filters = ""
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = function(id, datasets) NULL,
    ui = function(id, ..., datasets) NULL,
    filters = ""
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = function(id, datasets) NULL,
    ui = function() NULL,
    filters = ""
  ), "module\\(\\) ui argument requires a function with two ordered arguments:\n- 'id'\n- 'datasets' or '...'")

  testthat::expect_error(module(
    label = "label",
    server = function(id, datasets) NULL,
    ui = function(notid) NULL,
    filters = ""
  ), "module\\(\\) ui argument requires a function with two ordered arguments:\n- 'id'\n- 'datasets' or '...'")

  testthat::expect_error(module(
    label = "label",
    server = function(id, datasets) NULL,
    ui = function(id, notvalid) NULL,
    filters = ""
  ), "module\\(\\) ui argument requires a function with two ordered arguments:\n- 'id'\n- 'datasets' or '...'")
})

testthat::test_that("module requires filters argument to be a character", {
  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "all"
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = NULL
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = NA_character_
  ), "Contains missing values")
})

testthat::test_that("module requires server_args argument to be a list", {
  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    server_args = list(a = 1)
  ), NA)
  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    server_args = list()
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    server_args = NULL
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    server_args = ""
  ), "Assertion on 'server_args' failed.+'list'")

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    server_args = list(1, 2, 3)
  ), NA)
})

testthat::test_that("module requires ui_args argument to be a list", {
  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    ui_args = list(a = 1)
  ), NA)
  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    ui_args = list()
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    ui_args = NULL
  ), NA)

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    ui_args = ""
  ), "Assertion on 'ui_args' failed.+'list'")

  testthat::expect_error(module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "",
    ui_args = list(1, 2, 3)
  ), NA)
})

testthat::test_that("module() returns list of class 'teal_module' containing input objects", {
  test_module <- module(
    label = "aaa1",
    server = call_module_server_fun,
    ui = ui_fun1,
    filters = "all",
    server_args = NULL,
    ui_args = NULL
  )
  testthat::expect_s3_class(test_module, "teal_module")
  testthat::expect_named(test_module, c("label", "server", "ui", "filters", "server_args", "ui_args"))
  testthat::expect_identical(test_module$label, "aaa1")
  testthat::expect_identical(test_module$server, call_module_server_fun)
  testthat::expect_identical(test_module$ui, ui_fun1)
  testthat::expect_identical(test_module$filters, "all")
  testthat::expect_identical(test_module$server_args, NULL)
  testthat::expect_identical(test_module$ui_args, NULL)
})

testthat::test_that("modules requires label argument to be a string ", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )

  testthat::expect_error(modules(label = "label", test_module), NA)
  testthat::expect_error(modules(label = NULL, test_module), "Assertion on 'label' failed.+'NULL'")
  testthat::expect_error(
    modules(label = c("label", "label"), test_module),
    "Assertion on 'label' failed: Must have length 1"
  )
})

testthat::test_that("modules accept teal_module in ...", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )

  testthat::expect_error(modules(label = "label", test_module), NA)
})

testthat::test_that("modules accept multiple teal_module objects in ...", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )

  testthat::expect_error(modules(label = "label", test_module, test_module), NA)
})

testthat::test_that("modules accept multiple teal_module and teal_modules objects in ...", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )
  test_modules <- modules("label", test_module)

  testthat::expect_error(modules(label = "label", test_module, test_modules), NA)
})

testthat::test_that("modules does not accept objects other than teal_module(s) in ...", {
  testthat::expect_error(
    modules(label = "label", "a"),
    "not all arguments are of class teal_module or teal_modules"
  )
})

testthat::test_that("modules returns teal_modules object with label and children slot", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )
  out <- modules("label2", test_module)
  testthat::expect_s3_class(out, "teal_modules")
  testthat::expect_named(out, c("label", "children"))
})

testthat::test_that("modules returns children as list with list named after label attributes", {
  test_module <- module(
    label = "module",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )
  test_modules <- modules("modules", test_module)
  out <- modules("tabs", test_module, test_modules)$children
  testthat::expect_named(out, c("module", "modules"))
  testthat::expect_identical(out$module, test_module)
  testthat::expect_identical(out$modules, test_modules)
})

testthat::test_that("modules returns children as list with unique names if labels are duplicated", {
  test_module <- module(
    label = "module",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )
  test_modules <- modules("module", test_module)
  out <- modules("tabs", test_module, test_modules)$children
  testthat::expect_named(out, c("module", "module_1"))
  testthat::expect_identical(out$module, test_module)
  testthat::expect_identical(out$module_1, test_modules)
})


testthat::test_that("root_modules needs at least one argument", {
  expect_error(root_modules(), "You must provide at least one module.")
})

testthat::test_that("root_modules returns teal_modules object with label='root'", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )
  out <- root_modules(test_module)
  testthat::expect_s3_class(out, "teal_modules")
  testthat::expect_named(out, c("label", "children"))
  testthat::expect_identical(out$label, "root")
})
