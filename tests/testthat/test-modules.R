call_module_server_fun <- function(input, output, session, data) {
}

module_server_fun <- function(id, data) {
}

ui_fun1 <- function(id, ...) {
  tags$p(paste0("id: ", id))
}

testthat::test_that("Calling module() does not throw", {
  testthat::expect_no_error(suppressMessages(module()))
})

testthat::test_that("module requires label argument to be a string different than 'global_filters'", {
  testthat::expect_no_error(module(label = "label"))

  testthat::expect_error(module(label = NULL), "Assertion on 'label' failed.+'NULL'")

  testthat::expect_error(module(label = c("label", "label")), "Assertion on 'label' failed: Must have length 1.")

  testthat::expect_error(module(label = 1L), "Assertion on 'label' failed.+not 'integer'")

  testthat::expect_error(module(label = "global_filters"), "is reserved in teal")
})

testthat::test_that("module warns when server contains datasets argument", {
  testthat::expect_warning(
    module(server = function(id, datasets) NULL),
    "`datasets` argument in the server is deprecated"
  )
})

testthat::test_that("module expects server being a shiny server module with any argument", {
  testthat::expect_no_error(module(server = function(id) NULL))

  testthat::expect_no_error(module(server = function(id, any_argument) NULL))

  testthat::expect_no_error(module(server = function(input, output, session, any_argument) NULL))


  testthat::expect_error(
    module(server = function(input, output) NULL),
    "`server` argument requires a function with following arguments"
  )

  testthat::expect_error(
    module(server = function(any_argument) NULL),
    "`server` argument requires a function with following arguments"
  )
})

testthat::test_that("module requires server_args argument to be a list", {
  testthat::expect_no_error(module(server = function(id, a) NULL, server_args = list(a = 1)))
  testthat::expect_no_error(module(server_args = list()))
  testthat::expect_no_error(module(server_args = NULL))
  testthat::expect_error(module(server_args = ""), "Assertion on 'server_args' failed.+'list'")
  testthat::expect_error(module(server_args = list(1, 2, 3)), "Must have names")
})

testthat::test_that("module expects all server_args being a server arguments or passed through `...`", {
  testthat::expect_no_error(module(server = function(id, arg1) NULL, server_args = list(arg1 = NULL)))

  testthat::expect_no_error(module(server = function(id, ...) NULL, server_args = list(arg1 = NULL)))

  testthat::expect_error(
    module(server = function(id) NULL, server_args = list(arg1 = NULL)),
    "Following `server_args` elements have no equivalent in the formals of the server"
  )
})

testthat::test_that("module requires ui_args argument to be a list", {
  testthat::expect_no_error(module(ui = function(id, a) NULL, ui_args = list(a = 1)))
  testthat::expect_no_error(module(ui_args = list()))
  testthat::expect_no_error(module(ui_args = NULL))
  testthat::expect_error(module(ui_args = ""), "Assertion on 'ui_args' failed.+'list'")
  testthat::expect_error(module(ui_args = list(1, 2, 3)), "Must have names")
})

testthat::test_that("module throws when ui has data or datasets argument", {
  testthat::expect_error(module(ui = function(id, data) NULL))
  testthat::expect_error(module(ui = function(id, datasets) NULL))
})

testthat::test_that("module expects ui being a shiny ui module with any argument", {
  testthat::expect_no_error(module(ui = function(id) NULL))
  testthat::expect_no_error(module(ui = function(id, any_argument) NULL))
  testthat::expect_error(
    module(ui = function(any_argument) NULL),
    "`ui` argument requires a function with following arguments"
  )
})

testthat::test_that("module expects all ui_args being a ui arguments or passed through `...`", {
  testthat::expect_no_error(module(ui = function(id, arg1) NULL, ui_args = list(arg1 = NULL)))

  testthat::expect_no_error(module(ui = function(id, ...) NULL, ui_args = list(arg1 = NULL)))

  testthat::expect_error(
    module(ui = function(id) NULL, ui_args = list(arg1 = NULL)),
    "Following `ui_args` elements have no equivalent in the formals of UI"
  )
})

testthat::test_that("module requires datanames argument to be a character or NULL", {
  testthat::expect_no_error(module(datanames = "all"))
  testthat::expect_no_error(module(datanames = ""))
  testthat::expect_no_error(module(datanames = NULL))
  testthat::expect_error(module(server = function(id, data) NULL, datanames = NA_character_), "Contains missing values")
  testthat::expect_no_error(module(server = function(id, data) NULL, datanames = NULL))
})

testthat::test_that("module() returns list of class 'teal_module' containing input objects", {
  test_module <- module(
    label = "aaa1",
    server = call_module_server_fun,
    ui = ui_fun1,
    datanames = "all",
    server_args = NULL,
    ui_args = NULL
  )
  testthat::expect_s3_class(test_module, "teal_module")
  testthat::expect_named(
    test_module,
    c("label", "server", "ui", "datanames", "server_args", "ui_args", "transformators", "path")
  )
  testthat::expect_identical(test_module$label, "aaa1")
  testthat::expect_identical(test_module$server, call_module_server_fun)
  testthat::expect_identical(test_module$ui, ui_fun1)
  testthat::expect_identical(test_module$datanames, "all")
  testthat::expect_identical(test_module$server_args, NULL)
  testthat::expect_identical(test_module$ui_args, NULL)
  testthat::expect_identical(test_module$path, "aaa1")
})

testthat::test_that("modules gives error if no arguments other than label are used", {
  testthat::expect_error(modules(label = "my label"))
  testthat::expect_error(modules()) # using default label argument
})

testthat::test_that("modules requires label argument to be a string ", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    datanames = ""
  )

  testthat::expect_no_error(modules(label = "label", test_module))
  testthat::expect_error(modules(label = NULL, test_module), "Assertion on 'label' failed.+'NULL'")
  testthat::expect_error(
    modules(label = c("label", "label"), test_module),
    "Assertion on 'label' failed: Must have length <= 1"
  )
})

testthat::test_that("modules accept teal_module in ...", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    datanames = ""
  )

  testthat::expect_no_error(modules(label = "label", test_module))
})

testthat::test_that("modules accept multiple teal_module objects in ...", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    datanames = ""
  )

  testthat::expect_no_error(modules(label = "label", test_module, test_module))
})

testthat::test_that("modules accept multiple teal_module and teal_modules objects in ...", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    datanames = ""
  )
  test_modules <- modules(label = "label", test_module)

  testthat::expect_no_error(modules(label = "label", test_module, test_modules))
})

testthat::test_that("modules does not accept objects other than teal_module(s) in ...", {
  testthat::expect_error(
    modules(label = "label", 5),
    "the following types: \\{teal_module,teal_modules\\}",
  )
})

testthat::test_that("modules does not accept objects other than teal_module(s) in ...", {
  testthat::expect_error(
    modules(label = "label", "a"),
    "The only character argument to modules\\(\\) must be 'label'",
  )
})

testthat::test_that("modules returns teal_modules object with label and children slot", {
  test_module <- module(
    label = "label",
    server = module_server_fun,
    ui = ui_fun1,
    datanames = ""
  )
  out <- modules(label = "label2", test_module)
  testthat::expect_s3_class(out, "teal_modules")
  testthat::expect_named(out, c("label", "children"))
})

testthat::test_that("modules returns children as list and changes their path to match group they are grouped by", {
  test_module <- module(
    label = "module",
    server = module_server_fun,
    ui = ui_fun1,
    datanames = ""
  )
  test_modules <- modules(label = "modules", test_module)
  out <- modules(label = "tabs", test_module, test_modules)
  test_module$path <- "tabs / module"
  test_modules$children[[1]]$path <- "tabs / modules / module"
  testthat::expect_identical(out$children, list(test_module, test_modules))
})


testthat::test_that("modules returns useful error message if label argument not explicitly named", {
  test_module <- module(
    label = "module",
    server = module_server_fun,
    ui = ui_fun1,
    datanames = ""
  )
  testthat::expect_error(
    modules("module", test_module),
    "The only character argument to modules\\(\\) must be 'label'"
  )
})


testthat::test_that("modules returns children as list with unique path if labels are duplicated", {
  test_module <- module(
    label = "module",
    server = module_server_fun,
    ui = ui_fun1,
    datanames = ""
  )
  out <- modules(label = "modules", test_module, test_module)
  testthat::expect_identical(sapply(out$children, `[[`, "path"), c("modules / module", "modules / module - 1"))
})

# is_arg_used -----
get_srv_and_ui <- function() {
  list(
    server_fun = function(id, datasets) {},
    ui_fun = function(id, ...) {
      tags$p(paste0("id: ", id))
    }
  )
}

testthat::test_that("is_arg_used throws error if object is not teal_module or teal_modules", {
  testthat::expect_error(is_arg_used(5, "reporter"), "is_arg_used function not implemented for this object")
  testthat::expect_error(is_arg_used(list(), "reporter"), "is_arg_used function not implemented for this object")
})

testthat::test_that("is_arg_used returns true if teal_module has given `arg` in server function args", {
  testthat::expect_true(is_arg_used(module(server = function(id, data, reporter) NULL), "reporter"))
})

testthat::test_that("is_arg_used returns false if teal_module does not have reporter in server function args", {
  testthat::expect_false(is_arg_used(module(), "reporter"))
})


testthat::test_that("is_arg_used returns false if teal_modules has no children using given `arg`", {
  mod <- module()
  mods <- modules(label = "lab", mod, mod)
  testthat::expect_false(is_arg_used(mods, "reporter"))

  mods <- modules(label = "lab", mods, mod, mod)
  testthat::expect_false(is_arg_used(mods, "reporter"))
})

testthat::test_that("is_arg_used returns true if teal_modules has at least one child using given `arg`", {
  server_fun_with_reporter <- function(id, data, reporter) NULL

  mod <- module()
  mod_with_reporter <- module(server = server_fun_with_reporter)

  mods <- modules(label = "lab", mod, mod_with_reporter)
  testthat::expect_true(is_arg_used(mods, "reporter"))

  mods_2 <- modules(label = "lab", mods, mod, mod)
  testthat::expect_true(is_arg_used(mods_2, "reporter"))

  mods_3 <- modules(label = "lab", modules(label = "lab", mod, mod), mod_with_reporter, mod)
  testthat::expect_true(is_arg_used(mods_3, "reporter"))
})

testthat::test_that("is_arg_used returns TRUE/FALSE when the `arg` is in function formals", {
  testthat::expect_true(is_arg_used(function(x) NULL, "x"))
  testthat::expect_false(is_arg_used(function(x) NULL, "y"))
})

testthat::test_that("is_arg_used accepts `arg` to be a string only", {
  testthat::expect_error(is_arg_used(function(x) NULL, c("x", "y")))
  testthat::expect_error(is_arg_used(function(x) NULL, 1))
  testthat::expect_error(is_arg_used(function(x) NULL, NULL))
})

# format ----------------------------------------------------------------------------------------------------------

testthat::test_that("format.teal_modules returns proper structure", {
  mod <- module(label = "a")
  mod2 <- module(label = "c")
  mods <- modules(label = "c", mod, mod2)
  mod3 <- module(label = "c")

  appended_mods <- append_module(mods, mod3)

  testthat::expect_setequal(
    strsplit(cli::ansi_strip(format(appended_mods)), "\n")[[1]],
    c(
      "TEAL ROOT",
      "  |- a",
      "  |  |- Datasets         : all",
      "  |  |- Properties:",
      "  |  |  |- Bookmarkable  : FALSE",
      "  |  |  L- Reportable    : FALSE",
      "  |  |- UI Arguments     : ",
      "  |  |- Server Arguments : ",
      "  |  |- Decorators       : ",
      "  |  L- Transformators   : ",
      "  |- c",
      "  |  |- Datasets         : all",
      "  |  |- Properties:",
      "  |  |  |- Bookmarkable  : FALSE",
      "  |  |  L- Reportable    : FALSE",
      "  |  |- UI Arguments     : ",
      "  |  |- Server Arguments : ",
      "  |  |- Decorators       : ",
      "  |  L- Transformators   : ",
      "  L- c",
      "     |- Datasets         : all",
      "     |- Properties:",
      "     |  |- Bookmarkable  : FALSE",
      "     |  L- Reportable    : FALSE",
      "     |- UI Arguments     : ",
      "     |- Server Arguments : ",
      "     |- Decorators       : ",
      "     L- Transformators   : "
    )
  )
})


testthat::test_that("module datanames is appended by its transformators datanames", {
  transformator_w_datanames <- teal_transform_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          new_data <- within(data(), {
            new_dataset <- data.frame(a = 1:3, b = 4:6)
          })
          new_data
        })
      })
    },
    datanames = c("a", "b")
  )

  out <- module(datanames = "c", transformators = list(transformator_w_datanames))
  testthat::expect_identical(out$datanames, c("c", "a", "b"))
})

testthat::test_that("module datanames stays 'all' regardless of transformators", {
  transformator_w_datanames <- teal_transform_module(
    ui = function(id) NULL,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          new_data <- within(data(), {
            new_dataset <- data.frame(a = 1:3, b = 4:6)
          })
          new_data
        })
      })
    },
    datanames = c("a", "b")
  )

  out <- module(datanames = "all", transformators = list(transformator_w_datanames))
  testthat::expect_identical(out$datanames, "all")
})
