dataset_1 <- teal.data::dataset("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
adsl_dataset <- teal.data::cdisc_dataset(
  "ADSL", adsl_df,
  parent = character(0), keys = teal.data::get_cdisc_keys("ADSL")
)

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

testthat::test_that("Calling module() does not throw", {
  testthat::expect_error(module(), NA)
})

testthat::test_that("module requires label argument to be a string different than 'global_filters'", {
  testthat::expect_error(module(label = "label"), NA)

  testthat::expect_error(module(label = NULL), "Assertion on 'label' failed.+'NULL'")

  testthat::expect_error(module(label = c("label", "label")), "Assertion on 'label' failed: Must have length 1.")

  testthat::expect_error(module(label = 1L), "Assertion on 'label' failed.+not 'integer'")

  testthat::expect_error(module(label = "global_filters"), "is reserved in teal")
})

testthat::test_that("module expects server being a shiny server module with any argument", {
  testthat::expect_error(module(
    server = function(id) NULL
  ), NA)

  testthat::expect_error(module(
    server = function(id, any_argument) NULL,
  ), NA)

  testthat::expect_error(module(
    server = function(input, output, session, any_argument) NULL,
  ), NA)


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
  testthat::expect_error(module(server = function(id, a) NULL, server_args = list(a = 1)), NA)
  testthat::expect_error(module(server_args = list()), NA)
  testthat::expect_error(module(server_args = NULL), NA)
  testthat::expect_error(module(server_args = ""), "Assertion on 'server_args' failed.+'list'")
  testthat::expect_error(module(server_args = list(1, 2, 3)), "Must have names")
})

testthat::test_that("module expects all server_args being a server arguments or passed through `...`", {
  testthat::expect_error(module(
    server = function(id, arg1) NULL,
    server_args = list(arg1 = NULL)
  ), NA)

  testthat::expect_error(module(
    server = function(id, ...) NULL,
    server_args = list(arg1 = NULL)
  ), NA)

  testthat::expect_error(
    module(server = function(id) NULL, server_args = list(arg1 = NULL)),
    "Following `server_args` elements have no equivalent in the formals of the `server`"
  )
})

testthat::test_that("module requires ui_args argument to be a list", {
  testthat::expect_error(module(ui = function(id, a) NULL, ui_args = list(a = 1)), NA)
  testthat::expect_error(module(ui_args = list()), NA)
  testthat::expect_error(module(ui_args = NULL), NA)
  testthat::expect_error(module(ui_args = ""), "Assertion on 'ui_args' failed.+'list'")
  testthat::expect_error(module(ui_args = list(1, 2, 3)), "Must have names")
})

testthat::test_that("module expects ui being a shiny ui module with any argument", {
  testthat::expect_error(module(ui = function(id) NULL), NA)
  testthat::expect_error(module(ui = function(id, any_argument) NULL), NA)
  testthat::expect_error(
    module(ui = function(any_argument) NULL),
    "`ui` argument requires a function with following arguments"
  )
})

testthat::test_that("module expects all ui_args being a ui arguments or passed through `...`", {
  testthat::expect_error(module(
    ui = function(id, arg1) NULL,
    ui_args = list(arg1 = NULL)
  ), NA)

  testthat::expect_error(module(
    ui = function(id, ...) NULL,
    ui_args = list(arg1 = NULL)
  ), NA)


  testthat::expect_error(
    module(ui = function(id) NULL, ui_args = list(arg1 = NULL)),
    "Following `ui_args` elements have no equivalent in the formals of `ui`"
  )
})

testthat::test_that("module requires filters argument to be a character or NULL", {
  testthat::expect_error(module(filters = "all"), NA)
  testthat::expect_error(module(filters = ""), NA)
  testthat::expect_error(module(filters = NULL), NA)
  testthat::expect_error(module(filters = NA_character_), "Contains missing values")
  testthat::expect_error(module(server = function(id, data) NULL, filters = NULL), NA)
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

testthat::test_that("modules gives error if no arguments other than label are used", {
  testthat::expect_error(modules(label = "my label"))
  testthat::expect_error(modules()) # using default label argument
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
  test_modules <- modules(label = "label", test_module)

  testthat::expect_error(modules(label = "label", test_module, test_modules), NA)
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
    filters = ""
  )
  out <- modules(label = "label2", test_module)
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
  test_modules <- modules(label = "modules", test_module)
  out <- modules(label = "tabs", test_module, test_modules)$children
  testthat::expect_named(out, c("module", "modules"))
  testthat::expect_identical(out$module, test_module)
  testthat::expect_identical(out$modules, test_modules)
})


testthat::test_that("modules returns useful error message if label argument not explicitly named", {
  test_module <- module(
    label = "module",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )
  testthat::expect_error(modules("module", test_module), "The only character argument to modules\\(\\) must be 'label'")
})


testthat::test_that("modules returns children as list with unique names if labels are duplicated", {
  test_module <- module(
    label = "module",
    server = module_server_fun,
    ui = ui_fun1,
    filters = ""
  )
  test_modules <- modules(label = "module", test_module)
  out <- modules(label = "tabs", test_module, test_modules)$children
  testthat::expect_named(out, c("module", "module_1"))
  testthat::expect_identical(out$module, test_module)
  testthat::expect_identical(out$module_1, test_modules)
})


testthat::test_that("modules_depth accepts depth as integer", {
  testthat::expect_error(
    modules_depth(
      module(
        label = "label",
        server = module_server_fun,
        ui = ui_fun1,
        filters = ""
      ),
      depth = 3L
    ),
    NA
  )

  testthat::expect_error(
    modules_depth(
      module(
        label = "label",
        server = module_server_fun,
        ui = ui_fun1,
        filters = ""
      ),
      depth = "1"
    ),
    "Assertion on 'depth' failed.+'character'"
  )
})

testthat::test_that("modules_depth returns depth=0 by default", {
  testthat::expect_identical(
    modules_depth(
      module(
        label = "label",
        server = module_server_fun,
        ui = ui_fun1,
        filters = ""
      )
    ),
    0L
  )
})

testthat::test_that("modules_depth accepts modules to be teal_module or teal_modules", {
  testthat::expect_error(
    modules_depth(
      module(
        label = "label",
        server = module_server_fun,
        ui = ui_fun1,
        filters = ""
      )
    ),
    NA
  )
  testthat::expect_error(
    modules_depth(
      modules(
        label = "tabs",
        module(
          label = "label",
          server = module_server_fun,
          ui = ui_fun1,
          filters = ""
        )
      )
    ),
    NA
  )
})

testthat::test_that("modules_depth returns depth same as input for teal_module", {
  testthat::expect_identical(
    modules_depth(
      module(
        label = "label",
        server = module_server_fun,
        ui = ui_fun1,
        filters = ""
      )
    ),
    0L
  )
})

testthat::test_that("modules_depth increases depth by 1 for each teal_modules", {
  testthat::expect_identical(
    modules_depth(
      modules(
        label = "tabs",
        module(
          label = "label",
          server = module_server_fun,
          ui = ui_fun1,
          filters = ""
        )
      ),
      depth = 1L
    ),
    2L
  )

  testthat::expect_identical(
    modules_depth(
      modules(
        label = "tabs",
        modules(
          label = "tabs",
          module(
            label = "label",
            server = module_server_fun,
            ui = ui_fun1,
            filters = ""
          )
        )
      ),
      depth = 1L
    ),
    3L
  )
})


# is_arg_used -----
get_srv_and_ui <- function() {
  return(list(
    server_fun = function(id, datasets) {}, # nolint
    ui_fun = function(id, ...) {
      tags$p(paste0("id: ", id))
    }
  ))
}

testthat::test_that("is_arg_used throws error if object is not teal_module or teal_modules", {
  testthat::expect_error(is_arg_used(5, "reporter"), "is_arg_used function not implemented for this object")
  testthat::expect_error(is_arg_used(list(), "reporter"), "is_arg_used function not implemented for this object")
})

testthat::test_that("is_arg_used returns true if teal_module has given `arg` in server function args", {
  testthat::expect_true(is_arg_used(module(server = function(id, datasets, reporter) NULL), "reporter"))
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
  server_fun_with_reporter <- function(id, datasets, reporter) NULL

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


# ---- append_module
testthat::test_that("append_module throws error when modules is not inherited from teal_modules", {
  testthat::expect_error(
    append_module(module(), module()),
    "Assertion on 'modules' failed: Must inherit from class 'teal_modules'"
  )

  testthat::expect_error(
    append_module(module(), list(module())),
    "Assertion on 'modules' failed: Must inherit from class 'teal_modules'"
  )
})

testthat::test_that("append_module throws error is module is not inherited from teal_module", {
  mod <- module()
  mods <- modules(label = "A", mod)

  testthat::expect_error(
    append_module(mods, mods),
    "Assertion on 'module' failed: Must inherit from class 'teal_module'"
  )

  testthat::expect_error(
    append_module(mods, list(mod)),
    "Assertion on 'module' failed: Must inherit from class 'teal_module'"
  )
})

testthat::test_that("append_module appends a module to children of not nested teal_modules", {
  mod <- module(label = "a")
  mod2 <- module(label = "b")
  mods <- modules(label = "c", mod, mod2)
  mod3 <- module(label = "d")

  appended_mods <- append_module(mods, mod3)
  testthat::expect_equal(appended_mods$children, list(a = mod, b = mod2, d = mod3))
})


testthat::test_that("append_module appends a module to children of nested teal_modules", {
  mod <- module(label = "a")
  mod2 <- module(label = "b")
  mods <- modules(label = "c", mod)
  mods2 <- modules(label = "e", mods, mod2)
  mod3 <- module(label = "d")

  appended_mods <- append_module(mods2, mod3)
  testthat::expect_equal(appended_mods$children, list(c = mods, b = mod2, d = mod3))
})

testthat::test_that("append_module produces teal_modules with unique named children", {
  mod <- module(label = "a")
  mod2 <- module(label = "c")
  mods <- modules(label = "c", mod, mod2)
  mod3 <- module(label = "c")

  appended_mods <- append_module(mods, mod3)
  mod_names <- names(appended_mods$children)
  testthat::expect_equal(mod_names, unique(mod_names))
})

testthat::test_that("teal_filters fails when inexisting teal_slice id is specified in mapping", {
  testthat::expect_error(
    teal_filters(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list(
        module = "inexisting"
      )
    )
  )
})

testthat::test_that("teal_filters returns modules_teal_slices", {
  testthat::expect_s3_class(
    teal_filters(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test")
    ),
    "modules_teal_slices"
  )
})

testthat::test_that("teal_filters mapping should be an empty list or a named list", {
  testthat::expect_no_error(
    teal_filters(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list()
    )
  )
  testthat::expect_no_error(
    teal_filters(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list(module = c())
    )
  )
  testthat::expect_error(
    teal_filters(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list(1, 2, 3)
    )
  )
})

testthat::test_that("teal_filters fails when inexisting teal_slice id is specified in mapping", {
  testthat::expect_error(
    teal_filters(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list(
        module = "inexisting"
      )
    ),
    "inexisting not in test"
  )
})

testthat::test_that("teal_filters fails when mapping is specified with module_specific = FALSE", {
  testthat::expect_error(
    teal_filters(
      teal.slice::teal_slice(dataname = "data", varname = "var", id = "test"),
      mapping = list(module = "test"),
      module_specific = FALSE
    ),
    "`mapping` is specified .+ even though `module_specific` isn't TRUE"
  )
})
