dataset_1 <- Dataset$new("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl_dataset <- CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))

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

testthat::test_that("module correct server and ui arguments", {
  expect_error(module(
    "callModule",
    server = call_module_server_fun,
    ui = ui_fun1,
    filters = "all"
  ), NA)

  expect_error(module(
    "callModule",
    server = call_module_server_fun,
    ui = ui_fun2,
    filters = "all"
  ), NA)

  expect_error(module(
    "moduleServer",
    server = module_server_fun,
    ui = ui_fun2,
    filters = "all"
  ), NA)

  expect_error(module(
    "moduleServer",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "all"
  ), NA)
})

testthat::test_that("module with incorrect server and/or ui arguments", {
  expect_error(module(
    "callModule",
    server =  function(input, sth, session, datasets) {
    },
    ui = ui_fun1,
    filters = "all"
  ), "teal modules server functions need ordered arguments")

  expect_error(module(
    "callModule",
    server = call_module_server_fun,
    ui = function(sth, id, ...) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ), "teal modules need 'id' argument as a first argument in their ui function")

  expect_error(module(
    "callModule",
    server = call_module_server_fun,
    ui = function(id, sth) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ), "teal modules need 'datasets' or '...' argument as a second argument in their ui function")

  expect_error(module(
    "moduleServer",
    server = function(id, sth, datasets) {
    },
    ui = ui_fun1,
    filters = "all"
  ), "teal modules server functions need ordered arguments")
})

testthat::test_that("overall test of modules under server function defined with moduleServer or callModule", {
  mods_call_module <- modules(
    "d1",
    modules(
      "d2",
      modules(
        "d3",
        module(
          label = "aaa1",
          server = call_module_server_fun,
          ui = ui_fun1, filters = "all",
          server_args = NULL,
          ui_args = NULL
        ), module(
          label = "aaa2",
          server = call_module_server_fun,
          ui = ui_fun1,
          filters = "all",
          server_args = NULL,
          ui_args = NULL
        ), create_mod("aaa3")
      ),
      module(
        label = "bbb",
        server = call_module_server_fun,
        ui = ui_fun1,
        filters = "all",
        server_args = NULL,
        ui_args = NULL
      )
    ),
    module(
      label = "ccc",
      server = call_module_server_fun,
      ui = ui_fun1,
      filters = "all",
      server_args = NULL, ui_args = NULL
    )
  )

  mods_module_server <- modules(
    "d1",
    modules(
      "d2",
      modules(
        "d3",
        module(
          label = "aaa1",
          server = module_server_fun,
          ui = ui_fun1, filters = "all",
          server_args = NULL,
          ui_args = NULL
        ), module(
          label = "aaa2",
          server = module_server_fun,
          ui = ui_fun1,
          filters = "all",
          server_args = NULL,
          ui_args = NULL
        ), create_mod("aaa3")
      ),
      module(
        label = "bbb",
        server = module_server_fun,
        ui = ui_fun1,
        filters = "all",
        server_args = NULL,
        ui_args = NULL
      )
    ),
    module(
      label = "ccc",
      server = module_server_fun,
      ui = ui_fun1,
      filters = "all",
      server_args = NULL, ui_args = NULL
    )
  )

  expect_s3_class(mods_call_module, "teal_modules")
  expect_s3_class(mods_module_server, "teal_modules")

  expect_s3_class(mods_call_module$children$ccc, "teal_module")
  expect_s3_class(mods_module_server$children$ccc, "teal_module")

  expect_identical(names(formals(mods_call_module$children$ccc$server)), c("input", "output", "session", "datasets"))
  expect_identical(names(formals(mods_module_server$children$ccc$server)), c("id", "datasets"))

  expect_identical(names(formals(mods_call_module$children$ccc$ui))[1], "id")
  expect_identical(names(formals(mods_module_server$children$ccc$ui))[1], "id")
})

testthat::test_that("error when duplicated labels in modules", {
  expect_error(
    modules(
      "aa",
      module(
        "ccc",
        server = call_module_server_fun,
        ui = ui_fun2,
        filters = "all"
      ),
      module(
        "ccc",
        server = call_module_server_fun,
        ui = ui_fun2,
        filters = "all"
      )
    ), "Please choose unique labels for each tab"
  )
})

testthat::test_that("each modules and module needs a label", {
  expect_error(modules(), 'argument "label" is missing, with no default')
  expect_error(module(), 'argument "label" is missing, with no default')
  expect_error(modules("aa", modules()), 'argument "label" is missing, with no default')
  expect_error(modules("aa", module()), 'argument "label" is missing, with no default')
})

testthat::test_that("root_modules needs at least one argument", {
  expect_error(root_modules(), "You must provide at least one module.")
})


testthat::test_that("all modules arguments are of class teal_module or teal_modules", {

  expect_error(modules(
    "aa", module("aaa",
      server = call_module_server_fun,
      ui = ui_fun1,
      filters = "all"
    ),
    module("bbb",
      server = module_server_fun,
      ui = ui_fun1,
      filters = "all"
    )
  ), NA)

  expect_error(modules("aa", module("aaa",
    server = call_module_server_fun,
    ui = ui_fun1,
    filters = "all"
  ), list()), "modules: not all arguments are of class teal_module or teal_modules.")

  expect_error(modules("aa", list(module("aaa",
    server = call_module_server_fun,
    ui = ui_fun1,
    filters = "all"
  ), module("bbb",
    server = module_server_fun,
    ui = ui_fun1,
    filters = "all"
  ))), "modules: not all arguments are of class teal_module or teal_modules.")

})
