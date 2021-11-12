dataset_1 <- Dataset$new("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl_dataset <- CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))

mods_call_module <- teal:::get_dummy_modules()
mods_module_server <- teal:::get_dummy_modules(moduleServer = TRUE)

testthat::test_that("module correct server and ui arguments", {
  expect_silent(module(
    "callModule",
    server = function(input, output, session, datasets) {},
    ui = function(id, ...) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))

  expect_silent(module(
    "callModule",
    server = function(input, output, session, datasets) {},
    ui = function(id, datasets) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))

  expect_silent(module(
    "moduleServer",
    server = function(id, datasets) {},
    ui = function(id, datasets) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))

  expect_silent(module(
    "moduleServer",
    server = function(id, datasets) {},
    ui = function(id, ...) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))
})

testthat::test_that("module with incorrect server and/or ui arguments", {
  expect_error(module(
    "callModule",
    server = function(input, sth, session, datasets) {},
    ui = function(id, ...) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))

  expect_error(module(
    "callModule",
    server = function(input, output, session, datasets) {},
    ui = function(sth, id, ...) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))

  expect_error(module(
    "callModule",
    server = function(input, output, session, datasets) {},
    ui = function(id, sth) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))

  expect_error(module(
    "moduleServer",
    server = function(id, sth, datasets) {},
    ui = function(id, ...) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))
})

testthat::test_that("overall test of modules under server function defined with moduleServer or callModule", {
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
  modules(
    "aa",
    module(
      "ccc",
      server = function(input, output, session, datasets) {},
      ui = function(id, datasets) {
        tags$p(paste0("id: ", id))
      },
      filters = "all"
    ),
    module(
      "ccc",
      server = function(input, output, session, datasets) {},
      ui = function(id, datasets) {
        tags$p(paste0("id: ", id))
      },
      filters = "all"
    )
  )
})

testthat::test_that("no error when duplicated labels in modules as added after init", {
  mods_call_module$children$wrong_mod <- module(
    "ccc",
    server = function(input, output, session, datasets) {},
    ui = function(id, datasets) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  )
  mods_call_module$children$wrong_mod <- NULL
})


testthat::test_that("each modules and module needs a label", {
  expect_error(modules())
  expect_error(module())
  expect_error(modules("aa", modules()))
  expect_error(modules("aa", module()))
})

testthat::test_that("root_modules needs at least one argument", {
  expect_error(root_modules())
})


testthat::test_that("all modules arguments are of class teal_module or teal_modules", {
  check_modules <- function(modules) {
    for (x in modules$children) {
      if (inherits(x, "teal_modules")) {
        check_modules(x)
      } else if (inherits(x, "teal_module")) {
        NULL
      } else if (!inherits(x, "teal_module")) {
        stop("all modules arguments are of class teal_module or teal_modules")
      }
    }
  }

  expect_silent(check_modules(mods_call_module))

  mods_call_module$children$wrong_mod <- list()

  expect_error(check_modules(mods_call_module))

  mods_call_module$children$wrong_mod <- NULL
})
