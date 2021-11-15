dataset_1 <- Dataset$new("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl_dataset <- CDISCDataset$new("ADSL", adsl_df, parent = character(0), keys = get_cdisc_keys("ADSL"))


call_module_server_fun <- function(input, output, session, datasets) {
}

module_server_fun <- function(id, datasets) {
}

uiFun1 <- function(id, ...) {
  tags$p(paste0("id: ", id))
}

uiFun2 <- function(id, datasets) {
  tags$p(paste0("id: ", id))
}

mods_call_module <- structure(list(label = "d1", children = list(
  d2 = structure(list(
    label = "d2", children = list(
      d3 = structure(list(
        label = "d3",
        children = list(
          aaa1 = structure(list(
            label = "aaa1",
            server = call_module_server_fun,
            ui = uiFun1, filters = "all",
            server_args = NULL,
            ui_args = NULL
          ), class = "teal_module"),
          aaa2 = structure(list(label = "aaa2",
                                server = call_module_server_fun,
                                ui = uiFun1,
                                filters = "all",
                                server_args = NULL,
                                ui_args = NULL), class = "teal_module"),
          aaa3 = structure(list(label = "aaa3",
                                server = call_module_server_fun,
                                ui = uiFun1,
                                filters = "all",
                                server_args = NULL,
                                ui_args = NULL), class = "teal_module")
        )
      ), class = "teal_modules"),
      bbb = structure(list(label = "bbb",
                           server = call_module_server_fun,
                           ui = uiFun1,
                           filters = "all",
                           server_args = NULL,
                           ui_args = NULL), class = "teal_module")
    )
  ), class = "teal_modules"),
  ccc = structure(list(label = "ccc",
                       server = call_module_server_fun,
                       ui = uiFun1,
                       filters = "all",
                       server_args = NULL, ui_args = NULL),
                  class = "teal_module")
)), class = "teal_modules")

mods_module_server <- structure(list(label = "d1", children = list(
  d2 = structure(list(
    label = "d2", children = list(
      d3 = structure(list(
        label = "d3",
        children = list(
          aaa1 = structure(list(
            label = "aaa1",
            server = module_server_fun,
            ui = uiFun1, filters = "all",
            server_args = NULL,
            ui_args = NULL
          ), class = "teal_module"),
          aaa2 = structure(list(label = "aaa2",
                                server = module_server_fun,
                                ui = uiFun1,
                                filters = "all",
                                server_args = NULL,
                                ui_args = NULL), class = "teal_module"),
          aaa3 = structure(list(label = "aaa3",
                                server = module_server_fun,
                                ui = uiFun1,
                                filters = "all",
                                server_args = NULL,
                                ui_args = NULL), class = "teal_module")
        )
      ), class = "teal_modules"),
      bbb = structure(list(label = "bbb",
                           server = module_server_fun,
                           ui = uiFun1,
                           filters = "all",
                           server_args = NULL,
                           ui_args = NULL), class = "teal_module")
    )
  ), class = "teal_modules"),
  ccc = structure(list(label = "ccc",
                       server = module_server_fun,
                       ui = uiFun1,
                       filters = "all",
                       server_args = NULL, ui_args = NULL),
                  class = "teal_module")
)), class = "teal_modules")

testthat::test_that("module correct server and ui arguments", {
  expect_error(module(
    "callModule",
    server = call_module_server_fun,
    ui = uiFun1,
    filters = "all"
  ), NA)

  expect_error(module(
    "callModule",
    server = call_module_server_fun,
    ui = uiFun2,
    filters = "all"
  ), NA)

  expect_error(module(
    "moduleServer",
    server = module_server_fun,
    ui = uiFun2,
    filters = "all"
  ), NA)

  expect_error(module(
    "moduleServer",
    server = module_server_fun,
    ui = uiFun1,
    filters = "all"
  ), NA)
})

testthat::test_that("module with incorrect server and/or ui arguments", {
  expect_error(module(
    "callModule",
    server = server = function(input, sth, session, datasets) {
    },
    ui = uiFun1,
    filters = "all"
  ))

  expect_error(module(
    "callModule",
    server = call_module_server_fun,
    ui = function(sth, id, ...) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))

  expect_error(module(
    "callModule",
    server = call_module_server_fun,
    ui = function(id, sth) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
  ))

  expect_error(module(
    "moduleServer",
    server = function(id, sth, datasets) {
    },
    ui = uiFun1,
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
  expect_error(
    modules(
      "aa",
      module(
        "ccc",
        server = call_module_server_fun,
        ui = uiFun2,
        filters = "all"
      ),
      module(
        "ccc",
        server = call_module_server_fun,
        ui = uiFun2,
        filters = "all"
      )
    )
  )
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

  expect_error(modules(
    "aa", module("aaa",
      server = call_module_server_fun,
      ui = uiFun1,
      filters = "all"
    ),
    module("bbb",
      server = module_server_fun,
      ui = uiFun1,
      filters = "all"
    )
  ), NA)

  expect_error(modules("aa", module("aaa",
    server = call_module_server_fun,
    ui = uiFun1,
    filters = "all"
  ), list()))

  expect_error(modules("aa", list(module("aaa",
    server = call_module_server_fun,
    ui = uiFun1,
    filters = "all"
  ), module("bbb",
    server = module_server_fun,
    ui = uiFun1,
    filters = "all"
  ))))

})
