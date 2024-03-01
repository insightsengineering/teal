testthat::test_that("srv_teal_with_splash data accepts a teal_data_module", {
  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_with_splash,
      args = list(
        id = "id",
        data = teal_data_module(ui = function(id) div(), server = function(id) reactive(NULL)),
        modules = modules(example_module())
      ),
      expr = {}
    )
  )
})

testthat::test_that("srv_teal_with_splash throws when teal_data_module doesn't return reactive", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_teal_with_splash,
      args = list(
        id = "id",
        data = teal_data_module(ui = function(id) div(), server = function(id) NULL),
        modules = modules(example_module())
      ),
      expr = {}
    ),
    "The `teal_data_module` passed to `data` must return a reactive expression."
  )
})

testthat::test_that("srv_teal_with_splash teal_data_rv evaluates the server of teal_data_module", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = teal_data_module(ui = function(id) div(), server = function(id) reactive("whatever")),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_is(teal_data_rv, "reactive")
      testthat::expect_identical(teal_data_rv(), "whatever")
    }
  )
})

testthat::test_that("srv_teal_with_splash passes teal_data to reactive", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_is(teal_data_rv_validate, "reactive")
      testthat::expect_s4_class(teal_data_rv_validate(), "teal_data")
    }
  )
})

testthat::test_that("srv_teal_with_splash passes when datanames are empty with warning", {
  testthat::expect_warning(
    shiny::testServer(
      app = srv_teal_with_splash,
      args = list(
        id = "test",
        data = teal.data::teal_data(),
        modules = modules(example_module())
      ),
      expr = {
        testthat::expect_is(teal_data_rv_validate, "reactive")
        testthat::expect_s4_class(teal_data_rv_validate(), "teal_data")
      }
    ),
    "`data` object has no datanames. Default datanames are set using `teal_data`'s environment."
  )
})

testthat::test_that("srv_teal_with_splash teal_data_rv_validate throws when teal_data_module returns error", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = teal_data_module(
        ui = function(id) div(),
        server = function(id) reactive(stop("this error"))
      ),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_is(teal_data_rv_validate, "reactive")
      testthat::expect_error(teal_data_rv_validate(), "this error")
    }
  )
})

testthat::test_that("srv_teal_with_splash teal_data_rv_validate throws then qenv.error occurs", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = teal_data_module(
        ui = function(id) div(),
        server = function(id) reactive(within(teal.data::teal_data(), stop("not good")))
      ),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_is(teal_data_rv_validate, "reactive")
      testthat::expect_error(teal_data_rv_validate(), "not good")
    }
  )
})

testthat::test_that(
  "srv_teal_with_splash teal_data_rv_validate throws when teal_data_module doesn't return teal_data",
  {
    shiny::testServer(
      app = srv_teal_with_splash,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) div(),
          server = function(id) reactive(data.frame())
        ),
        modules = modules(example_module())
      ),
      expr = {
        testthat::expect_is(teal_data_rv_validate, "reactive")
        testthat::expect_error(teal_data_rv_validate(), "failed to return `teal_data`")
      }
    )
  }
)

testthat::test_that("srv_teal_with_splash teal_data_rv_validate throws when incompatible module's datanames", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = teal.data::teal_data(mtcars = mtcars, iris = iris, npk = npk),
      modules = modules(example_module(datanames = "non-existing"))
    ),
    expr = {
      testthat::expect_is(teal_data_rv_validate, "reactive")
      testthat::expect_error(
        teal_data_rv_validate(),
        "Module 'example teal module' uses datanames not available in 'data'"
      )
    }
  )
})

testthat::test_that("srv_teal_with_splash teal_data_rv_validate returns teal_data if incompatible filter's datanames", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = teal.data::teal_data(mtcars = mtcars),
      modules = modules(example_module(datanames = "mtcars")),
      filter = teal_slices(teal_slice(dataname = "iris", varname = "Species"))
    ),
    expr = {
      testthat::expect_is(teal_data_rv_validate, "reactive")
      testthat::expect_warning(
        teal_data_rv_validate(),
        "Filter 'iris Species' refers to dataname not available in 'data'"
      )
      testthat::expect_s4_class(teal_data_rv_validate(), "teal_data")
    }
  )
})

testthat::test_that("srv_teal_with_splash gets observe event from srv_teal", {
  shiny::testServer(
    app = srv_teal_with_splash,
    args = list(
      id = "test",
      data = teal.data::teal_data(),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_is(res, "Observer")
    }
  )
})

testthat::test_that("srv_teal_with_splash accepts data after within.teal_data_module", {
  tdm <- teal_data_module(ui = function(id) div(), server = function(id) reactive(teal_data(IRIS = iris)))
  tdm2 <- within(tdm, IRIS$id <- seq_len(NROW(IRIS$Species))) # nolint: object_name.

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_with_splash,
      args = list(
        id = "id",
        data = tdm2,
        modules = modules(example_module())
      ),
      expr = {
        testthat::expect_s3_class(teal_data_rv, "reactive")
        testthat::expect_s3_class(teal_data_rv_validate, "reactive")
        testthat::expect_s4_class(teal_data_rv_validate(), "teal_data")
        testthat::expect_identical(
          teal_data_rv_validate()[["IRIS"]],
          within(iris, id <- seq_len(NROW(Species)))
        )
      }
    )
  )
})

testthat::test_that("srv_teal_with_splash throws error when within.teal_data_module returns qenv.error", {
  tdm <- teal_data_module(ui = function(id) div(), server = function(id) reactive(teal_data(IRIS = iris)))
  tdm2 <- within(tdm, non_existing_var + 1)

  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_with_splash,
      args = list(
        id = "id",
        data = tdm2,
        modules = modules(example_module())
      ),
      expr = {
        testthat::expect_s3_class(teal_data_rv, "reactive")
        testthat::expect_s3_class(teal_data_rv(), "qenv.error")
        testthat::expect_s3_class(teal_data_rv_validate, "reactive")
        testthat::expect_error(teal_data_rv_validate(), "when evaluating qenv code")
      }
    )
  )
})

testthat::test_that("srv_teal_with_splash throws error when within.teal_data_module returns NULL", {
  tdm <- teal_data_module(ui = function(id) div(), server = function(id) reactive(NULL))
  tdm2 <- within(tdm, within(1 + 1))
  testthat::expect_no_error(
    shiny::testServer(
      app = srv_teal_with_splash,
      args = list(
        id = "id",
        data = tdm2,
        modules = modules(example_module())
      ),
      expr = {
        testthat::expect_s3_class(teal_data_rv, "reactive")
        testthat::expect_null(teal_data_rv())
        testthat::expect_s3_class(teal_data_rv_validate, "reactive")
        testthat::expect_error(
          teal_data_rv_validate(),
          "`teal_data_module` passed to `data` failed to return `teal_data` object"
        )
      }
    )
  )
})

testthat::test_that(
  paste(
    "srv_teal_with_splash throws error when within.teal_data_module returns arbitrary object",
    "(other than `teal_data` or `qenv.error`)"
  ),
  {
    tdm <- teal_data_module(ui = function(id) div(), server = function(id) reactive(NULL))
    tdm2 <- within(tdm, 1 + 1)
    testthat::expect_no_error(
      shiny::testServer(
        app = srv_teal_with_splash,
        args = list(
          id = "id",
          data = tdm2,
          modules = modules(example_module())
        ),
        expr = {
          testthat::expect_s3_class(teal_data_rv, "reactive")
          testthat::expect_null(teal_data_rv())
          testthat::expect_s3_class(teal_data_rv_validate, "reactive")
          testthat::expect_error(
            teal_data_rv_validate(),
            "`teal_data_module` passed to `data` failed to return `teal_data` object"
          )
        }
      )
    )
  }
)
