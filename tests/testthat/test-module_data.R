# asserts and returned value from the module is tested in test-module_teal.R
# here we test the validation only.

testthat::test_that("srv_data fails when teal_data_module doesn't return a reactive", {
  testthat::expect_error(
    shiny::testServer(
      app = srv_data,
      args = list(
        id = "test",
        data = teal_data_module(
          ui = function(id) NULL,
          server = function(id) teal_data(iris = iris)
        ),
        modules = modules(example_module())
      ),
      expr = NULL
    ),
    "The `teal_data_module` passed to `data` must return a reactive expression."
  )
})

testthat::test_that("srv_data: data_validated throws original error but module doesn't fail", {
  shiny::testServer(
    app = srv_data,
    args = list(
      id = "test",
      data = reactive(stop("this is an error")),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_error(
        data_validated(),
        "this is an error"
      )
    }
  )
})

testthat::test_that("srv_data: data_validated throws original validate message but module doesn't fail", {
  shiny::testServer(
    app = srv_data,
    args = list(
      id = "test",
      data = reactive(validate(need(FALSE, "this is a validation error"))),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_error(
        data_validated(),
        "this is a validation error"
      )
    }
  )
})

testthat::test_that("srv_data: data_validated throws an error when reactive returns silent.error", {
  # QUESTION: should this return NULL?
  # THEN in 1.0_module_data substitute
  #    } else if (inherits(data, c("reactive", "reactiveVal"))) {
  #    data
  # with
  #    } else if (inherits(data, c("reactive", "reactiveVal"))) {
  #      if (is.null(tryCatch(data(), error = function(cond) NULL))) {
  #        NULL
  #      } else {
  #        data
  #      }
  #    }
  shiny::testServer(
    app = srv_data,
    args = list(
      id = "test",
      data = reactive(req(FALSE)),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_error(data_validated())
    }
  )
})

testthat::test_that("srv_data: data_validated returns qenv.error if data is qenv.error", {
  # I THINK THIS REQUIERES DEEPER INVESTIAGTION AS I AM NOT SURE WE CAN LATER HANDLE "qenv.error"
  # I THINK WE SHOULD HANDLE and return a message instead of "qenv.error"
  # PROBABLY REQUIRE CHANGES in 1.0_module_data
  shiny::testServer(
    app = srv_data,
    args = list(
      id = "test",
      data = reactive(within(teal.data::teal_data(), stop("test"))),
      modules = modules(example_module())
    ),
    expr = {
      testthat::expect_s3_class(
        data_validated(),
        "qenv.error"
      )
    }
  )
})

# IMPORTANT: we stopped adding a note about CONTACTING THE APP DEVELOPER
