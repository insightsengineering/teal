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

testthat::describe("srv_data: data_validated", {
  testthat::it("throws original error with note to app user but module doesn't fail", {
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
          "this is an error \n \n Check your inputs or contact app developer"
        )
      }
    )
  })

  testthat::it("throws original validate with note to app user but module doesn't fail", {
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
          "this is a validation error \n \n Check your inputs or contact app developer"
        )
      }
    )
  })

  testthat::it("is NULL when reactive returns silent.error", {
    shiny::testServer(
      app = srv_data,
      args = list(
        id = "test",
        data = reactive(req(FALSE)),
        modules = modules(example_module())
      ),
      expr = {
        testthat::expect_null(data_validated())
      }
    )
  })

  testthat::it("throws original validate with note to app user but module doesn't fail", {
    shiny::testServer(
      app = srv_data,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), stop("test"))),
        modules = modules(example_module())
      ),
      expr = {
        testthat::expect_error(
          data_validated(),
          "test.+Check your inputs or contact app developer"
        )
      }
    )
  })

  testthat::it("returns data with signature (hash) code", {
    shiny::testServer(
      app = srv_data,
      args = list(
        id = "test",
        data = reactive(within(teal.data::teal_data(), {
          IRIS <- iris
          MTCARS <- mtcars
          IRIS <- head(IRIS)
        })),
        modules = modules(example_module())
      ),
      expr = {
        hashes <- vapply(
          c("IRIS", "MTCARS"),
          function(dataname) {
            sprintf(
              "stopifnot(rlang::hash(%s) == \"%s\") # @linksto %1$s",
              dataname,
              rlang::hash(data_validated()[[sprintf("%s", dataname)]])
            )
          },
          character(1L),
          USE.NAMES = FALSE
        )

        testthat::expect_identical(
          get_code(data_output()),
          paste(
            c(
              get_code(data_validated()),
              hashes
            ),
            collapse = "\n"
          )
        )
      }
    )
  })

})
