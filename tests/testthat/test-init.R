# data ----
testthat::test_that("init data accepts teal_data object", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module())
    )
  )
})

testthat::test_that("init data accepts teal_data_module", {
  testthat::expect_no_error(
    init(
      data = teal_data_module(ui = function(id) tags$div(), server = function(id) NULL),
      modules = modules(example_module())
    )
  )
})


# modules ----
testthat::test_that("init modules accepts a teal_modules object", {
  mods <- modules(example_module(), example_module())
  testthat::expect_no_error(init(data = teal.data::teal_data(iris = iris), modules = mods))
})

testthat::test_that("init modules accepts a list of teal_module elements", {
  mods <- list(example_module(), example_module())
  testthat::expect_no_error(init(data = teal.data::teal_data(iris = iris), modules = mods))
})

testthat::test_that("init modules accepts a teal_module object", {
  mods <- example_module()
  testthat::expect_no_error(init(data = teal.data::teal_data(iris = iris), modules = mods))
})

# filter ----
testthat::test_that("init filter accepts `teal_slices`", {
  fs <- teal.slice::teal_slices(
    teal.slice::teal_slice(dataname = "iris", varname = "species", selected = "setosa")
  )
  testthat::expect_no_error(
    init(data = teal.data::teal_data(iris = iris), modules = modules(example_module()), filter = fs)
  )
  testthat::expect_error(
    init(data = teal.data::teal_data(iris = iris), modules = modules(example_module()), filter = unclass(fs)),
    "Assertion on 'filter' failed"
  )
})

# data + modules ----
testthat::test_that("init throws when an empty `data` is used", {
  testthat::expect_error(
    init(data = teal.data::teal_data(), modules = list(example_module())),
    "The environment of `data` is empty."
  )
})

testthat::test_that(
  "init throws warning when datanames in modules incompatible w/ datanames in data and there is no transformators",
  {
    testthat::expect_warning(
      init(
        data = teal.data::teal_data(mtcars = mtcars),
        modules = list(example_module(datanames = "iris"))
      ),
      "Dataset `iris` is missing for module 'example teal module'. Dataset available in data: `mtcars`."
    )
  }
)

testthat::test_that(
  "init throws warning when datanames in modules incompatible w/ datanames in data and there is no transformators",
  {
    testthat::expect_warning(
      init(
        data = teal.data::teal_data(mtcars = mtcars),
        modules = list(example_module(datanames = c("a", "b")))
      ),
      "Datasets `a` and `b` are missing for module 'example teal module'. Dataset available in data: `mtcars`."
    )
  }
)

testthat::test_that(
  "init doesn't throw warning when datanames in modules incompatible w/ datanames in data and there are transformators",
  {
    testthat::expect_no_warning(
      init(
        data = teal.data::teal_data(mtcars = mtcars),
        modules = list(
          example_module(
            datanames = "iris",
            transformators = list(
              teal_transform_module(
                ui = function(id) NULL,
                server = function(id, data) {
                  moduleServer(id, function(input, output, session) {
                    NULL
                  })
                }
              )
            )
          )
        )
      )
    )
  }
)

testthat::describe("init throws warning when datanames in modules has reserved name", {
  testthat::it("`all`", {
    testthat::expect_warning(
      init(
        data = teal.data::teal_data(all = mtcars),
        modules = list(example_module())
      ),
      "`all` is reserved for internal use\\. Please avoid using it as a dataset name\\."
    )
  })

  testthat::it("`.raw_data` and `all`", {
    td <-
      testthat::expect_warning(
        init(
          data = teal.data::teal_data(
            all = mtcars,
            .raw_data = iris,
            join_keys = teal.data::join_keys(teal.data::join_key(".raw_data", "all", "a_key"))
          ),
          modules = list(example_module())
        ),
        "`.raw_data` and `all` are reserved for internal use\\. Please avoid using them as dataset names\\."
      )
  })
})

testthat::test_that("init throws when dataname in filter incompatible w/ datanames in data", {
  testthat::expect_warning(
    init(
      data = teal.data::teal_data(mtcars = mtcars),
      modules = modules(example_module()),
      filter = teal_slices(teal_slice(dataname = "iris", varname = "Species"))
    ),
    "Filter 'iris Species' refers to dataname not available in 'data'"
  )
})
