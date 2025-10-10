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

# filter module_specific tests ----
testthat::test_that("init throws error when filter mapping has invalid module names", {
  testthat::expect_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module(label = "mod1")),
      filter = teal_slices(
        teal_slice(dataname = "iris", varname = "Species"),
        module_specific = TRUE,
        mapping = list(nonexistent_module = "iris Species")
      )
    ),
    "Some module names in the mapping arguments don't match module labels"
  )
})

testthat::test_that("init throws error when modules have duplicate labels with module_specific filter", {
  testthat::expect_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(
        example_module(label = "duplicate_label"),
        example_module(label = "duplicate_label")
      ),
      filter = teal_slices(
        teal_slice(dataname = "iris", varname = "Species"),
        module_specific = TRUE,
        mapping = list(duplicate_label = "iris Species")
      )
    ),
    "Module labels should be unique when teal_slices\\(mapping = TRUE\\)"
  )
})

testthat::test_that("init accepts valid module_specific filter with proper mapping", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris, mtcars = mtcars),
      modules = modules(
        example_module(label = "iris_module"),
        example_module(label = "mtcars_module")
      ),
      filter = teal_slices(
        teal_slice(dataname = "iris", varname = "Species"),
        teal_slice(dataname = "mtcars", varname = "cyl"),
        module_specific = TRUE,
        mapping = list(
          iris_module = "iris Species",
          global_filters = "mtcars cyl"
        )
      )
    )
  )
})

# reporter tests ----
testthat::test_that("init accepts NULL reporter to disable reporting", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      reporter = NULL
    )
  )
})

testthat::test_that("init accepts Reporter object", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      reporter = teal.reporter::Reporter$new()
    )
  )
})

# deprecated parameters tests ----
testthat::test_that("init shows deprecation warning for title parameter", {
  lifecycle::expect_deprecated(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      title = "Deprecated Title"
    )
  )
})

testthat::test_that("init shows deprecation warning for header parameter", {
  lifecycle::expect_deprecated(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      header = tags$div("Deprecated Header")
    )
  )
})

testthat::test_that("init shows deprecation warning for footer parameter", {
  lifecycle::expect_deprecated(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      footer = tags$div("Deprecated Footer")
    )
  )
})

testthat::test_that("init shows deprecation warning for id parameter", {
  lifecycle::expect_deprecated(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(example_module()),
      id = "test_id"
    )
  )
})

testthat::test_that("init shows deprecation warning when landing_popup_module is included in modules", {
  lifecycle::expect_deprecated(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = modules(
        example_module(),
        landing_popup_module(
          label = "Landing",
          title = "Welcome",
          content = "Welcome to the app"
        )
      )
    )
  )
})

testthat::test_that("init throws error when landing_popup_module is added twice to modules", {
  testthat::expect_error(
    suppressWarnings(
      init(
        data = teal.data::teal_data(iris = iris),
        modules = modules(
          example_module(),
          landing_popup_module(
            label = "Landing 1",
            title = "Welcome 1",
            content = "First landing popup"
          ),
          landing_popup_module(
            label = "Landing 2",
            title = "Welcome 2",
            content = "Second landing popup"
          )
        )
      )
    ),
    "Only one `landing_popup_module` can be used\\."
  )
})
