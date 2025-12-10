testthat::test_that("example_module2 can be created with default parameters", {
  testthat::expect_no_error(example_module2())
})

testthat::test_that("example_module2 returns a teal_module", {
  mod <- example_module2()
  testthat::expect_s3_class(mod, "teal_module")
})

testthat::test_that("example_module2 is bookmarkable", {
  mod <- example_module2()
  testthat::expect_true(attr(mod, "teal_bookmarkable"))
})

testthat::test_that("example_module2 can be used in init", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris),
      modules = example_module2()
    )
  )
})

testthat::test_that("example_module2 can be used with custom label", {
  mod <- example_module2(label = "custom label")
  testthat::expect_equal(mod$label, "custom label")
})

testthat::test_that("example_module2 can be combined with other modules", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(iris = iris, mtcars = mtcars),
      modules = modules(example_module(), example_module2())
    )
  )
})
