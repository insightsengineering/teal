testthat::test_that("example_module2 creates a valid teal_module", {
  result <- example_module2()
  testthat::expect_s3_class(result, "teal_module")
  testthat::expect_true(attr(result, "teal_bookmarkable"))
})

testthat::test_that("example_module2 accepts label parameter", {
  result <- example_module2(label = "Custom Label")
  testthat::expect_s3_class(result, "teal_module")
})

testthat::test_that("example_module2 accepts datanames parameter", {
  testthat::expect_no_error(example_module2(datanames = "all"))
  testthat::expect_no_error(example_module2(datanames = c("iris", "mtcars")))
  testthat::expect_no_error(example_module2(datanames = NULL))
})

testthat::test_that("example_module and example_module2 can be used together in modules()", {
  result <- modules(
    example_module(label = "example 1"),
    example_module2(label = "example 2")
  )
  testthat::expect_s3_class(result, "teal_modules")
  testthat::expect_length(result, 2)
})

testthat::test_that("example_module2 works with init()", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(IRIS = iris, MTCARS = mtcars),
      modules = example_module2()
    )
  )
})

testthat::test_that("example_module and example_module2 work together with init()", {
  testthat::expect_no_error(
    init(
      data = teal.data::teal_data(IRIS = iris, MTCARS = mtcars),
      modules = modules(
        example_module(label = "example 1"),
        example_module2(label = "example 2")
      )
    )
  )
})
