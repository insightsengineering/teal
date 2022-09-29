testthat::test_that("get_code_tdata accepts tdata", {
  data <- new_tdata(data = list(iris = iris), code = "iris <- iris")
  testthat::expect_error(isolate(get_code_tdata(data)), NA)
})

testthat::test_that("get_code_tdata throws error when input is not tdata", {
  testthat::expect_error(
    isolate(get_code_tdata(iris)),
    "Assertion on 'data' failed: Must inherit from class 'tdata', but has class 'data.frame'."
  )

  testthat::expect_error(
    isolate(get_code_tdata("iris")),
    "Assertion on 'data' failed: Must inherit from class 'tdata', but has class 'character'."
  )
})

testthat::test_that("get_code_tdata returns character code", {
  data <- new_tdata(data = list(iris = iris), code = "iris <- iris")
  testthat::expect_identical(isolate(get_code_tdata(data)), "iris <- iris")
})

