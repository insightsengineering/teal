testthat::test_that("get_metadata returns NULL if no metadata", {
  my_tdata <- new_tdata(data = list(iris = iris, mtcars = mtcars))
  testthat::expect_null(get_metadata(my_tdata, "iris"))
})

testthat::test_that("get_metadata returns NULL if no metadata for given dataset", {
  my_tdata <- new_tdata(
    data = list(iris = iris, mtcars = mtcars),
    metadata = list(mtcars = list(A = 1)))
  testthat::expect_null(get_metadata(my_tdata, "iris"))
})

testthat::test_that("get_metadata returns metadata for given dataset", {
  my_tdata <- new_tdata(
    data = list(iris = iris, mtcars = mtcars),
    metadata = list(mtcars = list(A = 1, B = 2)))
  testthat::expect_equal(get_metadata(my_tdata, "mtcars"), list(A = 1, B = 2))
})

testthat::test_that("get_metadata returns NULL if dataset doesn't exist", {
  my_tdata <- new_tdata(
    data = list(iris = iris, mtcars = mtcars),
    metadata = list(mtcars = list(A = 1, B = 2)))
  testthat::expect_null(get_metadata(my_tdata, "not_existing_df"))
})

testthat::test_that("get_code returns empty character if tdata object has no code", {
  my_tdata <- new_tdata(data = list(iris = iris, mtcars = mtcars))
  testthat::expect_equal("",isolate(get_code(my_tdata)))
})

testthat::test_that("get_code returns character of code if tdata object has code", {

  code_string <- c("iris <- head(iris)", "mtcars <- head(mtcars)")

  #reactive case (for constructor)
  my_tdata <- new_tdata(
    data = list(x = iris, mtcars = head(mtcars)),
    code = reactive(code_string)
  )
  testthat::expect_equal(isolate(get_code(my_tdata)), code_string)

  #not reactive case (for constructor)
  my_tdata <- new_tdata(
    data = list(x = iris, mtcars = head(mtcars)),
    code = code_string
  )
  testthat::expect_equal(isolate(get_code(my_tdata)), code_string)
})


testthat::test_that("tdata2env returns environment containing tdata contents ", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  my_tdata <- new_tdata(data = list(iris = iris, mae = reactive(miniACC)))

  my_env <- isolate(tdata2env(my_tdata))
  my_env_as_list <- as.list(my_env)
  testthat::expect_setequal(names(my_env_as_list), c("iris", "mae"))
  testthat::expect_equal(iris, my_env_as_list$iris)
  testthat::expect_equal(miniACC, my_env_as_list$mae)
})

testthat::test_that("tdata2env throws error if argument is not tdata", {
  testthat::expect_error(tdata2env(iris), "Must inherit from class 'tdata'")
})

testthat::test_that("get_join_keys returns NULL if no JoinKeys object exists inside tdata", {

  my_tdata <- new_tdata(data = list(iris = iris, mae = reactive(miniACC)))
  testthat::expect_null(get_join_keys(my_tdata))

})

testthat::test_that("get_join_keys returns JoinKeys object if it exists inside tdata", {

  jk <- teal.data::join_keys(teal.data::join_key("A", "B", c("id" = "fk")))

  my_tdata <- new_tdata(
    data = list(
      A = data.frame(id = 1:10, val = 1:10),
      B = data.frame(id = 1:10, val = 1:10, fk = 10:1)
    ),
    join_keys = jk
  )

  testthat::expect_equal(get_join_keys(my_tdata), jk)

})

