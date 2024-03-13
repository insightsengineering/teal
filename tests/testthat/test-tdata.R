withr::local_options(lifecycle_verbosity = "quiet")

# ---- constructor ----
testthat::test_that("new_tdata accepts reactive and not reactive MAE and data.frames", {
  testthat::skip_if_not_installed("MultiAssayExperiment")
  utils::data(miniACC, package = "MultiAssayExperiment")

  testthat::expect_no_error(
    new_tdata(
      list(
        a = reactive(data.frame(x = 1:10)),
        b = data.frame(y = 1:10),
        c = reactive(miniACC),
        d = miniACC
      )
    )
  )
})

testthat::test_that("new_tdata throws error if data is not a list with unique names", {
  testthat::expect_error(
    new_tdata(data.frame(1:10)), "Must be of type 'list'"
  )

  testthat::expect_error(
    new_tdata(list(data.frame(1:10))), "Must have names"
  )

  testthat::expect_error(
    new_tdata(list(x = data.frame(1:10), x = data.frame(1:5))), "Must have unique names"
  )
})

testthat::test_that("new_tdata throws error if contents of data list are not of correct type", {
  testthat::expect_error(
    new_tdata(list(x = 1)), "May only contain the following types: \\{data.frame,reactive,MultiAssayExperiment\\}"
  )
})

testthat::test_that("new_tdata throws error if code is not character or reactive character", {
  testthat::expect_error(
    new_tdata(list(x = iris), code = 5),
    "Assertion on 'code' failed: Must inherit from class 'character'/'reactive'"
  )

  testthat::expect_error(
    new_tdata(list(x = iris), code = reactive(5)),
    "Assertion on 'code' failed: Must inherit from class 'character'"
  )
})

testthat::test_that("new_tdata accepts character and reactive characters for code argument", {
  testthat::expect_no_error(
    new_tdata(list(x = iris, y = mtcars), code = c("x <- iris", "y <- mtcars"))
  )

  testthat::expect_no_error(
    new_tdata(list(x = iris, y = mtcars), code = reactive(c("x <- iris", "y <- mtcars")))
  )
})

testthat::test_that("new_tdata throws error if join_keys is not of class join_keys", {
  testthat::expect_error(
    new_tdata(list(x = iris), join_keys = "x"),
    "Assertion on 'join_keys' failed: Must inherit from class 'join_keys'"
  )
})

testthat::test_that("new_tdata throws no error if join_keys is of class join_keys", {
  testthat::expect_no_error(
    new_tdata(list(x = iris), join_keys = teal.data::join_keys())
  )
})

# note not testing the contents of metadata elements are good as we are relying on
# the (tested) function in teal.data to do this
testthat::test_that(
  "new_tdata throws error if metadata is not a list with unique names a subset of the names of data",
  {
    testthat::expect_error(
      new_tdata(list(x = iris, y = mtcars), metadata = 1:3),
      "Assertion on 'metadata' failed: Must be of type 'list' \\(or 'NULL'\\)"
    )

    testthat::expect_error(
      new_tdata(list(x = iris, y = mtcars), metadata = list(1, 2, 3)),
      "Assertion on 'metadata' failed: Must have names."
    )

    testthat::expect_error(
      new_tdata(list(x = iris, y = mtcars), metadata = list(x = list(A = 1), z = list(B = 1))),
      "Must be a subset of \\{'x','y'\\}, but has additional elements \\{'z'\\}."
    )
  }
)

testthat::test_that("new_tdata does not throw error with valid metadata", {
  testthat::expect_no_error(
    new_tdata(list(x = iris, y = mtcars), metadata = list(x = list(A = 1), y = list(B = 1)))
  )
})

# ---- get_metadata ----
testthat::test_that("get_metadata returns NULL if no metadata", {
  my_tdata <- new_tdata(data = list(iris = iris, mtcars = mtcars))
  testthat::expect_null(get_metadata(my_tdata, "iris"))
})

testthat::test_that("get_metadata returns NULL if no metadata for given dataset", {
  my_tdata <- new_tdata(
    data = list(iris = iris, mtcars = mtcars),
    metadata = list(mtcars = list(A = 1))
  )
  testthat::expect_null(get_metadata(my_tdata, "iris"))
})

testthat::test_that("get_metadata returns metadata for given dataset", {
  my_tdata <- new_tdata(
    data = list(iris = iris, mtcars = mtcars),
    metadata = list(mtcars = list(A = 1, B = 2))
  )
  testthat::expect_equal(get_metadata(my_tdata, "mtcars"), list(A = 1, B = 2))
})

testthat::test_that("get_metadata returns NULL if dataset doesn't exist", {
  my_tdata <- new_tdata(
    data = list(iris = iris, mtcars = mtcars),
    metadata = list(mtcars = list(A = 1, B = 2))
  )
  testthat::expect_null(get_metadata(my_tdata, "not_existing_df"))
})

# ---- get_code ----
testthat::test_that("get_code returns empty character if tdata object has no code", {
  my_tdata <- new_tdata(data = list(iris = iris, mtcars = mtcars))
  testthat::expect_equal("", isolate(get_code_tdata(my_tdata)))
})

testthat::test_that("get_code returns character of code if tdata object has code", {
  code_string <- c("iris <- head(iris)", "mtcars <- head(mtcars)")

  # reactive case (for constructor)
  my_tdata <- new_tdata(
    data = list(x = iris, mtcars = head(mtcars)),
    code = reactive(code_string)
  )
  testthat::expect_equal(isolate(get_code_tdata(my_tdata)), code_string)

  # not reactive case (for constructor)
  my_tdata <- new_tdata(
    data = list(x = iris, mtcars = head(mtcars)),
    code = code_string
  )
  testthat::expect_equal(isolate(get_code_tdata(my_tdata)), code_string)
})

# ---- get_code wrapper ----

testthat::test_that("get_code_tdata accepts tdata", {
  data <- new_tdata(data = list(iris = iris), code = "iris <- iris")
  testthat::expect_no_error(isolate(get_code_tdata(data)))
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

# ---- tdata2env ----
testthat::test_that("tdata2env returns environment containing tdata contents ", {
  testthat::skip_if_not_installed("MultiAssayExperiment")
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

# ---- join_keys ----
testthat::test_that("join_keys returns NULL if no join_keys object exists inside tdata", {
  my_tdata <- new_tdata(data = list(iris = iris, mae = reactive(miniACC)))
  testthat::expect_null(join_keys(my_tdata))
})

testthat::test_that("join_keys returns join_keys object if it exists inside tdata", {
  jk <- teal.data::join_keys(teal.data::join_key("A", "B", c("id" = "fk")))

  my_tdata <- new_tdata(
    data = list(
      A = data.frame(id = 1:10, val = 1:10),
      B = data.frame(id = 1:10, val = 1:10, fk = 10:1)
    ),
    join_keys = jk
  )

  testthat::expect_equal(join_keys(my_tdata), jk)
})


# as_tdata ----
code <- c("iris <- iris", "mtcars <- mtcars")
data_tdata <- teal::new_tdata(list(iris = iris, mtcars = mtcars), code)
data_teal_data <- teal.data::teal_data(iris = iris, mtcars = mtcars, code = code)
data_reactive <- shiny::reactive(teal.data::teal_data(iris = iris, mtcars = mtcars, code = code))

testthat::test_that("as_tdata accepts all possible inputs", {
  testthat::expect_no_error(as_tdata(data_tdata))
  testthat::expect_no_error(as_tdata(data_teal_data))
  testthat::expect_no_error(as_tdata(data_reactive))
})

testthat::test_that("as_tdata always returns tdata object", {
  data_tdata_downgraded <- as_tdata(data_tdata)
  data_teal_data_downgraded <- as_tdata(data_teal_data)
  data_reactive_downgraded <- as_tdata(data_teal_data)

  testthat::expect_s3_class(data_tdata_downgraded, "tdata")
  testthat::expect_s3_class(data_teal_data_downgraded, "tdata")
  testthat::expect_s3_class(data_reactive_downgraded, "tdata")
})

testthat::test_that("datasets are maintained during conversion", {
  data_tdata_downgraded <- as_tdata(data_teal_data)

  datanames_teal_data <- sort(teal.data::datanames(data_teal_data))
  datanames_tdata <- sort(names(data_tdata_downgraded))

  testthat::expect_identical(datanames_teal_data, datanames_tdata)

  datasets_teal_data <- sapply(datanames_teal_data, function(x) teal.code::get_var(data_teal_data, x))
  datasets_tdata <- sapply(datanames_tdata, function(x) shiny::isolate(data_tdata_downgraded[[x]]()))

  testthat::expect_identical(datasets_teal_data, datasets_tdata)
})

testthat::test_that("as_tdata maintains code during conversion", {
  data_teal_data_downgraded <- as_tdata(data_teal_data)
  testthat::expect_identical(
    teal.code::get_code(data_teal_data),
    shiny::isolate(attr(data_teal_data_downgraded, "code")())
  )
})
