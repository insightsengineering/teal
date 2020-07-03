context("CallableFunction")

test_that("Test callable", {
  x_fun <- callable_function(mean)
  x_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )

  expect_identical(
    x_fun$get_args(),
    list(
      x = c(1.0, 2.0, NA_real_),
      na.rm = TRUE)
  )

  # get_call doesn't change args persistently
  expect_false(
    identical(
      x_fun$get_call(),
      x_fun$get_call(args = list(x = c(1.0, 2.0), na.rm = TRUE))
    )
  )

  # args are still as in the beginning
  expect_identical(
    x_fun$get_args(),
    list(
      x = c(1.0, 2.0, NA_real_),
      na.rm = TRUE)
  )

  expect_identical(
    x_fun$run(),
    mean(c(1.0, 2.0, NA_real_), na.rm = TRUE)
  )

  # run doesn't change args persistently
  args <- list(na.rm = FALSE)
  expect_false(
    identical(
      x_fun$run(),
      x_fun$run(args = args)
    )
  )

  expect_false(
    identical(
      x_fun$.__enclos_env__$private$args,
      args
    )
  )

  # args can be changed persistently by set_arg_value()
  x_fun$set_arg_value(name = "na.rm", value = FALSE)
  expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = FALSE)"
  )

  # args can be changed/added persistently by set_args()
  x_fun$set_args(list(na.rm = TRUE, trim = 0.3))

  expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE, trim = 0.3)"
  )


  # try
  expect_identical(
    x_fun$run(try = TRUE),
    x_fun$run(try = FALSE)
  )

  expect_identical(
    x_fun$run(return = FALSE),
    NULL
  )

  # cleaning args
  x_fun$set_args(args = NULL)
  expect_null(x_fun$get_args())
})

test_that("test callable errors", {
  x <- 1

  expect_error(
    callable_function(x),
    "is.function"
  )

  expect_error(
    callable_function(),
    "is missing, with no default"
  )


  expect_silent(x_fun <- callable_function(mean))

  # mean accepts extra arguments
  expect_silent(
    x_fun$set_args(list(y = 2, x = 1, na.rm = TRUE))
  )
  expect_identical(
    x_fun$run(),
    mean(y = 2, x = 1, na.rm = TRUE)
  )

  expect_error(
    callable_function(`+`)
  )

  expect_equal({
    x <- callable_function(base::all.equal)
    x$set_args(list(target = c("abc"), current = c("abc")))
    x$run()},
    TRUE
  )


  x_fun <- callable_function(abs)
  expect_silent(
    x_fun$set_args(list(y = 2, x = 1, na.rm = TRUE))
  )
  expect_error(
    x_fun$run(),
    "3 arguments passed to"
  )
})

test_that("find callable function name", {

  fun <- function(fun) {
    fun1(fun)
  }

  fun1 <- function(callable) {
    fun2(callable)
  }

  fun2 <- function(callable) {
    x_fun <- callable_function(callable)
    x_fun$.__enclos_env__$private$fun_name
  }

  expect_identical(
    fun(mean),
    "mean"
  )
})
