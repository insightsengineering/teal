context("CallableFunction")

test_that("Test callable", {
  x_fun <- CallableFunction$new(mean)
  x_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )

  expect_false(
    identical(
      x_fun$get_call(),
      x_fun$get_call(args = list(x = c(1.0, 2.0), na.rm = TRUE))
    )
  )

  expect_identical(
    x_fun$run(),
    mean(c(1.0, 2.0, NA_real_), na.rm = TRUE)
  )

  args <- list(x = c(1.0, 2.0, NA_real_), na.rm = FALSE)
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

  x_fun$set_arg_value(name = "na.rm", value = FALSE)
  expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = FALSE)"
  )
})


test_that("test callable errors", {
  x <- 1

  expect_error(
    CallableFunction$new(x),
    "is.function"
  )

  expect_error(
    CallableFunction$new(),
    "is missing, with no default"
  )
  expect_equal({
    x <- CallableFunction$new(function(x = 1){
          return(x)
    })
    x$run()},
    1
  )

  x_fun <- CallableFunction$new(mean)

  # mean accepts extra arguments
  expect_silent(
    x_fun$set_args(list(y = 2, x = 1, na.rm = TRUE))
  )
  expect_identical(
    x_fun$run(),
    mean(y = 2, x = 1, na.rm = TRUE)
  )

  expect_error(
    CallableFunction$new(`+`)
  )

  expect_equal({
    x <- CallableFunction$new(base::all.equal)
    x$set_args(list(target = c("abc"), current = c("abc")))
    x$run()},
    TRUE
  )


  x_fun <- CallableFunction$new(abs)
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
    x_fun <- CallableFunction$new(callable)
    x_fun$.__enclos_env__$private$fun_name
  }

  expect_identical(
    fun(mean),
    "mean"
  )
})
