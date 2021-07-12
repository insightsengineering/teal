test_that("Function found", {
  expect_silent(
    cfun <- callable_function(fun = data.frame)
  )

  expect_identical(
    cfun$get_call(),
    "data.frame()"
  )

  expect_silent(
    cfun2 <- callable_function(fun = base::data.frame)
  )

  expect_identical(
    cfun2$get_call(),
    "data.frame()"
  )

  custom_fun <- function() {
    1L
  }
  expect_silent(
    cfun3 <- callable_function(
      fun = custom_fun
    )
  )

  expect_identical(
    cfun3$get_call(),
    "(function() {\n    1L\n})()"
  )

  expect_identical(
    cfun3$run(),
    1L
  )

  expect_silent(
    cfun4 <- callable_function(
      function() {
        library("MultiAssayExperiment")
        data("miniACC")
        return(miniACC)
      }
    )
  )

  expect_identical(
    cfun4$get_call(),
    "(function() {\n    library(\"MultiAssayExperiment\")\n    data(\"miniACC\")\n    return(miniACC)\n})()"
  )

  fun5 <- mean
  expect_silent(
    cfun5 <- callable_function(fun5)
  )
})

testthat::test_that("CallableFunction returns the correct name if passed a base function directly", {
  testthat::expect_equal(callable_function(print)$get_call(), "print()")
})

testthat::test_that("CallableFunction returns the correct name if passed an anonymous function", {
  testthat::expect_equal(callable_function(function() "test")$get_call(), "(function() \"test\")()")
})

testthat::test_that("CallableFunction returns the correct name if passed a name of a base function", {
  testthat::expect_equal(callable_function("print")$get_call(), "print()")
})

testthat::test_that("CallableFunction returns the correct name if passed a generic from a namespace", {
  testthat::expect_equal(callable_function(utils::head)$get_call(), "head()")
})

testthat::test_that("CallableFunction returns the correct name if passed a prefixed name of a function", {
  testthat::expect_equal(callable_function("base::print")$get_call(), "base::print()")
})

testthat::test_that("CallableFunction returns the correct name if passed a namespace function indirectly", {
  x <- print
  testthat::expect_equal(callable_function(x)$get_call(), "print()")
})

testthat::test_that("CallableFunction returns the correct name if passed a function from global env indirectly", {
  x <- function() "test"
  testthat::expect_equal(callable_function(x)$get_call(), "(function() \"test\")()")
})

testthat::test_that("CallableFunction returns the correct name if passed a function name indirectly", {
  x <- "print"
  testthat::expect_equal(callable_function(x)$get_call(), "print()")
})

testthat::test_that(
  "CallableFunction throws an error if passed a namespace function via a binding in the parent frame", {
    x <- print
    testthat::expect_error(callable_function("x")$get_call(), "object 'x' of mode 'function' was not found")
  }
)

testthat::test_that("CallableFunction returns the correct name if passed a Primitive directly", {
  testthat::expect_equal(callable_function(.Primitive("+"))$get_call(), ".Primitive(\"+\")()")
})

testthat::test_that("CallableFunction throws an error if passed a Primitive by character", {
  testthat::expect_error(callable_function(".Primitive('+')")$get_call())
})

testthat::test_that("CallableFunction throws an error if passed a prefixed object (not a function)", {
  testthat::expect_error(
    callable_function("datasets::iris")$get_call(),
    regexp = "object 'datasets::iris' of mode 'function' was not found"
  )
})

test_that("Test inputs", {
  x_fun <- callable_function("mean")
  x_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )

  fun <- as.symbol("mean")
  y_fun <- callable_function(fun)
  y_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  expect_identical(
    y_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )

  z_fun <- callable_function(base::mean)
  z_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  expect_identical(
    z_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )
})

test_that("Test callable", {
  x_fun <- callable_function(base::mean)
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
    "CallableFunction can be specified as character, symbol, call or function"
  )

  expect_error(
    callable_function("x"),
    "object 'x' of mode 'function' was not found"
  )

  expect_error(
    callable_function(garbageIn),
    "not found"
  )

  expect_error(
    callable_function("garbageIn"),
    "object 'garbageIn' of mode 'function' was not found"
  )

  expect_error(
    callable_function(),
    "A valid function name must be provided."
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

test_that("is failed", {
  fun <- callable_function(sqrt)
  expect_error(
    fun$run(args = list(x = "")),
    "non-numeric argument to mathematical function"
  )
  expect_s3_class(
    fun$run(args = list(x = ""), try = TRUE),
    "error"
  )
  expect_true(fun$is_failed())
  expect_identical(
    fun$get_error_message(),
    "non-numeric argument to mathematical function"
  )

  expect_silent(fun$run(args = list(x = 1.5)))
  expect_false(fun$is_failed())
  expect_identical(
    fun$get_error_message(),
    character(0)
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
    fun(base::mean),
    "mean"
  )
})

test_that("test cloning", {
  fun <- callable_function(stats::sd)
  fun$set_args(list(x = call(":", as.name("x1"), as.name("x2"))))
  fun$assign_to_env(x = "x1", value = 0)
  fun$assign_to_env(x = "x2", value = 10)
  expect_identical(
    fun$get_call(),
    "sd(x = x1:x2)"
  )

  expect_identical(
    ls(envir = fun$.__enclos_env__$private$env),
    c("x1", "x2")
  )

  expect_identical(
    fun$run(),
    stats::sd(0:10)
  )

  fun_cloned <- fun$clone()
  expect_identical(
    fun$.__enclos_env__$private$env,
    fun_cloned$.__enclos_env__$private$env
  )

  fun_cloned_deep <- fun$clone(deep = TRUE)
  expect_false(
    identical(
      fun$.__enclos_env__$private$env,
      fun_cloned_deep$.__enclos_env__$private$env
    )
  )


})
