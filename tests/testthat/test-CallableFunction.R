testthat::test_that("Function found", {
  testthat::expect_silent(
    cfun <- callable_function(fun = data.frame)
  )

  testthat::expect_identical(
    cfun$get_call(),
    "data.frame()"
  )

  testthat::expect_silent(
    cfun2 <- callable_function(fun = base::data.frame)
  )

  testthat::expect_identical(
    cfun2$get_call(),
    "data.frame()"
  )

  custom_fun <- function() {
    1L
  }
  testthat::expect_silent(
    cfun3 <- callable_function(
      fun = custom_fun
    )
  )

  testthat::expect_identical(
    cfun3$get_call(),
    "(function() {\n    1L\n})()"
  )

  testthat::expect_identical(
    cfun3$run(),
    1L
  )

  testthat::expect_silent(
    cfun4 <- callable_function(
      function() {
        library("MultiAssayExperiment")
        data("miniACC")
        return(miniACC)
      }
    )
  )

  testthat::expect_identical(
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
  testthat::expect_equal(callable_function(utils::head)$get_call(), "utils::head()")
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
  "CallableFunction throws an error if passed a namespace function via a binding in the parent frame",
  code = {
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

testthat::test_that("Test inputs", {
  x_fun <- callable_function("mean")
  x_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  testthat::expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )

  fun <- as.symbol("mean")
  y_fun <- callable_function(fun)
  y_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  testthat::expect_identical(
    y_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )

  z_fun <- callable_function(base::mean)
  z_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  testthat::expect_identical(
    z_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )
})

testthat::test_that("Test callable", {
  x_fun <- callable_function(base::mean)
  x_fun$set_args(list(x = c(1.0, 2.0, NA_real_), na.rm = TRUE))

  testthat::expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE)"
  )

  testthat::expect_identical(
    x_fun$get_args(),
    list(
      x = c(1.0, 2.0, NA_real_),
      na.rm = TRUE
    )
  )

  # get_call doesn't change args persistently
  testthat::expect_false(
    identical(
      x_fun$get_call(),
      x_fun$get_call(args = list(x = c(1.0, 2.0), na.rm = TRUE))
    )
  )

  # args are still as in the beginning
  testthat::expect_identical(
    x_fun$get_args(),
    list(
      x = c(1.0, 2.0, NA_real_),
      na.rm = TRUE
    )
  )

  testthat::expect_identical(
    x_fun$run(),
    mean(c(1.0, 2.0, NA_real_), na.rm = TRUE)
  )

  # run doesn't change args persistently
  args <- list(na.rm = FALSE)
  testthat::expect_false(
    identical(
      x_fun$run(),
      x_fun$run(args = args)
    )
  )

  testthat::expect_false(
    identical(
      x_fun$.__enclos_env__$private$args,
      args
    )
  )

  # args can be changed persistently by set_arg_value()
  x_fun$set_arg_value(name = "na.rm", value = FALSE)
  testthat::expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = FALSE)"
  )

  # args can be changed/added persistently by set_args()
  x_fun$set_args(list(na.rm = TRUE, trim = 0.3))

  testthat::expect_identical(
    x_fun$get_call(),
    "mean(x = c(1, 2, NA), na.rm = TRUE, trim = 0.3)"
  )


  # try
  testthat::expect_identical(
    x_fun$run(try = TRUE),
    x_fun$run(try = FALSE)
  )

  testthat::expect_identical(
    x_fun$run(return = FALSE),
    NULL
  )

  # cleaning args
  x_fun$set_args(args = NULL)
  testthat::expect_null(x_fun$get_args())
})

testthat::test_that("test callable errors", {
  x <- 1

  testthat::expect_error(
    callable_function(x),
    "CallableFunction can be specified as character, symbol, call or function"
  )

  testthat::expect_error(
    callable_function("x"),
    "object 'x' of mode 'function' was not found"
  )

  testthat::expect_error(
    callable_function(garbageIn),
    "not found"
  )

  testthat::expect_error(
    callable_function("garbageIn"),
    "object 'garbageIn' of mode 'function' was not found"
  )

  testthat::expect_error(
    callable_function(),
    "A valid function name must be provided."
  )


  testthat::expect_silent(x_fun <- callable_function(mean))


  # mean accepts extra arguments
  testthat::expect_silent(
    x_fun$set_args(list(y = 2, x = 1, na.rm = TRUE))
  )
  testthat::expect_identical(
    x_fun$run(),
    mean(y = 2, x = 1, na.rm = TRUE)
  )

  testthat::expect_equal(
    object = {
      x <- callable_function(base::all.equal)
      x$set_args(list(target = c("abc"), current = c("abc")))
      x$run()
    },
    TRUE
  )


  x_fun <- callable_function(abs)
  testthat::expect_silent(
    x_fun$set_args(list(y = 2, x = 1, na.rm = TRUE))
  )
  testthat::expect_error(
    x_fun$run(),
    "3 arguments passed to"
  )
})

testthat::test_that("is failed", {
  fun <- callable_function(sqrt)
  testthat::expect_error(
    fun$run(args = list(x = "")),
    "non-numeric argument to mathematical function"
  )
  testthat::expect_output(
    testthat::expect_s3_class(
      fun$run(args = list(x = ""), try = TRUE),
      "error"
    )
  )
  testthat::expect_true(fun$is_failed())
  testthat::expect_identical(
    fun$get_error_message(),
    "non-numeric argument to mathematical function"
  )

  testthat::expect_silent(fun$run(args = list(x = 1.5)))
  testthat::expect_false(fun$is_failed())
  testthat::expect_identical(
    fun$get_error_message(),
    character(0)
  )
})

testthat::test_that("test cloning", {
  fun <- callable_function(stats::sd)
  fun$set_args(list(x = call(":", as.name("x1"), as.name("x2"))))
  fun$assign_to_env(x = "x1", value = 0)
  fun$assign_to_env(x = "x2", value = 10)
  testthat::expect_identical(
    fun$get_call(),
    "stats::sd(x = x1:x2)"
  )

  testthat::expect_identical(
    ls(envir = fun$.__enclos_env__$private$env),
    c("x1", "x2")
  )

  testthat::expect_identical(
    fun$run(),
    stats::sd(0:10)
  )

  fun_cloned <- fun$clone()
  testthat::expect_identical(
    fun$.__enclos_env__$private$env,
    fun_cloned$.__enclos_env__$private$env
  )

  fun_cloned_deep <- fun$clone(deep = TRUE)
  testthat::expect_false(
    identical(
      fun$.__enclos_env__$private$env,
      fun_cloned_deep$.__enclos_env__$private$env
    )
  )
})

testthat::test_that("get_binding_name throws if the function could not be found in the environment", {
  testthat::expect_error(get_binding_name("test", emptyenv()), regexp = "Object not found in the environment")
})
