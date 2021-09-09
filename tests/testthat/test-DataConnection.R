library(scda)

testthat::test_that("data connection", {
  open_fun <- callable_function(data.frame)
  open_fun$set_args(list(x = 1:5))

  close_fun <- callable_function(data.frame)
  close_fun$set_args(list(x = 1:2))

  con <- DataConnection$new(open_fun = open_fun, close_fun = close_fun)
  con$set_open_args(args = list(y = letters[1:5]))

  testthat::expect_identical(
    as.list(con$get_open_call(deparse = FALSE)),
    list(as.name("data.frame"), x = 1:5, y = letters[1:5])
  )

  testthat::expect_identical(
    con$get_open_call(),
    "data.frame(x = 1:5, y = c(\"a\", \"b\", \"c\", \"d\", \"e\"))"
  )


  testthat::expect_false(con$is_opened())
  con$open()
  testthat::expect_true(con$is_opened())

  # passing arguments to open doesn't overwrite args
  con$open(args = list(x = 1:5, y = LETTERS[1:5]))
  testthat::expect_identical(
    as.list(con$get_open_call(deparse = FALSE)),
    list(as.name("data.frame"), x = 1:5, y = letters[1:5])
  )


  testthat::expect_identical(
    con$get_open_call(),
    "data.frame(x = 1:5, y = c(\"a\", \"b\", \"c\", \"d\", \"e\"))"
  )

  testthat::expect_silent(
    con$close(silent = TRUE)
  )
  testthat::expect_false(con$is_opened())
})
