context("cdisc_data")

test_that("Basic example - without code and check", {
  x <- 1
  attr(x, "keys") <- "test"

  expect_silent(cdisc_data(x, code = NULL, check = FALSE))
  expect_silent(cdisc_data(x, arg1 = x, arg2 = x, code = NULL, check = FALSE))
})

test_that("Basic example - with code without check", {
  x <- 1
  attr(x, "keys") <- "test"

  expect_silent(cdisc_data(ASL = x, code = "x <- 1; attr(x, 'keys') <- 'test'", check = FALSE))
  expect_silent(cdisc_data(ASL = x, arg1 = x, arg2 = x, code = "x <- 1; attr(x, 'keys') <- 'test'", check = FALSE))
})

test_that("Basic example - with code and check", {
  x <- 1
  attr(x, "keys") <- "test"

  expect_silent(cdisc_data(ASL = x, code = "x <- 1; attr(x, 'keys') <- 'test'", check = TRUE))
  expect_silent(cdisc_data(ASL = x, arg1 = x, arg2 = x, code = "x <- 1; attr(x, 'keys') <- 'test'", check = TRUE))
})

test_that("Check is skipped if code is empty", {
  x <- 1
  attr(x, "keys") <- "test"

  expect_silent(cdisc_data(ASL = x, check = TRUE))
  expect_silent(cdisc_data(ASL = x, code = NULL, check = TRUE))
  expect_silent(cdisc_data(ASL = x, code = "", check = TRUE))
})

test_that("Naming list elements", {
  x <- 1
  attr(x, "keys") <- "test"

  expect_identical(names(cdisc_data(x)), "ASL")
  expect_identical(names(cdisc_data(ASL = x, arg1 = x, arg2 = x)), c("ASL", "arg1", "arg2"))
})

test_that("List values", {
  x <- 1
  attr(x, "keys") <- "test"

  result <- cdisc_data(x)

  result_to_compare <- list(ASL = 1)
  attr(result_to_compare[["ASL"]], "keys") <- "test"
  attr(result_to_compare[["ASL"]], "dataname") <- "ASL"
  attr(result_to_compare[["ASL"]], "source") <- "# !!! Preprocessing code is empty"

  expect_identical(result, result_to_compare)


  x1 <- 1
  attr(x1, "keys") <- "test"
  x2 <- 2
  attr(x2, "keys") <- "test"
  x3 <- 3
  attr(x3, "keys") <- "test"
  result <- cdisc_data(x1, arg2 = x2, arg3 = x3)


  result_to_compare <- list(ASL = 1, arg2 = 2, arg3 = 3)
  attr(result_to_compare[["ASL"]], "keys") <- "test"
  attr(result_to_compare[["arg2"]], "keys") <- "test"
  attr(result_to_compare[["arg3"]], "keys") <- "test"
  attr(result_to_compare[["ASL"]], "dataname") <- "ASL"
  attr(result_to_compare[["arg2"]], "dataname") <- "arg2"
  attr(result_to_compare[["arg3"]], "dataname") <- "arg3"
  attr(result_to_compare[["ASL"]], "source") <- "# !!! Preprocessing code is empty"
  attr(result_to_compare[["arg2"]], "source") <- "# !!! Preprocessing code is empty"
  attr(result_to_compare[["arg3"]], "source") <- "# !!! Preprocessing code is empty"

  expect_identical(result, result_to_compare)
})

test_that("Empty code", {
  x <- 1
  attr(x, "keys") <- "test"

  # missing code
  result <- cdisc_data(x, check = FALSE)
  expect_identical(attr(result[[1]], "source"), "# !!! Preprocessing code is empty")

  # NULL code
  result <- cdisc_data(x, code = NULL, check = FALSE)
  expect_identical(attr(result[[1]], "source"), "# !!! Preprocessing code is empty")

  # empty code
  result <- cdisc_data(x, code = "", check = FALSE)
  expect_identical(attr(result[[1]], "source"), "# !!! Preprocessing code is empty")

  # multiple datasets
  # missing code
  result <- cdisc_data(x, arg1 = x, arg2 = x, check = FALSE)
  expect_identical(attr(result$ASL, "source"), "# !!! Preprocessing code is empty")
  expect_identical(attr(result$arg1, "source"), "# !!! Preprocessing code is empty")
  expect_identical(attr(result$arg2, "source"), "# !!! Preprocessing code is empty")

  # NULL code
  result <- cdisc_data(x, arg1 = x, arg2 = x, code = NULL, check = FALSE)
  expect_identical(attr(result$ASL, "source"), "# !!! Preprocessing code is empty")
  expect_identical(attr(result$arg1, "source"), "# !!! Preprocessing code is empty")
  expect_identical(attr(result$arg2, "source"), "# !!! Preprocessing code is empty")

  # empty code
  result <- cdisc_data(x, arg1 = x, arg2 = x, code = "", check = FALSE)
  expect_identical(attr(result$ASL, "source"), "# !!! Preprocessing code is empty")
  expect_identical(attr(result$arg1, "source"), "# !!! Preprocessing code is empty")
  expect_identical(attr(result$arg2, "source"), "# !!! Preprocessing code is empty")
})

# test_that("Non empty code", {
#   x <- 1
#   attr(x, "keys") <- "test"
#
#   result <- cdisc_data(x, code = "test code", check = FALSE)
#   expect_identical(attr(result[[1]], "source"), "test code")
#
#   result <- cdisc_data(x, arg1 = x, arg2 = x, code = "test code", check = FALSE)
#   expect_identical(attr(result$ASL, "source"), "test code")
#   expect_identical(attr(result$arg1, "source"), "test code")
#   expect_identical(attr(result$arg2, "source"), "test code")
# })

test_that("Arguments creaded by code", {
  result <- cdisc_data(x, code = "x <- 1; attr(x, 'keys') <- 'test'", check = FALSE)

  result_to_compare <- list(ASL = 1)
  attr(result_to_compare[["ASL"]], "keys") <- "test"
  attr(result_to_compare[["ASL"]], "dataname") <- "ASL"
  attr(result_to_compare[["ASL"]], "source") <- "x <- 1; attr(x, 'keys') <- 'test'"

  expect_identical(result, result_to_compare)


  code <- paste(
    "x <- 1; attr(x, 'keys') <- 'test';",
    "y <- 2; attr(y, 'keys') <- 'test';",
    "z <- 3; attr(z, 'keys') <- 'test';"
  )
  result <- cdisc_data(x, arg2 = y, arg3 = z, code = code, check = FALSE)

  result_to_compare <- list(ASL = 1, arg2 = 2, arg3 = 3)
  attr(result_to_compare[["ASL"]], "keys") <- "test"
  attr(result_to_compare[["arg2"]], "keys") <- "test"
  attr(result_to_compare[["arg3"]], "keys") <- "test"
  attr(result_to_compare[["ASL"]], "dataname") <- "ASL"
  attr(result_to_compare[["arg2"]], "dataname") <- "arg2"
  attr(result_to_compare[["arg3"]], "dataname") <- "arg3"
  attr(result_to_compare[["ASL"]], "source") <- code
  attr(result_to_compare[["arg2"]], "source") <- code
  attr(result_to_compare[["arg3"]], "source") <- code

  expect_identical(result, result_to_compare)
})

test_that("Error - ASL is missing", {
  expect_error(cdisc_data(arg1 = 1, code = NULL, check = FALSE), "ASL and code arguments are missing")
  expect_error(cdisc_data(code = "x <- 2", check = FALSE), "ASL is missing and cannot be generated by code")
})

test_that("Error - checking is forbidden if any argument is call", {
  expect_error(
    cdisc_data(1 + 2, code = "test code", check = TRUE),
    "Automatic checking is not supported if arguments provided as calls"
  )


  x <- 1
  attr(x, "keys") <- "test"

  expect_error(
    cdisc_data(1 + 2, code = "test code", check = TRUE),
    "Automatic checking is not supported if arguments provided as calls"
  )


  expect_error(
    cdisc_data(foo(1), code = "test code", check = TRUE),
    "Automatic checking is not supported if arguments provided as calls"
  )
})

test_that("Error - not named arguments", {
  x <- 1
  expect_error(
    cdisc_data(x, x, code = NULL, check = FALSE),
    "All arguments passed to '...' should be named"
  )
  expect_error(
    cdisc_data(y, y, code = "y <- 1", check = FALSE),
    "All arguments passed to '...' should be named"
  )
})

test_that("Error - no keys attribute", {
  x <- 1
  expect_error(
    cdisc_data(x, code = NULL, check = FALSE),
    "Cannot find 'keys' attribute"
  )

  x <- 1
  attr(x, "keys") <- "test"
  y <- 2
  expect_error(
    cdisc_data(x, arg1 = y, code = NULL, check = FALSE),
    "Cannot find 'keys' attribute"
  )

  x <- 1
  attr(x, "keys") <- "test"
  expect_error(
    cdisc_data(x, arg1 = z, code = "z <- 3", check = FALSE),
    "Cannot find 'keys' attribute"
  )
})

test_that("Error - keys do not match", {
  x1 <- 1
  attr(x1, "keys") <- "test1"

  x2 <- 1
  attr(x2, "keys") <- "test2"

  code <- paste(
    "x1 <- 1; attr(x1, 'keys') <- 'test1';",
    "x2 <- 1; attr(x2, 'keys') <- 'test2';"
  )

  expect_error(
    cdisc_data(x1, arg1 = x2, code = code, check = FALSE),
    "Cannot find match of .* keys"
  )
})

# test_that("Error - cannot reproduce object", {
#   x1 <- 1
#   attr(x1, "keys") <- "test"
#
#   x2 <- 2
#   attr(x2, "keys") <- "test"
#
#   code <- paste(
#     "x1 <- 10; attr(x1, 'keys') <- 'test';",
#     "x2 <- 20; attr(x2, 'keys') <- 'test';"
#   )
#
#   expect_error(
#     cdisc_data(x1, arg1 = x2, code = code, check = TRUE),
#     "Cannot reproduce"
#   )
# })
