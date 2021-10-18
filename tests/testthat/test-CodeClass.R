library(scda)

cc <- teal:::CodeClass$new()

test_that("Basic example CodeClass", {
  expect_true(inherits(cc$set_code(c("foo <- function() {1}", "foo2 <- function() {2}")), "CodeClass"))
  expect_identical(cc$get_code(), "foo <- function() {\n    1\n}\nfoo2 <- function() {\n    2\n}")
  expect_equal(
    cc$get_code(deparse = FALSE),
    list(rlang::expr(foo <- function() {
      1
    }), rlang::expr(foo2 <- function() {
      2
    }))
  )
})

cc$set_code(c("ADSL <- synthetic_cdisc_data(\"latest\")$adsl", "ADSL$var <- 1"), "ADSL")
cc$set_code("ADSL$a <- foo()", "ADSL")

cc$set_code("ADSL_2 <- head(ADSL, 5)", "ADSL_2", deps = "ADSL")


cc$set_code("baz <- function() {2}")
cc$set_code("ADSL_2$a <- baz()", "ADSL_2")

test_that("example datasets", {
  expect_identical(
    cc$get_code("ADSL"),
    paste0("foo <- function() {\n    1\n}\nfoo2 <- function() {\n    2\n}\n",
           "ADSL <- synthetic_cdisc_data(\"latest\")$adsl\nADSL$var <- 1\nADSL$a <- foo()")
  )
})

adsl_code <- cc$get_code("ADSL", deparse = FALSE)
adsl2_code <- cc$get_code("ADSL_2", deparse = FALSE)

test_that("example datasets deps", {
  expect_true(all(adsl_code %in% adsl2_code))
  expect_equal(adsl2_code[!adsl2_code %in% adsl_code], list(
    rlang::expr(ADSL_2 <- head(ADSL, 5)), # nolint
    rlang::expr(baz <- function() {
      2
    }),
    rlang::expr(ADSL_2$a <- baz())
  ))
})

#########################################
#########################################
#########################################

x1 <- teal:::CodeClass$new()
x1$set_code("ADSL <- synthetic_cdisc_data(\"latest\")$adsl", "ADSL")

x2 <- teal:::CodeClass$new()
x2$set_code("ADSL_2 <- head(ADSL, 5)", "ADSL_2", "ADSL")

x <- teal:::CodeClass$new()
x$append(x1)
x$append(x2)

test_that("CodeClass append", {
  expect_identical(x$get_code(), paste0(c(x1$get_code(), x2$get_code()), collapse = "\n"))
  expect_identical(x$get_code(deparse = FALSE), append(x1$get_code(deparse = FALSE), x2$get_code(deparse = FALSE)))
  expect_identical(x$get_code(c("ADSL", "ADSL_2")),
    paste0(c(x$get_code("ADSL"), x2$get_code("ADSL_2")), collapse = "\n"))
})


x3 <- teal:::CodeClass$new()
x3$set_code("ADRS <- synthetic_cdisc_data(\"latest\")$adrs", "ADRS")
x$append(x3)


test_that("CodeClass append deps", {
  expect_identical(
    x$get_code(),
    paste0("ADSL <- synthetic_cdisc_data(\"latest\")$adsl\nADSL_2 <- head(ADSL, 5)\n",
      "ADRS <- synthetic_cdisc_data(\"latest\")$adrs")
  )
})

x$set_code("ADRS$x <- foo(ADSL$x)", c("ADRS"), deps = "ADSL")
x$set_code("", "ADRS")

test_that("CodeClass append deps", {
  expect_identical(
    x$get_code("ADRS"),
    paste0("ADSL <- synthetic_cdisc_data(\"latest\")$adsl\nADRS <- synthetic_cdisc_data(\"latest\")$adrs\n",
      "ADRS$x <- foo(ADSL$x)\n")
  )
  expect_equal(x$get_code("ADRS", deparse = FALSE), list(
    rlang::expr(ADSL <- synthetic_cdisc_data("latest")$adsl), # nolint
    rlang::expr(ADRS <- synthetic_cdisc_data("latest")$adrs), # nolint
    rlang::expr(ADRS$x <- foo(ADSL$x))
  ))
  expect_identical(
    x$get_code("ADSL"),
    "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
  )
})

test_that("Exception handling with dataname of *xyz", {
  x <- teal:::CodeClass$new()
  x$set_code("open_connection()", dataname = "*open")
  x$set_code("x1 <- foo()", dataname = "x1")
  x$set_code("x2 <- bar()", dataname = "x2")
  x$set_code("close_connection()", dataname = "*close")

  expect_identical(
    x$get_code(),
    "open_connection()\nx1 <- foo()\nx2 <- bar()\nclose_connection()"
  )
  expect_identical(
    x$get_code("x1"),
    "open_connection()\nx1 <- foo()\nclose_connection()"
  )
  expect_identical(
    x$get_code("x2"),
    "open_connection()\nx2 <- bar()\nclose_connection()"
  )

  # add mutation
  x$set_code("x1 <- baz(x1)", dataname = "x1")
  expect_identical(
    x$get_code(),
    "open_connection()\nx1 <- foo()\nx2 <- bar()\nclose_connection()\nx1 <- baz(x1)"
  )
  expect_identical(
    x$get_code("x1"),
    "open_connection()\nx1 <- foo()\nclose_connection()\nx1 <- baz(x1)"
  )
  expect_identical(
    x$get_code("x2"),
    "open_connection()\nx2 <- bar()\nclose_connection()"
  )
})

adsl <- scda_cdisc_dataset_connector("ADSL", "adsl")
adae <- scda_cdisc_dataset_connector("ADAE", "adae")
adaem <- adae %>% mutate_dataset("ADAE$vv=nrow(ADSL); attr(ADSL$vv, 'label') <- 'vv'", vars = list(ADSL = adsl))
adae <- scda_cdisc_dataset_connector("ADAE", "adae")
adaem2 <- adae %>% mutate_dataset("ADAE$vv=nrow(ADSL); attr(ADSL$vv, 'label') <- 'vv'", vars = list(ADSL = ""))

test_that("CodeClass list_to_code_class", {
  expect_true(inherits(adaem$get_code_class(), "CodeClass"))
  expect_true(inherits(adaem2$get_code_class(), "CodeClass"))
})

# Append duplicated code ====
test_that("Duplicated code is appended if it doesn't have a dataname", {
  cc1 <- CodeClass$new(code = "print('test')")
  cc2 <- CodeClass$new(code = "print('test')")
  cc1$append(cc2)
  expect_equal(
    cc1$get_code(),
    "print(\"test\")\nprint(\"test\")")
})

test_that("Duplicated code is not appended if its dataname is duplicated", {
  cc1 <- CodeClass$new(code = "print('test')", dataname = "test")
  cc2 <- CodeClass$new(code = "print('test')", dataname = "test")
  expect_equal(
    cc1$get_code(),
    "print(\"test\")")
})

test_that("Duplicated code is appended if its dataname is different", {
  cc1 <- CodeClass$new(code = "print('test')", dataname = "test1")
  cc2 <- CodeClass$new(code = "print('test')", dataname = "test2")
  cc1$append(cc2)
  expect_equal(
    cc1$get_code(),
    "print(\"test\")\nprint(\"test\")")
})

test_that("list_to_code_class: assigning dataname to the object name inside of the list", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun)

  pull_fun2 <- callable_function(data.frame)
  pull_fun2$set_args(args = list(head_integers = 1:6))
  t_dc2 <- dataset_connector("test_dc2", pull_fun2)

  load_dataset(t_dc)
  load_dataset(t_dc2)

  mutate_dataset(t_dc, "t_dc2 <- NULL", vars = list(t_dc2 = t_dc2))
  expect_equal(
    pretty_code_string(t_dc$get_code()),
    c("test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "test_dc2 <- data.frame(head_integers = 1:6)",
      "t_dc2 <- test_dc2",
      "t_dc2 <- NULL"
    )
  )

  ds <- Dataset$new("head_mtcars", x = head(mtcars), code = "head_mtcars <- head(mtcars)")

  mutate_dataset(t_dc, "test_dc$carb <- ds$carb", vars = list(ds = ds))
  expect_equal(
    pretty_code_string(t_dc$get_code()),
    c("test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "test_dc2 <- data.frame(head_integers = 1:6)",
      "t_dc2 <- test_dc2",
      "head_mtcars <- head(mtcars)",
      "ds <- head_mtcars",
      "t_dc2 <- NULL",
      "test_dc$carb <- ds$carb"
    )
  )

  ds2 <- Dataset$new("head_iris", x = head(iris), code = "head_iris <- head(iris)")
  mutate_dataset(t_dc2, "test_dc2$Species <- head_iris$Species", vars = list(head_iris = ds2))
  expect_equal(
    pretty_code_string(t_dc2$get_code()),
    c("test_dc2 <- data.frame(head_integers = 1:6)",
      "head_iris <- head(iris)",
      "test_dc2$Species <- head_iris$Species"
    )
  )
})
