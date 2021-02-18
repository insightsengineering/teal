library(random.cdisc.data)

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

cc$set_code(c("ADSL <- radsl()", "ADSL$var <- 1"), "ADSL")
cc$set_code("ADSL$a <- foo()", "ADSL")

cc$set_code("ADAE <- radae(ADSL = ADSL)", "ADAE", deps = "ADSL")


cc$set_code("baz <- function() {2}")
cc$set_code("ADAE$a <- baz()", "ADAE")

test_that("example datasets", {
  expect_identical(
    cc$get_code("ADSL"),
    "foo <- function() {\n    1\n}\nfoo2 <- function() {\n    2\n}\nADSL <- radsl()\nADSL$var <- 1\nADSL$a <- foo()"
  )
})

adsl_code <- cc$get_code("ADSL", deparse = FALSE)
adae_code <- cc$get_code("ADAE", deparse = FALSE)

test_that("example datasets deps", {
  expect_true(all(adsl_code %in% adae_code))
  expect_equal(adae_code[!adae_code %in% adsl_code], list(
    rlang::expr(ADAE <- radae(ADSL = ADSL)), # nolint
    rlang::expr(baz <- function() {
      2
    }),
    rlang::expr(ADAE$a <- baz())
  ))
})

#########################################
#########################################
#########################################

x1 <- teal:::CodeClass$new()
x1$set_code("ADSL <- radsl(cached = TRUE)", "ADSL")

x2 <- teal:::CodeClass$new()
x2$set_code("ADAE <- radae(ADSL = ADSL)", "ADAE", "ADSL")

x <- teal:::CodeClass$new()
x$append(x1)
x$append(x2)

test_that("CodeClass append", {
  expect_identical(x$get_code(), paste0(c(x1$get_code(), x2$get_code()), collapse = "\n"))
  expect_identical(x$get_code(deparse = FALSE), append(x1$get_code(deparse = FALSE), x2$get_code(deparse = FALSE)))
  expect_identical(x$get_code(c("ADSL", "ADAE")), paste0(c(x$get_code("ADSL"), x2$get_code("ADAE")), collapse = "\n"))
})


x3 <- teal:::CodeClass$new()
x3$set_code("ADRS <- radae(cached = TRUE)", "ADRS")
x$append(x3)


test_that("CodeClass append deps", {
  expect_identical(
    x$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADAE <- radae(ADSL = ADSL)\nADRS <- radae(cached = TRUE)"
  )
})

x$set_code("ADRS$x <- foo(ADSL$x)", c("ADRS"), deps = "ADSL")
x$set_code("", "ADRS")

test_that("CodeClass append deps", {
  expect_identical(
    x$get_code("ADRS"),
    "ADSL <- radsl(cached = TRUE)\nADRS <- radae(cached = TRUE)\nADRS$x <- foo(ADSL$x)\n"
  )
  expect_equal(x$get_code("ADRS", deparse = FALSE), list(
    rlang::expr(ADSL <- radsl(cached = TRUE)), # nolint
    rlang::expr(ADRS <- radae(cached = TRUE)), # nolint
    rlang::expr(ADRS$x <- foo(ADSL$x))
  ))
  expect_identical(
    x$get_code("ADSL"),
    "ADSL <- radsl(cached = TRUE)"
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

adsl <- rcd_cdisc_dataset_connector("ADSL", radsl)
adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
adaem <- adae %>% mutate_dataset("ADAE$vv=nrow(ADSL); attr(ADSL$vv, 'label') <- 'vv'", vars = list(ADSL = adsl))
adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
adaem2 <- adae %>% mutate_dataset("ADAE$vv=nrow(ADSL); attr(ADSL$vv, 'label') <- 'vv'", vars = list(ADSL = ""))

test_that("CodeClass list_to_code_class", {
  expect_true(inherits(adaem$get_code_class(), "CodeClass"))
  expect_true(inherits(adaem2$get_code_class(), "CodeClass"))
})

# Append duplicated code ====
# Regression test from https://github.roche.com/NEST/teal/issues/974
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
  cc1$append(cc2)
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
