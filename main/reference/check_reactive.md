# Check that argument is reactive.

Check that argument is reactive.

## Usage

``` r
check_reactive(x, null.ok = FALSE)

test_reactive(x, null.ok = FALSE)

assert_reactive(
  x,
  null.ok = FALSE,
  .var.name = checkmate::vname(x),
  add = NULL
)
```

## Arguments

- x:

  \[`any`\]  
  Object to check.

- null.ok:

  \[`logical(1)`\]  
  If set to `TRUE`, `x` may also be `NULL`. In this case only a type
  check of `x` is performed, all additional checks are disabled.

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]  
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## Value

Depending on the function prefix: If the check is successful, the
functions `assertClass`/`assert_class` return `x` invisibly, whereas
`checkClass`/`check_class` and `testClass`/`test_class` return `TRUE`.
If the check is not successful, `assertClass`/`assert_class` throws an
error message, `testClass`/`test_class` returns `FALSE`, and
`checkClass`/`check_class` return a string with the error message. The
function `expect_class` always returns an
[`expectation`](https://testthat.r-lib.org/reference/expectation.html).
