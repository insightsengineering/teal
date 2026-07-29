# Calls expression when condition is met

Function postpones `handlerExpr` to the moment when `eventExpr`
(condition) returns `TRUE`, otherwise nothing happens.

## Usage

``` r
.call_once_when(
  eventExpr,
  handlerExpr,
  event.env = parent.frame(),
  handler.env = parent.frame(),
  ...
)
```

## Arguments

- eventExpr:

  A (quoted or unquoted) logical expression that represents the event;
  this can be a simple reactive value like input\$click, a call to a
  reactive expression like dataset(), or even a complex expression
  inside curly braces.

- handlerExpr:

  The expression to call whenever `eventExpr` is invalidated. This
  should be a side-effect-producing action (the return value will be
  ignored). It will be executed within an
  [`isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html) scope.

- event.env:

  The parent environment for the reactive expression. By default, this
  is the calling environment, the same as when defining an ordinary
  non-reactive expression. If `eventExpr` is a quosure and
  `event.quoted` is `TRUE`, then `event.env` is ignored.

- handler.env:

  The parent environment for the reactive expression. By default, this
  is the calling environment, the same as when defining an ordinary
  non-reactive expression. If `handlerExpr` is a quosure and
  `handler.quoted` is `TRUE`, then `handler.env` is ignored.

- ...:

  additional arguments passed to `observeEvent` with the exception of
  `eventExpr` that is not allowed.

## Value

An observer.
