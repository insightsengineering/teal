# Capture error and decorate error message.

Capture error and decorate error message.

## Usage

``` r
decorate_err_msg(x, pre = character(0), post = character(0))
```

## Arguments

- x:

  object to evaluate

- pre:

  (`character(1)`) A string to prepend to error message

- post:

  (`character(1)`) A string to append to error message

## Value

`x` if no error, otherwise throws error with decorated message
