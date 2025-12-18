# Disable the "Show R Code" global button in the UI

Convenience function that disables the user's ability to see the code of
the module.

## Usage

``` r
disable_src(x)
```

## Arguments

- x:

  (`teal_module`) a `teal_module` object.

## Value

modified data object that indicates that it should not show the "Show R
Code" button in the UI.

## Details

This is equivalent to setting the attribute `teal.enable_src` to `FALSE`
on the data object returned by the module.

## See also

[`disable_report()`](https://insightsengineering.github.io/teal/reference/disable_report.md)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEBiaQBFaAZyj06EAObSbRFYwIyS0qK6FO1DIwRAAmKkECaKhyikK0pOwC0tKhUKR+ALzSAO4JABZCnHDcAPppGdq40rSMtrE1dTY8uMnSIeFBNtLZHRFwNkkQKSlwAB6wqEGlfUHs1PZw1D3SfGDjk0HSXMuzcGs6AD4AfKm29tM2ntpt-BB3tErS7ELkouJS2jogbTaFEKwAILodjRAAkKlo1XBNhEUkYdwAvmBEQBdIA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEBiaQFkV1camoyYRACY24AZwFpUcxUNqk7ALS0s5QpFDSALzSAO4BABZCnHDcAPphEdq40rSMtO6+ufnuPLjB0k6uDoUxVW7uQRAhIXAAHrD2cGn1DuzUUPRw1NHSfGDtnQ7SAIzjZRWtHTBdPS5u-YPDo+OTK9MATPMVOgA+AHyhBYMOae6MBNoC-BACtErS7ELkouJS2joQBV3EkIKwAILodjeAAkKloOVh7hEUkYLwAvmB0QBdIA)

## Examples

``` r
# Disabling source on a single module
app <- init(
  data = within(teal_data(), iris <- iris),
  modules = modules(
    example_module(label = "example teal module") |> disable_src()
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
# Multiple modules
app <- init(
  data = within(teal_data(), iris <- iris),
  modules = modules(
    example_module(label = "example 1"),
    example_module(label = "example 2")
  ) |> disable_src()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
