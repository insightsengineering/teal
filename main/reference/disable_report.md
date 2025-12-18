# Disable the report for a `teal_module`

Convenience function that disables the user's ability to add the module
to the report previewer.

## Usage

``` r
disable_report(x)
```

## Arguments

- x:

  (`teal_module`) a `teal_module` object.

## Value

modified data object that indicates that it should disable the reporter
functionality.

## See also

[`disable_src()`](https://insightsengineering.github.io/teal/reference/disable_src.md)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEBiaQBFaAZyj06EAObTGcVEUalpJaVGk2Qk7UMjBEACYqIQJoqHKKQrSk7ALS0uFQpP4AvNIA7kkAFkKccNwA+hlZ2rjStIy28XUNNjy4qdJhkSE20rldUXA2KRBpaXAAHrCoIeUDIezU9nDUfdJ8YJPTIdJcq-NwGzoAPgB86bb2s24eXtod-BCPtErS7ELkouJS2jogHTZihBWABBdDsWIAEhUtFqUJsIikjEeAF8BGAUQBdIA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEBiaQBFaAZyj06EAObTGcVEUalpJaTBXU4qjUMjBEACYBcDYCaKhyikK0pOwC0tLhUKRQ0gC80gDuyQAWQpxw3AD6mdnauNK0jLYJDU02PLhpfhFRNnndkSE2qRDp6XAAHrDBcJVhg3Ds1PZw1P18YJPTIdIAjBsdXeNTMDNzPSFLK2v5G1unOwBMB106AD4AfBm29iGVbh4vNoBPxBEppOwhORROIpNodCAujZShBWABBdDsOIAEhUtHqOJsIikjFBAF8wGSALpAA)

- example-3:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEBiaQBFaAZyj06EAObTGcVEUbjnyz9NIAFjJoqAIhcopCtKTsAtLSACZQpFDSALzSAO7RAUKccNwA+kkp2rjStIy2ERVVNjy4cdIwRAkq1HA26c2t7Z2xEPHxcAAesKgdhS1tHezU9nDU3Xxgo+Md-gVL030r-IPSDU1uHl4i3QByBgAy1wL7tErS7ELkouJS2jogTTa5EKwAILodghAAkKlo5XBNhEUkY+wAvmBEQBdIA)

## Examples

``` r
# Disabling report on a single module
app <- init(
  data = within(teal_data(), iris <- iris),
  modules = modules(
    example_module(label = "example teal module") |> disable_report()
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Disabling report on multiple modules
app <- init(
  data = within(teal_data(), iris <- iris),
  modules = modules(
    example_module(label = "example 1"),
    example_module(label = "example 2")
  ) |> disable_report()
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
# Disabling reporting for the app
app <- init(
  data = within(teal_data(), iris <- iris),
  modules = modules(
    example_module(label = "example teal module")
  ),
  reporter = NULL
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
