# `TealReportCard`

**\[experimental\]** Child class of
[`teal.reporter::ReportCard`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html)
that is used for `teal` specific applications. In addition to the parent
methods, it supports rendering `teal` specific elements such as the
source code, the encodings panel content and the filter panel content as
part of the meta data.

## Super class

[`teal.reporter::ReportCard`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html)
-\> `TealReportCard`

## Methods

### Public methods

- [`TealReportCard$append_src()`](#method-TealReportCard-append_src)

- [`TealReportCard$append_fs()`](#method-TealReportCard-append_fs)

- [`TealReportCard$append_encodings()`](#method-TealReportCard-append_encodings)

- [`TealReportCard$clone()`](#method-TealReportCard-clone)

Inherited methods

- [`teal.reporter::ReportCard$append_content()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-append_content)
- [`teal.reporter::ReportCard$append_html()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-append_html)
- [`teal.reporter::ReportCard$append_metadata()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-append_metadata)
- [`teal.reporter::ReportCard$append_plot()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-append_plot)
- [`teal.reporter::ReportCard$append_rcode()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-append_rcode)
- [`teal.reporter::ReportCard$append_table()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-append_table)
- [`teal.reporter::ReportCard$append_text()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-append_text)
- [`teal.reporter::ReportCard$from_list()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-from_list)
- [`teal.reporter::ReportCard$get_content()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-get_content)
- [`teal.reporter::ReportCard$get_metadata()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-get_metadata)
- [`teal.reporter::ReportCard$get_name()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-get_name)
- [`teal.reporter::ReportCard$initialize()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-initialize)
- [`teal.reporter::ReportCard$reset()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-reset)
- [`teal.reporter::ReportCard$set_content_names()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-set_content_names)
- [`teal.reporter::ReportCard$set_name()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-set_name)
- [`teal.reporter::ReportCard$to_list()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/ReportCard.html#method-to_list)

------------------------------------------------------------------------

### Method `append_src()`

Appends the source code to the `content` meta data of this
`TealReportCard`.

#### Usage

    TealReportCard$append_src(src, ...)

#### Arguments

- `src`:

  (`character(1)`) code as text.

- `...`:

  any `rmarkdown` `R` chunk parameter and its value. But `eval`
  parameter is always set to `FALSE`.

#### Returns

Object of class `TealReportCard`, invisibly.

#### Examples

    card <- TealReportCard$new()$append_src(
      "plot(iris)"
    )
    card$get_content()[[1]]

------------------------------------------------------------------------

### Method `append_fs()`

Appends the filter state list to the `content` and `metadata` of this
`TealReportCard`. If the filter state list has an attribute named
`formatted`, it appends it to the card otherwise it uses the default
[`yaml::as.yaml`](https://yaml.r-lib.org/reference/as.yaml.html) to
format the list. If the filter state list is empty, nothing is appended
to the `content`.

#### Usage

    TealReportCard$append_fs(fs)

#### Arguments

- `fs`:

  (`teal_slices`) object returned from
  [`teal_slices()`](https://insightsengineering.github.io/teal/reference/teal_slices.md)
  function.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `append_encodings()`

Appends the encodings list to the `content` and `metadata` of this
`TealReportCard`.

#### Usage

    TealReportCard$append_encodings(encodings)

#### Arguments

- `encodings`:

  (`list`) list of encodings selections of the `teal` app.

#### Returns

`self`, invisibly.

#### Examples

    card <- TealReportCard$new()$append_encodings(list(variable1 = "X"))
    card$get_content()[[1]]

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TealReportCard$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `TealReportCard$append_src`
## ------------------------------------------------

card <- TealReportCard$new()$append_src(
  "plot(iris)"
)
#> Warning: `ReportCard$new()` was deprecated in teal.reporter 0.6.0.
#> ℹ Please use `teal_card()` instead.
#> ℹ Use teal_report class instead. See vignette('teal-report-class',
#>   'teal.reporter') for more information.
#> ℹ The deprecated feature was likely used in the R6 package.
#>   Please report the issue at <https://github.com/r-lib/R6/issues>.
card$get_content()[[1]]
#> [1] "plot(iris)"
#> attr(,"params")
#> list()
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"

## ------------------------------------------------
## Method `TealReportCard$append_encodings`
## ------------------------------------------------

card <- TealReportCard$new()$append_encodings(list(variable1 = "X"))
card$get_content()[[1]]
#> [1] "### Selected Options"
```
