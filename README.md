
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `teal`: Interactive Exploratory Data Analysis with `Shiny` Web-Applications <a href='https://insightsengineering.github.io/teal/'><img src="man/figures/logo.svg" align="right" height="139" style="max-width: 100%; max-height: 139px;"/></a  >

<!-- start badges -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/teal?color=green)](https://cran.r-project.org/package=teal)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/teal?color=green)](https://cran.r-project.org/package=teal)
[![Last Month
Downloads](http://cranlogs.r-pkg.org/badges/last-month/teal?color=green)](https://cran.r-project.org/package=teal)
[![Last Week
Downloads](http://cranlogs.r-pkg.org/badges/last-week/teal?color=green)](https://cran.r-project.org/package=teal)

[![Check
🛠](https://github.com/insightsengineering/teal/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal/main/unit-test-report/)
[![Docs
📚](https://github.com/insightsengineering/teal/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal/)
[![Code Coverage
📔](https://raw.githubusercontent.com/insightsengineering/teal/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal/main/coverage-report/)

![GitHub
forks](https://img.shields.io/github/forks/insightsengineering/teal?style=social)
![GitHub repo
stars](https://img.shields.io/github/stars/insightsengineering/teal?style=social)

![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal)
![GitHub
contributors](https://img.shields.io/github/contributors/insightsengineering/teal)
![GitHub last
commit](https://img.shields.io/github/last-commit/insightsengineering/teal)
![GitHub pull
requests](https://img.shields.io/github/issues-pr/insightsengineering/teal)
![GitHub repo
size](https://img.shields.io/github/repo-size/insightsengineering/teal)
![GitHub language
count](https://img.shields.io/github/languages/count/insightsengineering/teal)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current
Version](https://img.shields.io/github/r-package/v/insightsengineering/teal/main?color=purple&label=package%20version)](https://github.com/insightsengineering/teal/tree/main)
[![Open
Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal?color=red&label=open%20issues)](https://github.com/insightsengineering/teal/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

`teal` is a `shiny`-based interactive exploration framework for
analyzing data. `teal` applications require app developers to specify:

<!-- markdownlint-disable MD007 MD030 -->

- Data, which can be:
  - CDISC data, commonly used for clinical trial reporting
  - Independent datasets, for example from a `data.frame`
  - Related datasets, for example a set of `data.frames` with key
    columns to enable data joins
  - `MultiAssayExperiment` objects which are `R` data structures for
    representing and analyzing multi-omics experiments
- `teal` modules:
  - `teal modules` are `shiny` modules built within the `teal` framework
    that specify analysis to be performed. For example, it can be a
    module for exploring outliers in the data, or a module for
    visualizing the data in line plots. Although these can be created
    from scratch, many `teal` modules have been released and we
    recommend starting with modules found in the following packages:
    - [`teal.modules.general`](https://insightsengineering.github.io/teal.modules.general/latest-tag/):
      general modules for exploring relational/independent/CDISC data
    - [`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/):
      modules specific to CDISC data and clinical trial reporting
    - [`teal.modules.hermes`](https://insightsengineering.github.io/teal.modules.hermes/latest-tag/):
      modules for analyzing `MultiAssayExperiment` objects

<!-- markdownlint-enable MD007 MD030 -->

A lot of the functionality of the `teal` framework derives from the
following packages:

<!-- markdownlint-disable MD007 MD030 -->

- [`teal.data`](https://insightsengineering.github.io/teal.data/latest-tag/):
  creating and loading the data needed for `teal` applications.
- [`teal.widgets`](https://insightsengineering.github.io/teal.widgets/latest-tag/):
  `shiny` components used within `teal`.
- [`teal.slice`](https://insightsengineering.github.io/teal.slice/latest-tag/):
  provides a filtering panel to allow filtering of data.
- [`teal.code`](https://insightsengineering.github.io/teal.code/latest-tag/):
  handles reproducibility of outputs.
- [`teal.logger`](https://insightsengineering.github.io/teal.logger/latest-tag/):
  standardizes logging within `teal` framework.
- [`teal.reporter`](https://insightsengineering.github.io/teal.reporter/latest-tag/):
  allows `teal` applications to generate reports.

Dive deeper into `teal` with our comprehensive video guide. Please click
the image below to start learning:

[![A Complete Guide to Getting Started with
teal](https://img.youtube.com/vi/N8ZamECICSI/0.jpg)](https://www.youtube.com/watch?v=N8ZamECICSI)

<!-- markdownlint-enable MD007 MD030 -->

## Installation

``` r
install.packages("teal")
```

Alternatively, you might also use the development version.

``` r
# install.packages("pak")
pak::pak("insightsengineering/teal")
```

## Usage

``` r
library(teal)

app <- init(
  data = teal_data(iris = iris),
  modules = list(
    module(
      label = "iris histogram",
      server = function(input, output, session, data) {
        updateSelectInput(
          session = session,
          inputId = "var",
          choices = names(data()[["iris"]])[1:4]
        )

        output$hist <- renderPlot({
          req(input$var)
          hist(x = data()[["iris"]][[input$var]])
        })
      },
      ui = function(id) {
        ns <- NS(id)
        list(
          selectInput(
            inputId = ns("var"),
            label = "Column name",
            choices = NULL
          ),
          plotOutput(outputId = ns("hist"))
        )
      }
    )
  )
)

shinyApp(app$ui, app$server)
```

### Try it out in Shinylive

[Open in
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0IAtKgAEAHgC0I2hFql2AkSIAmUUlBEBeEV2oB9Veva1GtAM5bppsz1yKRMIsoCu1OBe10z8+0scu3BQglEJFqKHo4aks+MBNzEQALc1IiAHNmGFi7YNCRMzhGADdCywAzZwgCUloSYwhUZ1JcESImxub89zNaiBbDKB4REF8851RDOABlKLhqgEkGpqC8vIKzHpJLdc2+0dWZDvnlGLAilmz9vIJEoloCd0toeDN2AfYeYGBY+LNYgF1-p8AIyIAAs-yuSn4glyqzapA6ABJkt5xFJGBRlIUAArUIjyEZw1YiTEAR3qyPOjBhJJCqPkAA9LO9Pt84tYAf8vocmkjqYDaSSAL5C0LCnKrZy0cqVaq9YzKIZEukQCySEQAOSmirFeS8PmJqwKbgWS0NdNCvNIxyer1i1Nitih+oiUVOAGEiNRnDBgs84JcjSSbncHh4tQBVAAy0Zd0MlltQ+NIAHl2ssEUcTto1QowAynXrQsWRMLRkKYTCBGZkhBWABBdDsYRI6UtVsFYqFHhgYX-IA)

<div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-full-screen="false" data-require-bs-caller="card()" data-require-bs-version="5" id="bslib-card-2335">
<div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
<iframe class="html-fill-item" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0IAtKgAEAHgC0I2hFql2AkSIAmUUlBEBeEV2oB9Veva1GtAM5bppsz1yKRMIsoCu1OBe10z8+0scu3BQglEJFqKHo4aks+MBNzEQALc1IiAHNmGFi7YNCRMzhGADdCywAzZwgCUloSYwhUZ1JcESImxub89zNaiBbDKB4REF8851RDOABlKLhqgEkGpqC8vIKzHpJLdc2+0dWZDvnlGLAilmz9vIJEoloCd0toeDN2AfYeYGBY+LNYgF1-p8AIyIAAs-yuSn4glyqzapA6ABJkt5xFJGBRlIUAArUIjyEZw1YiTEAR3qyPOjBhJJCqPkAA9LO9Pt84tYAf8vocmkjqYDaSSAL5C0LCnKrZy0cqVaq9YzKIZEukQCySEQAOSmirFeS8PmJqwKbgWS0NdNCvNIxyer1i1Nitih+oiUVOAGEiNRnDBgs84JcjSSbncHh4tQBVAAy0Zd0MlltQ+NIAHl2ssEUcTto1QowAynXrQsWRMLRkKYTCBGZkhBWABBdDsYRI6UtVsFYqFHhgYX-IA&amp;h=0" width="100%" height="600" allowfullscreen="" allow="autoplay" data-external="1"></iframe>
</div>
<bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
<template>Expand</template>
<button aria-expanded="false" aria-label="Expand card" class="bslib-full-screen-enter badge rounded-pill"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" style="height:1em;width:1em;fill:currentColor;" aria-hidden="true" role="img"><path d="M20 5C20 4.4 19.6 4 19 4H13C12.4 4 12 3.6 12 3C12 2.4 12.4 2 13 2H21C21.6 2 22 2.4 22 3V11C22 11.6 21.6 12 21 12C20.4 12 20 11.6 20 11V5ZM4 19C4 19.6 4.4 20 5 20H11C11.6 20 12 20.4 12 21C12 21.6 11.6 22 11 22H3C2.4 22 2 21.6 2 21V13C2 12.4 2.4 12 3 12C3.6 12 4 12.4 4 13V19Z"/></svg></button>
</bslib-tooltip>
<script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
</div>

## More resources

Please see
[`teal.gallery`](https://insightsengineering.github.io/teal.gallery/)
and [TLG Catalog](https://insightsengineering.github.io/tlg-catalog/) to
see examples of `teal` apps.

Please start with the [“Technical Blueprint”
article](https://insightsengineering.github.io/teal/latest-tag/articles/blueprint/index.html),
[“Getting Started”
article](https://insightsengineering.github.io/teal/latest-tag/articles/getting-started-with-teal.html),
and then other [package
vignettes](https://insightsengineering.github.io/teal/latest-tag/articles/index.html)
for more detailed guide.

## Getting help

If you encounter a bug or have a feature request, please file an issue.
For questions, discussions, and updates, use the `teal` channel in the
[`pharmaverse` slack workspace](https://pharmaverse.slack.com).

## Acknowledgment

This package is a result of a joint efforts by many developers and
stakeholders. We would like to thank everyone who contributed so far!

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over
time](https://starchart.cc/insightsengineering/teal.svg)](https://starchart.cc/insightsengineering/teal)

### Stargazers

[![Stargazers repo roster for
@insightsengineering/teal](http://reporoster.com/stars/insightsengineering/teal)](https://github.com/insightsengineering/teal/stargazers)

### Forkers

[![Forkers repo roster for
@insightsengineering/teal](http://reporoster.com/forks/insightsengineering/teal)](https://github.com/insightsengineering/teal/network/members)
