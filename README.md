# Teal: Interactive Exploratory Data Analysis with Shiny Web-Applications <a href='https://github.com/insightsengineering/teal'><img src="man/figures/teal.png" align="right" height="139" style="max-width: 100%;"/></a  >

*teal* is a shiny-based interactive exploration framework for analyzing data. `teal` applications require app developers to specify:

<!-- markdownlint-disable MD007 MD030 -->
-   Data, which can be:
    -    CDISC data, commonly used for clinical trial reporting
    -    Independent datasets, for example from a `data.frame`
    -    Related datasets, for example a set of `data.frames` with key columns to enable data joins
    -    `MultiAssayExperiment` objects which are R data structures for representing and analyzing multi-omics experiments
-   `teal` modules:
    -   `teal modules` are shiny modules built within the `teal` framework that specify analysis to be performed. For example, it can be a module for exploring outliers in the data, or a module for visualizing the data in line plots. Although these can be created from scratch, lost of `teal` modules have been released and we recommend starting with modules found in the following packages:
        -   [`teal.modules.general`](https://github.com/insightsengineering/teal.modules.general): general modules for exploring relational/independent/CDISC data
        -   [`teal.modules.clinical`](https://github.com/insightsengineering/teal.modules.clinical): modules specific to CDISC data and clinical trial reporting
        -   [`teal.modules.hermes`](https://github.com/insightsengineering/teal.modules.hermes): modules for analyzing `MultiAssayExperiment` objects

<!-- markdownlint-enable MD007 MD030 -->
With the 2 components above, the `teal` package then provides:

- The ability to "pull" data into applications at runtime - for example from external data stores which require authentication
- A standardized method to allow application users to filter their data before it is passed into analysis modules 

Further features which enhance the `teal` framework are available in the [`teal.devel`](https://github.com/insightsengineering/teal.devel) package.

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/teal@*release")
```

In order to run many of the examples you will also need to install the [`scda`](https://github.com/insightsengineering/scda) package.

See package vignettes `browseVignettes(package = "teal")` for usage of this package.
