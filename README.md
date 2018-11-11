
# Teal: Interactive Exploratory Analysis Shiny Web-Applications

We are working on a next major version that we expect to release in mid 2019.

Please read more about teal on our agile-R website at [go.roche.com/agile-R](http://go.roche.com/agile-R).

*teal* is a shiny-based interactive exploration framework for analyzing clinical
trials data. `teal` currently provides a dynamic filtering facility and diverse
data viewers. `teal` shiny applications are built using standard [shiny
modules](https://shiny.rstudio.com/articles/modules.html).

# Getting Started

1. Install `teal` as described in the [installation section](#installation) below.
1. Create a new file `app.R`, and paste this code into it:

	```r
	library(teal)
		
	ASL <- generate_sample_data('ASL')
	ARS <- generate_sample_data('ARS')
	ATE <- generate_sample_data('ATE')
	
	x <- teal::init(
	  data =  list(ASL = ASL, ARS = ARS, ATE = ATE),
	  modules = root_modules(
	    module(
	      "data source",
	      server = function(input, output, session, datasets) {},
	      ui = function(id) div(p("information about data source")),
	      filters = NULL
	    ),
	    tm_data_table(),
	    tm_variable_browser(),
	    modules(
	      label = "analysis items",
	      tm_table(
	        label = "demographic table",
	        dataname = "ASL",
	        xvar = "SEX",
	        yvar = "RACE",
	        yvar_choices = c("RACE", "AGEGR", "REGION")
	      ),
	      tm_scatterplot(
	        label = "scatterplot",
	        dataname = "ASL",
	        xvar = "AGE",
	        yvar = "BBMI",
	        color_by = "_none_",
	        color_by_choices = c("_none_", "STUDYID")
	      ),
	      module(
	        label = "survival curves",
	        server = function(input, output, session, datasets) {},
	        ui = function(id) div(p("Kaplan Meier Curve")),
	        filters = "ATE"
	      )
	    )
	  ),
	  header = tags$h1("Sample App"),
	  footer = tags$p("Copyright 2017")
	)
	
	shinyApp(x$ui, x$server)
	```
	
1. Execute the above code to run the sample app.
1. You can now modify this sample app to fit your needs. For example, delete
   `ARS` and `ATE`, and change `ASL` to your dataset, for example:

	```r
	ASL <- read_bce("/opt/BIOSTAT/qa/ts00010/libraries/asl.sas7bdat")
	```
   
1. Edit the modules in the `modules = ` argument. For example, you can
   rearrange, delete, or add new modules
1. Push this app to the shiny server (only you can access), with:

	```r
	## setwd() to location of app.R
	rocheBCE::shinypub("./", "users/your-unix-id/sample-app")
	```

1. For  more info, run this vignette (paste into R console and run):

	```r
	vignette("01_getting_started", package = "teal")
	```
    
    
# Installation 

## Stable Release

While on the Roche network, [open R or RStudio](https://r.roche.com) and execute
the following:

```r
# install.packages("devtools")
library(devtools)
install_github(
  'Rpackages/teal', ref = "v0.0.4",
  host = 'https://github.roche.com/api/v3',
  upgrade_dependencies = FALSE,
  build_vignettes = TRUE
)
```

## Development Version

Please read the [web-manual for the development version](https://pages.github.roche.com/Rpackages/teal/dev/). To install the development version use 

```r
devtools::install_github(
  'Rpackages/teal', ref = "master",
  host = 'https://github.roche.com/api/v3',
  upgrade_dependencies = FALSE,
  build_vignettes = TRUE
)
```

# More Teal Modules

Where there are quite a few teal modules in the wild, we are currently working
on high-quality reusable teal modules which are available in the
[teal.tern](https://pages.github.roche.com/Rpackages/teal.tern)
package.

# Documentation

Currently there are a number of resources with documentation:

1. The [agile-R website](http://go.roche.com/agile-R)

1. The [project website](https://pages.github.roche.com/Rpackages/teal/)

1. The `teal` package vignettes run

    ```
    library(teal)
    vignettes(package = "teal")
    ```

1. The `teal` R manual pages

    ```
    help(package = "teal")
    ```
