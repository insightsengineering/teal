
# Teal: Interactive Exploratory Data Analysis with Shiny Web-Applications

We are working on a next major version that is data standard independent which we expect to release in Q3 2019.

Please read more about teal on our agile-R website at [go.roche.com/agile-R](http://go.roche.com/agile-R).

*teal* is a shiny-based interactive exploration framework for analyzing clinical
trials data. `teal` currently provides a dynamic filtering facility and diverse
data viewers. `teal` shiny applications are built using standard [shiny
modules](https://shiny.rstudio.com/articles/modules.html).

# Getting Started

1. Install `teal` as described in the [agile-R website](http://go.roche.com/agile-R).
1. Create a new file `app.R`, and paste this code into it:

	```r
	library(teal)
	library(random.cdisc.data)
		
	ASL <- radsl()
	ARS <- radrs(ASL)
	ATE <- radtte(ASL)
	
	attr(ASL, "source") <- "random.cdisc.data::radsl()"
	attr(ARS, "source") <- "random.cdisc.data::radrs(ASL)"
	attr(ATE, "source") <- "random.cdisc.data::radtte(ASL)"
	
	app <- teal::init(
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
	
	shinyApp(app$ui, app$server)
	```
	
1. Execute the above code to run the sample app.
1. You can now modify this sample app to fit your needs. For example, delete
   `ARS` and `ATE`, and change `ASL` to your dataset, for example:

	```r
	ASL <- read_bce("/opt/BIOSTAT/qa/ts00010/libraries/asl.sas7bdat")
	```
   
1. Edit the modules in the `modules = ` argument. For example, you can
   rearrange, delete, or add new modules

    
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
