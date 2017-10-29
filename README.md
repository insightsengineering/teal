# Teal: Interactive exploration and analysis of clinical trial data

*Active development happens on the `beta` branch, so contribute to that branch.
We use the `master` branch only for R package releases (i.e. versions).*

*teal* is a shiny-based interactive exploration framework for analyzing clinical
trials data. `teal` currently provides a dynamic filtering facility and 
diverse data viewers. `teal` shiny applications are built using standard
[shiny modules](https://shiny.rstudio.com/articles/modules.html).

# Getting Started

1. Install `teal` as in [Installation below](#installation).
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
	
1. Execute the above code to run a sample app.
1. You can now modify this sample app to fit your need. For example, delete
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

While on the Roche network, [open R or RStudio](https://r.roche.com) and execute
the following:

```r
# install.packages("devtools")
library(devtools)
install_github('Rpackages/teal', host='https://github.roche.com/api/v3', build_vignettes = TRUE)
```

# Documentation

Currently there are a number of resources with documentation:

1. The `teal` package vignettes run

    ```
    library(teal)
    vignettes(package = "teal")
    ```

2. The `teal` R manual pages

    ```
    help(package = "teal")
    ```
    
3. Training slides and screencasts:

    + [Using teal](https://docs.google.com/presentation/d/1RTzALidxFQrUV4oH0OoIE_4EZRQPOf8Qw-EX06r-fp8/edit) ([screencast](https://streamingmedia.roche.com/media/Teal+Introduction+Workshop+with+Shanghai+via+Webex/1_k51jv1jo))
    + [Creating teal modules](https://docs.google.com/presentation/d/1_V0w4x9Ve5rw0nZydkpyfe5NZnA00MlzSgSPr3tANkk/edit#slide=id.g23552d0ebb_0_0) ([screencast](https://streamingmedia.roche.com/media/advanced_teal_workshop_GSC_2017/1_tltjoz5m))
