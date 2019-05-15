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
	library(teal.modules.general)
	library(random.cdisc.data)
	
	ASL <- radsl(seed = 1)
	ARS <- radrs(ASL, seed = 100)
	ATE <- radtte(ASL, seed = 1000)
	
	app <- init(
	  data = list(ASL = ASL, ARS = ARS, ATE = ATE),
	  modules = root_modules(
	    module(
	      "data source",
	      server = function(input, output, session, datasets) {},
	      ui = function(id) div(p("information about data source")),
	      filters = NULL
	    ),
	    module(
	      "ASL AGE histogram",
	      server = function(input, output, session, datasets) {
	        output$hist <- renderPlot(hist(datasets$get_data("ASL")$AGE))
	      },
	      ui = function(id) {ns <- NS(id); plotOutput(ns('hist'))},
	      filters = NULL
	    ),
	    modules(
	      label = "Example datasets (tree module)",
	      module(
	        "iris",
	        server = function(input, output, session, datasets) {output$iris <- renderPlot(plot(iris))},
	        ui = function(id) {ns <- NS(id); plotOutput(ns('iris'))},
	        filters = NULL
	      ),
	      module(
	        "cars",
	        server = function(input, output, session, datasets) {output$cars <- renderPlot(plot(cars))},
	        ui = function(id) {ns <- NS(id); plotOutput(ns('cars'))},
	        filters = NULL
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
  'NEST/teal', 
  ref = "devel",
  host = 'https://github.roche.com/api/v3',
  upgrade_dependencies = FALSE,
  build_vignettes = TRUE
)
```

# More Teal Modules

Where there are quite a few teal modules in the wild, we are currently working
on high-quality reusable teal modules which are available in the
[teal.modules.clinical](https://pages.github.roche.com/NEST/teal.modules.clinical)
package.

# Documentation

Currently there are a number of resources with documentation:

1. The [agile-R website](http://go.roche.com/agile-R)

1. The [project website](https://pages.github.roche.com/NEST/teal/)

1. The `teal` package vignettes run

    ```
    library(teal)
    vignettes(package = "teal")
    ```

1. The `teal` R manual pages

    ```
    help(package = "teal")
    ```
