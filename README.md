
# Teal R Package for Creating Interactive Shiny Apps

*teal* is a shiny based interactive exploration framework for analyzing clinical
trials data. `teal` currently provides a dynamic filtering facility and 
different data viewers. `teal` shiny applications are build using standard
[shiny modules](https://shiny.rstudio.com/articles/modules.html).

# Installation 

While on the Roche network, open R or RStudio and execute the following:

```r
# install.packages("devtools")
library(devtools)
install_github('waddella/teal', host='https://github.roche.com/api/v3', build_vignettes = TRUE)
```

# Getting Started

Load the teal package and take a look at the vignette:

```r
library(teal)
vignette("01_getting_started", package="teal")
```

