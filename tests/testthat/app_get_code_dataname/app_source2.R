# code ADTTE>
adtte_filters <- filter_spec(
  vars = c("PARAMCD"), # only key variables are allowed
  sep = " - ",
  choices = c("OS", "PFS", "EFS"),
  selected = "OS",
  multiple = TRUE, # if multiple, then a spread is needed
  label = "Choose endpoint"
)

# nocode >
nocode <- letters[1:10]
nocode2 <- seq(1, 10)
# <nocode

a <- "a" # nocode
# < ADTTE code

source("app_source3.R")
