adte_filters <- filter_spec(
  vars = c("PARAMCD"), # only key variables are allowed
  sep = " - ",
  choices = c("OS", "PFS", "EFS"),
  selected = "OS",
  multiple = TRUE, # if multiple, then a spread is needed
  label = "Choose endpoint")

#nopreproc >
elo <- letters[1:10]
elo2 <- seq(1, 10)
# <nopreproc

source("app_source3.R")
