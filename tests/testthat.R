pkg_name <- "teal"
library(pkg_name, character.only = TRUE)
testthat::test_check(pkg_name)
unlink(".renv")
unlink("BiocManager")
