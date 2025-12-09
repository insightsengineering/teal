pkg_name <- "teal"
library(pkg_name, character.only = TRUE)
testthat::test_check(pkg_name)
unlink(".renv", recursive = TRUE, force = TRUE)
unlink("BiocManager", recursive = TRUE, force = TRUE)
