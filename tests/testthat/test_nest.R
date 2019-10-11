library(test.nest)

if (Sys.getenv("R_COVR") != "true") {
  test_strict()
}
test_lintr()
test_regexp()
