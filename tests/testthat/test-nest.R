library(test.nest)

test_lintr()
if (Sys.getenv("R_COVR") != "true") {
  test_strict()
}
test_regexp()
test_spell()
test_usage()
test_importfrom()
