simple_teal_data <- function() {
  data <- within(teal_data(), {
    iris <- iris
    mtcars <- mtcars
  })
  data
}
