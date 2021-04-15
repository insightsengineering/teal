test_that("Will output warnings when value_choices applied on datasets with missing values and / or labels", {

  data <- data.frame(
    A = c(1, 2, 3),
    B = c(NA, "a", "b"),
    C = rep(NA, 3),
    D = factor(c("x", "y", NA), levels = c("x", "y", "NA")),
    F = c(NA, "NA", "M"),
    G = c(1, 2, NA),
    H = c(TRUE, NA, FALSE),
    I = rep(TRUE, 3),
    J = c("NA", "a", "b")
  )
  expect_warning(value_choices(data, var_choices = c("F")))
  expect_warning(value_choices(data, var_choices = c("D")))
  expect_warning(value_choices(data, var_choices = c("F", "D")))
  expect_warning(value_choices(data, var_choices = c("A", "D")))
  expect_warning(value_choices(data, var_choices = c("A", "F")))
  expect_error(value_choices(data, var_choices = "K"))
  expect_error(value_choices(data, var_choices = "F", var_label = "K"))
  expect_warning(value_choices(data, var_choices = c("J")), NA)
  expect_warning(value_choices(data, var_choices = c("B")), NA)
})
