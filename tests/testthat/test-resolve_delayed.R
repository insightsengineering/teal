library(scda)
scda_data <- synthetic_cdisc_data("latest")
adsl <- scda_data$adsl # nolint
adtte <- scda_data$adtte # nolint
data <- cdisc_data(
  cdisc_dataset("ADSL", adsl),
  cdisc_dataset("ADTTE", adtte)
)

ds <- teal:::CDISCFilteredData$new()
isolate(filtered_data_set(data, ds))

testthat::test_that("resolve_delayed_expr works correctly", {
  # function assumptions check
  # 1) single argument called "data"
  testthat::expect_error(
    resolve_delayed_expr(function() {}, ds = adsl, is_value_choices = FALSE), # nolint
    regexp = "is_fully_named_list(formals(x)) is not TRUE",
    fixed = TRUE
  )
  testthat::expect_error(
    resolve_delayed_expr(function(a) {}, ds = adsl, is_value_choices = FALSE), # nolint
    regexp = 'names(formals(x))[1] == "data" is not TRUE',
    fixed = TRUE
  )
  testthat::expect_error(
    resolve_delayed_expr(function(data, a) {}, ds = adsl, is_value_choices = FALSE), # nolint
    regexp = "length(formals(x)) == 1 is not TRUE",
    fixed = TRUE
  )

  # function assumptions check
  # 2a) returning character unique vector of length <= ncol(ds)
  testthat::expect_error(
    resolve_delayed_expr(function(data) 1, ds = adsl, is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset"
  )
  testthat::expect_error(
    resolve_delayed_expr(function(data) c("a", "a"), ds = adsl, is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset"
  )
  testthat::expect_error(
    resolve_delayed_expr(function(data) c("a", "b"), ds = adsl[1], is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset"
  )

  # function assumptions check
  # 2b) returning unique vector
  testthat::expect_error(
    resolve_delayed_expr(function(data) c(1, 1), ds = adsl, is_value_choices = TRUE),
    regexp = "must return a vector with unique values from the respective columns of the dataset"
  )

  # function return value check
  testthat::expect_equal(
    resolve_delayed_expr(function(data) c("a", "b"), ds = adsl, is_value_choices = FALSE),
    c("a", "b")
  )
  testthat::expect_equal(resolve_delayed_expr(function(data) 1:2, ds = adsl, is_value_choices = TRUE), 1:2)
})

testthat::test_that("resolve_delayed.list works correctly", {
  arm_ref_comp <- list(
    ARMCD = list(
      ref = value_choices(adtte, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices(adtte, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
    ),
    ARM = list(
      ref = variable_choices(adsl, subset = "ARM"), comp = variable_choices(adsl, subset = "ARMCD")
    ),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )
  arm_ref_comp_ddl <- list(
    ARMCD = list(
      ref = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
    ),
    ARM = list(
      ref = variable_choices("ADSL", subset = "ARM"), comp = variable_choices("ADSL", subset = "ARMCD")
    ),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )

  ddl_resolved <- isolate(resolve_delayed(arm_ref_comp_ddl, ds))
  testthat::expect_identical(arm_ref_comp, ddl_resolved)
})
