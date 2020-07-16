context("tests for delayed data loading functionality")

library(random.cdisc.data)
ADSL <- radsl(cached = T) #nolint
attr(ADSL, "keys") <- get_cdisc_keys("ADSL")

ds <- FilteredData$new()
isolate(ds$set_data("ADSL", ADSL))


test_that("resolve_delayed_expr works correctly", {

  # function assumptions check
  # 1) single argument called "data"
  expect_error(resolve_delayed_expr(function() {}, ds = ADSL, is_value_choices = FALSE), #nolint
               regexp = "is_fully_named_list(formals(x)) is not TRUE",
               fixed = T)
  expect_error(resolve_delayed_expr(function(a) {}, ds = ADSL, is_value_choices = FALSE), #nolint
               regexp = 'names(formals(x))[1] == "data" is not TRUE',
               fixed = T)
  expect_error(resolve_delayed_expr(function(data, a) {}, ds = ADSL, is_value_choices = FALSE), #nolint
               regexp = "length(formals(x)) == 1 is not TRUE",
               fixed = T)

  # function assumptions check
  # 2a) returning character unique vector of length <= ncol(ds)
  expect_error(resolve_delayed_expr(function(data) 1, ds = ADSL, is_value_choices = FALSE),
               regexp = "must return a character vector giving unique names")
  expect_error(resolve_delayed_expr(function(data) c("a", "a"), ds = ADSL, is_value_choices = FALSE),
               regexp = "must return a character vector giving unique names")
  expect_error(resolve_delayed_expr(function(data) c("a", "b"), ds = ADSL[1], is_value_choices = FALSE),
               regexp = "must return a character vector giving unique names")

  # function assumptions check
  # 2b) returning unique vector
  expect_error(resolve_delayed_expr(function(data) c(1, 1), ds = ADSL, is_value_choices = TRUE),
               regexp = "must return a vector giving unique values")

  # function return value check
  expect_equal(resolve_delayed_expr(function(data) c("a", "b"), ds = ADSL, is_value_choices = FALSE),
               c("a", "b"))
  expect_equal(resolve_delayed_expr(function(data) 1:2, ds = ADSL, is_value_choices = TRUE),
               1:2)
})


test_that("delayed version of variable_choices", {

  # hard-coded subset
  obj <- variable_choices("ADSL", subset = c("SEX", "ARMCD", "COUNTRY"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = c("SEX", "ARMCD", "COUNTRY")),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = c("SEX", "ARMCD", "COUNTRY"))
  )


  # functional subset
  obj <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
  expect_equal(
    obj,
    structure(
      list(data = "ADSL",
           subset = function(data) colnames(data)[1:2]),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = colnames(ADSL)[1:2])
  )

})

test_that("delayed version of value_choices", {

  # hard-coded subset
  obj <- value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B"), sep = " - "),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B"))
  )


  # functional subset
  obj <- value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
                       subset = function(data) {
                         levels(data$ARMCD)[1:2]
                       })
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", var_choices = "ARMCD", var_label = "ARM",
           subset = function(data) {
             levels(data$ARMCD)[1:2]
           },
           sep = " - "),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM",
                  subset = function(data) {
                    levels(data$ARMCD)[1:2]
                  })
  )


  # functional subset with multiple columns
  combine_armcd_bmrkr2 <- function(data) {
    apply(
      expand.grid(levels(data$ARMCD)[1:2], levels(data$BMRKR2), stringsAsFactors = FALSE),
      1,
      paste,
      collapse = " - "
    )
  }

  obj <- value_choices("ADSL", var_choices = c("ARMCD", "BMRKR2"), var_label = c("ARM", "BMRKR2"),
                       subset = combine_armcd_bmrkr2)
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", var_choices = c("ARMCD", "BMRKR2"), var_label = c("ARM", "BMRKR2"),
           subset = combine_armcd_bmrkr2, sep = " - "),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    value_choices(ADSL, var_choices = c("ARMCD", "BMRKR2"), var_label = c("ARM", "BMRKR2"),
                  subset = combine_armcd_bmrkr2)
  )

})


vc_hard <- variable_choices("ADSL", subset = c("STUDYID", "USUBJID"))
vc_hard_exp <- structure(
  list(data = "ADSL", subset = c("STUDYID", "USUBJID")),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_hard_short <- variable_choices("ADSL", subset = "STUDYID")
vc_hard_short_exp <- structure(
  list(data = "ADSL", subset = "STUDYID"),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
vc_fun_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1:2]),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun_short <- variable_choices("ADSL", subset = function(data) colnames(data)[1])
vc_fun_short_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1]),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

test_that("delayed version of choices_selected", {

  # hard-coded choices and selected
  obj <- choices_selected(vc_hard, selected = vc_hard_short)
  expect_equal(
    obj,
    structure(
      list(choices = vc_hard_exp, selected = vc_hard_short_exp, keep_order = FALSE, fixed = FALSE),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- choices_selected(variable_choices(ADSL, subset = c("STUDYID", "USUBJID")),
                              selected = variable_choices(ADSL, subset = "STUDYID"))
  expect_equal(res_obj, exp_obj)

  # functional choices and selected
  obj <- choices_selected(vc_fun, selected = vc_fun_short)
  expect_equal(
    obj,
    structure(
      list(choices = vc_fun_exp, selected = vc_fun_short_exp, keep_order = FALSE, fixed = FALSE),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(res_obj, exp_obj)

})

test_that("delayed version of select_spec", {

  # hard-coded choices & selected
  obj <- select_spec(vc_hard, selected = vc_hard_short, multiple = FALSE)
  expect_equal(
    obj,
    structure(
      list(choices = vc_hard_exp, selected = vc_hard_short_exp, always_selected = NULL,
           multiple = FALSE, fixed = FALSE, label = "Column"),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- select_spec(variable_choices(ADSL, subset = c("STUDYID", "USUBJID")),
                         selected = variable_choices(ADSL, "STUDYID"))
  expect_equal(res_obj, exp_obj)

  # functional choices & selected
  obj <- select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE)
  expect_equal(
    obj,
    structure(
      list(choices = vc_fun_exp, selected = vc_fun_short, always_selected = NULL,
           multiple = FALSE, fixed = FALSE, label = "Column"),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(res_obj, exp_obj)

})

test_that("delayed version of filter_spec", {

  # hard-coded vars & choices & selected
  obj <- filter_spec(
    vars = variable_choices("ADSL", subset = "ARMCD"),
    choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
    selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
    multiple = FALSE
  )

  expect_equal(
    obj,
    structure(
      list(
        vars = variable_choices("ADSL", subset = "ARMCD"),
        choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
        selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
        multiple = FALSE,
        label = "Filter",
        sep = " - "
      ),
      class = c("delayed_filter_spec", "delayed_data", "filter_spec")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- filter_spec(
    vars = variable_choices(ADSL, subset = "ARMCD"),
    choices = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
    selected = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
    multiple = FALSE
  )

  # comparison not implemented, must be done individually
  expect_equal(res_obj$choices, exp_obj$choices)
  expect_equal(res_obj$selected, exp_obj$selected)
  expect_equal(res_obj[-match(c("choices", "selected"), names(res_obj))],
               exp_obj[-match(c("choices", "selected"), names(exp_obj))])


  # functional choices & selected
  obj <- filter_spec(
    vars = variable_choices("ADSL", subset = function(data) "ARMCD"),
    choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
                            subset = function(data) levels(data$ARMCD)[1:2]),
    selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
                             subset = function(data) "ARM A"),
    multiple = FALSE
  )

  expect_equal(
    obj,
    structure(
      list(
        vars = variable_choices("ADSL", subset = function(data) "ARMCD"),
        choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
                                subset = function(data) levels(data$ARMCD)[1:2]),
        selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
                                 subset = function(data) "ARM A"),
        multiple = FALSE,
        label = "Filter",
        sep = " - "
      ),
      class = c("delayed_filter_spec", "delayed_data", "filter_spec")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))

  # comparison not implemented, must be done individually
  expect_equal(res_obj$choices, exp_obj$choices)
  expect_equal(res_obj$selected, exp_obj$selected)
  expect_equal(res_obj[-match(c("choices", "selected"), names(res_obj))],
               exp_obj[-match(c("choices", "selected"), names(exp_obj))])
})

test_that("delayed version of data_extract_spec", {

  # hard-coded subset
  obj <- data_extract_spec(
    "ADSL",
    select = select_spec(vc_hard, selected = vc_hard_short, multiple = FALSE),
    filter = filter_spec(
      vars = variable_choices("ADSL", subset = "ARMCD"),
      choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(ADSL, c("STUDYID", "USUBJID")), selected = variable_choices(ADSL, "STUDYID")),
    filter = filter_spec(
      vars = variable_choices(ADSL, subset = "ARMCD"),
      choices = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  expect_equal(res_obj$select, exp_obj$select)
  expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)


  # functional subset
  obj <- data_extract_spec(
    "ADSL",
    select = select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE),
    filter = filter_spec(
      vars = variable_choices("ADSL", subset = "ARMCD"),
      choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
                              subset = function(data) c("ARM A", "ARM B")),
      selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM",
                               subset = function(data) "ARM A"),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(ADSL, c("STUDYID", "USUBJID")), selected = variable_choices(ADSL, "STUDYID")),
    filter = filter_spec(
      vars = variable_choices(ADSL, subset = "ARMCD"),
      choices = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  expect_equal(res_obj$select, exp_obj$select)
  expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)

})
