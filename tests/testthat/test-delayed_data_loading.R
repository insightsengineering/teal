context("tests for delayed data loading functionality")

library(random.cdisc.data)
ADSL <- radsl(cached = TRUE) # nolint
attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
ADTTE <- radtte(cached = TRUE) # nolint

ds <- teal:::FilteredData$new()
isolate(ds$set_data("ADSL", ADSL))
isolate(ds$set_data("ADTTE", ADTTE))


test_that("resolve_delayed_expr works correctly", {

  # function assumptions check
  # 1) single argument called "data"
  expect_error(
    resolve_delayed_expr(function() {}, ds = ADSL, is_value_choices = FALSE), #nolint
    regexp = "is_fully_named_list(formals(x)) is not TRUE",
    fixed = T)
  expect_error(
    resolve_delayed_expr(function(a) {}, ds = ADSL, is_value_choices = FALSE), #nolint
    regexp = 'names(formals(x))[1] == "data" is not TRUE',
    fixed = T)
  expect_error(
    resolve_delayed_expr(function(data, a) {}, ds = ADSL, is_value_choices = FALSE), #nolint
    regexp = "length(formals(x)) == 1 is not TRUE",
    fixed = T)

  # function assumptions check
  # 2a) returning character unique vector of length <= ncol(ds)
  expect_error(
    resolve_delayed_expr(function(data) 1, ds = ADSL, is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset")
  expect_error(
    resolve_delayed_expr(function(data) c("a", "a"), ds = ADSL, is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset")
  expect_error(
    resolve_delayed_expr(function(data) c("a", "b"), ds = ADSL[1], is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset")

  # function assumptions check
  # 2b) returning unique vector
  expect_error(
    resolve_delayed_expr(function(data) c(1, 1), ds = ADSL, is_value_choices = TRUE),
    regexp = "must return a vector with unique values from the respective columns of the dataset")

  # function return value check
  expect_equal(
    resolve_delayed_expr(function(data) c("a", "b"), ds = ADSL, is_value_choices = FALSE),
    c("a", "b"))
  expect_equal(resolve_delayed_expr(function(data) 1:2, ds = ADSL, is_value_choices = TRUE), 1:2)
})

test_that("resolve_delayed.list works correctly", {
  arm_ref_comp <- list(
    ARMCD = list(
      ref = value_choices(ADTTE, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices(ADTTE, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))),
    ARM = list(
      ref = variable_choices(ADSL, subset = "ARM"), comp = variable_choices(ADSL, subset = "ARMCD")),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )
  arm_ref_comp_ddl <- list(
    ARMCD = list(
      ref = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))),
    ARM = list(
      ref = variable_choices("ADSL", subset = "ARM"), comp = variable_choices("ADSL", subset = "ARMCD")),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )

  ddl_resolved <- isolate(resolve_delayed(arm_ref_comp_ddl, ds))
  expect_identical(arm_ref_comp, ddl_resolved)
})

test_that("delayed version of variable_choices", {

  # hard-coded subset
  obj <- variable_choices("ADSL", subset = c("SEX", "ARMCD", "COUNTRY"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = c("SEX", "ARMCD", "COUNTRY"), key = NULL),
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
      list(data = "ADSL", subset = function(data) colnames(data)[1:2], key = NULL),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = colnames(ADSL)[1:2], key = get_cdisc_keys("ADSL")$primary)
  )

  # non-null key value
  obj <- variable_choices("ADSL", key = c("USUBJID", "STUDYID"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = NULL, key = c("USUBJID", "STUDYID")),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, key = c("USUBJID", "STUDYID"))
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
  obj <- value_choices(
    "ADSL",
    var_choices = "ARMCD",
    var_label = "ARM",
    subset = function(data) {
      levels(data$ARMCD)[1:2]
    })
  expect_equal(
    obj,
    structure(
      list(
        data = "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
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

  obj <- value_choices(
    "ADSL",
    var_choices = c("ARMCD", "BMRKR2"),
    var_label = c("ARM", "BMRKR2"),
    subset = combine_armcd_bmrkr2)
  expect_equal(
    obj,
    structure(
      list(
        data = "ADSL",
        var_choices = c("ARMCD", "BMRKR2"),
        var_label = c("ARM", "BMRKR2"),
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
  list(data = "ADSL", subset = c("STUDYID", "USUBJID"), key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_hard_short <- variable_choices("ADSL", subset = "STUDYID")
vc_hard_short_exp <- structure(
  list(data = "ADSL", subset = "STUDYID", key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
vc_fun_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1:2], key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun_short <- variable_choices("ADSL", subset = function(data) colnames(data)[1])
vc_fun_short_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1], key = NULL),
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
  exp_obj <- choices_selected(
    variable_choices(ADSL, subset = c("STUDYID", "USUBJID"), key = get_cdisc_keys("ADSL")$primary),
    selected = variable_choices(ADSL, subset = c("STUDYID"), key = get_cdisc_keys("ADSL")$primary))
  expect_equal(res_obj, exp_obj, check.attributes = TRUE)

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
      list(
        choices = vc_hard_exp,
        selected = vc_hard_short_exp,
        always_selected = NULL,
        multiple = FALSE, fixed = FALSE, label = NULL),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- select_spec(
    variable_choices(ADSL, subset = c("STUDYID", "USUBJID"), key = get_cdisc_keys("ADSL")$primary),
    selected = variable_choices(ADSL, "STUDYID", key = get_cdisc_keys("ADSL")$primary))
  expect_equal(res_obj, exp_obj)

  # functional choices & selected
  obj <- select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE)
  expect_equal(
    obj,
    structure(
      list(
        choices = vc_fun_exp,
        selected = vc_fun_short,
        always_selected = NULL,
        multiple = FALSE, fixed = FALSE, label = NULL),
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
        label = NULL,
        sep = " - ",
        drop_keys = TRUE
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
  expect_equal(
    res_obj[-match(c("choices", "selected"), names(res_obj))],
    exp_obj[-match(c("choices", "selected"), names(exp_obj))])


  # functional choices & selected
  obj <- filter_spec(
    vars = variable_choices("ADSL", subset = function(data) "ARMCD"),
    choices = value_choices(
      "ADSL",
      var_choices = "ARMCD",
      var_label = "ARM",
      subset = function(data) levels(data$ARMCD)[1:2]),
    selected = value_choices(
      "ADSL",
      var_choices = "ARMCD",
      var_label = "ARM",
      subset = function(data) "ARM A"),
    multiple = FALSE
  )

  expect_equal(
    obj,
    structure(
      list(
        vars = variable_choices("ADSL", subset = function(data) "ARMCD"),
        choices = value_choices(
          "ADSL",
          var_choices = "ARMCD",
          var_label = "ARM",
          subset = function(data) levels(data$ARMCD)[1:2]),
        selected = value_choices(
          "ADSL",
          var_choices = "ARMCD",
          var_label = "ARM",
          subset = function(data) "ARM A"),
        multiple = FALSE,
        label = NULL,
        sep = " - ",
        drop_keys = TRUE
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
    select = select_spec(variable_choices(ADSL, c("STUDYID", "USUBJID"), key = get_cdisc_keys("ADSL")$primary),
      selected = variable_choices(ADSL, "STUDYID", key = get_cdisc_keys("ADSL")$primary)),
    filter = filter_spec(
      vars = variable_choices(ADSL, subset = "ARMCD", key = get_cdisc_keys("ADSL")$primary),
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
      choices = value_choices(
        "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) c("ARM A", "ARM B")),
      selected = value_choices(
        "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) "ARM A"),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(ADSL, c("STUDYID", "USUBJID"), key = get_cdisc_keys("ADSL")$primary),
      selected = variable_choices(ADSL, "STUDYID", key = get_cdisc_keys("ADSL")$primary)),
    filter = filter_spec(
      vars = variable_choices(ADSL, subset = "ARMCD", key = get_cdisc_keys("ADSL")$primary),
      choices = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  expect_equal(res_obj$select, exp_obj$select)
  expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)

})

# Delayed data extract - single data connector with two rcd dataset connectors ----

get_continuous <- function(data) {
  # example function to show selections from delayed data
  idx <- vapply(data, function(x) is.numeric(x) && length(unique(x)) > 6, logical(1))
  colnames(data)[idx]
}

test_that("Delayed data extract - single data connector with two rcd dataset connectors", {

  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, cached = TRUE, ADSL = adsl)
  data <- cdisc_data(rcd_data(adsl, adae))

  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    )
  )
  y <- data_extract_spec(
    dataname = "ADAE",
    select = select_spec(
      choices = variable_choices("ADAE", subset = get_continuous)
    )
  )

  for (connector in data$get_connectors()) {
    connector$pull()
  }
  # test delayed data extract
  ds <- FilteredData$new()
  isolate(set_datasets_data(ds, data))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADAE <- get_raw_data(data, "ADAE") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous, key = get_cdisc_keys("ADSL")$primary)
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADAE",
    select = select_spec(
      choices = variable_choices(ADAE, subset = get_continuous, key = get_cdisc_keys("ADAE")$primary)
    )
  )
  x_result <- isolate(resolve_delayed(x, datasets = ds))
  y_result <- isolate(resolve_delayed(y, datasets = ds))
  expect_identical(x_result, x_expected)
  expect_identical(y_result, y_expected)
})

# Delayed choices selected - single data connector with two rcd dataset connectors ----

test_that("Delayed choices selected - single data connector with two rcd dataset connectors", {

  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, cached = TRUE, ADSL = adsl)
  data <- cdisc_data(rcd_data(adsl, adae))

  choices <- variable_choices("ADSL")
  for (connector in data$get_connectors()) {
    connector$pull()
  }
  ds <- FilteredData$new()
  isolate(set_datasets_data(ds, data))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  choices_expected <- variable_choices(ADSL, key = get_cdisc_keys("ADSL")$primary)
  choices_result <- isolate(resolve_delayed(choices, datasets = ds))
  expect_identical(choices_result, choices_expected)
})

# Delayed data extract - filtered ----

test_that("Delayed data extract - filtered", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adsl$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "ADSL seed", min = 0, value = 1),
      optionalSliderInput(inputId = ns("study_duration"),
                          label = "Duration of study in years",
                          min = 0,
                          max = 5,
                          value = 2,
                          step = 1)
    )
  }
  )
  adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE, ADSL = adsl)
  data <- cdisc_data(rcd_data(adsl, adrs))

  # object is a duplicate of Mixed Data and Datasets -no class tests required

  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices("ADSL",
                              var_choices = "ARMCD",
                              var_label = "ARM",
                              subset = function(data) levels(data$ARMCD)[1:2]
      ),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices("ADRS", subset = get_continuous)
    )
  )

  for (connector in data$get_connectors()) {
    connector$pull()
  }
  ds <- FilteredData$new()
  isolate(set_datasets_data(ds, data))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADRS <- get_raw_data(data, "ADRS") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(ADSL,
                              var_choices = "ARMCD",
                              var_label = "ARM",
                              subset = function(data) levels(data$ARMCD)[1:2]
      ),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(ADRS, subset = get_continuous)
    )
  )
  x_result <- isolate(resolve_delayed(x, datasets = ds))
  y_result <- isolate(resolve_delayed(y, datasets = ds))
  expect_identical(x_result, x_expected)
  expect_identical(y_result, y_expected)

  # object is a duplicate of Mixed Data and Datasets -no code tests required
})

# Delayed extract filter concatenated - single data connector with two rcd dataset connectors ----

test_that("Delayed extract filter concatenated - single data connector with two rcd dataset connectors", {

  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE, ADSL = adsl)
  data <- teal_data(rcd_data(adsl, adrs))

  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices("ADSL",
                              var_choices = "ARMCD",
                              var_label = "ARM",
                              subset = function(data) levels(data$ARMCD)[1:2]
      ),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices("ADRS", subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = c("PARAMCD", "AVISIT"),
      choices = value_choices(
        data = "ADRS",
        var_choices = c("PARAMCD", "AVISIT"),
        var_label = c("PARAMCD", "AVISIT"),
        subset = function(data) {
          paste(
            levels(data$PARAMCD),
            levels(data$AVISIT)[4:6],
            sep = " - "
          )
        }
      ),
      selected = "INVET - END OF INDUCTION",
      multiple = TRUE
    )
  )

  for (connector in data$get_connectors()) {
    connector$pull()
  }
  ds <- FilteredData$new()
  isolate(set_datasets_data(ds, data))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADRS <- get_raw_data(data, "ADRS") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(ADSL,
                              var_choices = "ARMCD",
                              var_label = "ARM",
                              subset = function(data) levels(data$ARMCD)[1:2]
      ),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(ADRS, subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = c("PARAMCD", "AVISIT"),
      choices = value_choices(
        data = ADRS,
        var_choices = c("PARAMCD", "AVISIT"),
        var_label = c("PARAMCD", "AVISIT"),
        subset = function(data) {
          paste(
            levels(data$PARAMCD),
            levels(data$AVISIT)[4:6],
            sep = " - "
          )
        }
      ),
      selected = "INVET - END OF INDUCTION",
      multiple = TRUE
    )
  )
  x_result <- isolate(resolve_delayed(x, datasets = ds))
  y_result <- isolate(resolve_delayed(y, datasets = ds))
  expect_identical(x_result, x_expected)
  expect_identical(y_result, y_expected)
})

# Delayed extract two filters - single data connector with two rcd dataset connectors ----

test_that("Delayed extract two filters - single data connector with two rcd dataset connectors", {

  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE, ADSL = adsl)
  data <- teal_data(rcd_data(adsl, adrs))

  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices("ADSL",
                              var_choices = "ARMCD",
                              var_label = "ARM",
                              subset = function(data) levels(data$ARMCD)[1:2]
      ),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices("ADRS", subset = get_continuous)
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = "ADRS",
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = "ADRS",
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )

  for (connector in data$get_connectors()) {
    connector$pull()
  }
  ds <- FilteredData$new()
  isolate(set_datasets_data(ds, data))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADRS <- get_raw_data(data, "ADRS") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(ADSL,
                              var_choices = "ARMCD",
                              var_label = "ARM",
                              subset = function(data) levels(data$ARMCD)[1:2]
      ),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(ADRS, subset = get_continuous)
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = ADRS,
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = ADRS,
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )
  x_result <- isolate(resolve_delayed(x, datasets = ds))
  y_result <- isolate(resolve_delayed(y, datasets = ds))
  expect_identical(x_result, x_expected)
  expect_identical(y_result, y_expected)
})

# Delayed extract - dataset & connector ----

test_that("Delayed extract - RelationalData with single dataset and multiple connectors", {

  adsl <- relational_dataset(radsl(cached = TRUE),
                             dataname = "ADSL",
                             keys = get_cdisc_keys("ADSL"),
                             code = "ADSL <- radsl(cached = TRUE)",
                             label = "ADSL"
  )
  adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE, ADSL = adsl)
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE, ADSL = adsl)
  data <- cdisc_data(adsl, rcd_data(adrs, adtte))

  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(get_raw_data(adsl), var_choices = "ARMCD", var_label = "ARM"),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices("ADRS", subset = get_continuous)
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = "ADRS",
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = "ADRS",
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )

  for (connector in data$get_connectors()) {
    connector$pull()
  }
  ds <- FilteredData$new()
  isolate(set_datasets_data(ds, data))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADRS <- get_raw_data(data, "ADRS") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous, key = get_cdisc_keys("ADSL")$primary)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(get_raw_data(adsl), var_choices = "ARMCD", var_label = "ARM"),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(ADRS, subset = get_continuous, key = get_cdisc_keys("ADRS")$primary)
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = ADRS,
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = ADRS,
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )
  x_result <- isolate(resolve_delayed(x, datasets = ds))
  y_result <- isolate(resolve_delayed(y, datasets = ds))
  expect_identical(x_result, x_expected)
  expect_identical(y_result, y_expected)
})
