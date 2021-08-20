# Contains integration tests between delayed data loading objects and
# the objects responsible for loading, pulling and filtering the data

library(random.cdisc.data)
ADSL <- radsl(cached = TRUE) # nolint
ADTTE <- radtte(cached = TRUE) # nolint
data <- cdisc_data(
  cdisc_dataset("ADSL", ADSL),
  cdisc_dataset("ADTTE", ADTTE)
)

ds <- teal:::CDISCFilteredData$new()
isolate(filtered_data_set(data, ds))

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

# Delayed data extract - single data connector with two rcd dataset connectors ----
get_continuous <- function(data) {
  # example function to show selections from delayed data
  idx <- vapply(data, function(x) is.numeric(x) && length(unique(x)) > 6, logical(1))
  colnames(data)[idx]
}

test_that("Delayed data extract - single data connector with two rcd dataset connectors", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, cached = TRUE)
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
      choices = variable_choices("ADAE", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
    )
  )

  for (connector in data$get_connectors()) {
    connector$pull()
  }
  # test delayed data extract
  ds <- teal:::CDISCFilteredData$new()
  isolate(filtered_data_set(data, ds))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADAE <- get_raw_data(data, "ADAE") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous, key = get_cdisc_keys("ADSL")),
      selected = NULL
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADAE",
    select = select_spec(
      choices = variable_choices(ADAE, subset = get_continuous, key = get_cdisc_keys("ADAE"))
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
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, cached = TRUE)
  data <- cdisc_data(rcd_data(adsl, adae))

  choices <- variable_choices("ADSL")
  for (connector in data$get_connectors()) {
    connector$pull()
  }
  ds <- teal:::CDISCFilteredData$new()
  isolate(filtered_data_set(data, ds))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  choices_expected <- variable_choices(ADSL, key = get_cdisc_keys("ADSL"))
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
  adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
  data <- cdisc_data(rcd_data(adsl, adrs))

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
      choices = variable_choices("ADRS", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
    )
  )

  for (connector in data$get_connectors()) {
    connector$pull()
  }
  ds <- teal:::CDISCFilteredData$new()
  isolate(filtered_data_set(data, ds))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADRS <- get_raw_data(data, "ADRS") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous),
      selected = NULL
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
})

# Delayed extract filter concatenated - single data connector with two rcd dataset connectors ----
test_that("Delayed extract filter concatenated - single data connector with two rcd dataset connectors", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
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
      choices = variable_choices("ADRS", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
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
  ds <- teal:::CDISCFilteredData$new()
  isolate(filtered_data_set(data, ds))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADRS <- get_raw_data(data, "ADRS") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous),
      selected = NULL
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
  adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE)
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
      choices = variable_choices("ADRS", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
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
  ds <- teal:::CDISCFilteredData$new()
  isolate(filtered_data_set(data, ds))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADRS <- get_raw_data(data, "ADRS") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous),
      selected = NULL
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

  adsl <- dataset(
    radsl(cached = TRUE),
    dataname = "ADSL",
    keys = get_cdisc_keys("ADSL"),
    code = "ADSL <- radsl(cached = TRUE)",
    label = "ADSL")
  adrs <- rcd_cdisc_dataset_connector("ADRS", radrs, cached = TRUE, ADSL = adsl, keys = get_cdisc_keys("ADRS"))
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE, ADSL = adsl, keys = get_cdisc_keys("ADTTE"))
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
      choices = variable_choices("ADRS", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
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
  ds <- teal:::CDISCFilteredData$new()
  isolate(filtered_data_set(data, ds))

  ADSL <- get_raw_data(data, "ADSL") # nolint
  ADRS <- get_raw_data(data, "ADRS") # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous, key = get_cdisc_keys("ADSL")),
      NULL
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
      choices = variable_choices(ADRS, subset = get_continuous, key = get_cdisc_keys("ADRS"))
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
