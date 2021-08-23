test_that("check_metadata does not produce error if join_keys are consistent for given datasets", {

  df_1 <- data.frame(x = 1:10, y = 1:10)
  df_2 <- data.frame(u = 1:10, v = 1:10)

  constructor_wrapper <- function(join_keys) {
    data <- teal_data(
      dataset("df_1", df_1),
      dataset("df_2", df_2),
      join_keys = join_keys
    )
  }

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys()
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "u")))
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "u", "y" = "v")))
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_2", "df_2", c("u" = "u")))
    )$check_metadata()
  )

})

test_that("check_metadata fails if inconsistent join_keys for given datasets", {

  df_1 <- data.frame(x = 1:10, y = 1:10)
  df_2 <- data.frame(u = 1:10, v = 1:10)

  constructor_wrapper <- function(join_keys) {
    data <- teal_data(
      dataset("df_1", df_1),
      dataset("df_2", df_2),
      join_keys = join_keys
    )
  }

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "w")))
    )$check_metadata()
  )

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "y", "v" = "v")))
    )$check_metadata()
  )

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "x")))
    )$check_metadata()
  )

})

test_that("deep clone", {
  ## DatasetConnector
  expect_silent({
    x_c <- CallableFunction$new(data.frame)
    x_c$set_args(list(c1 = seq_len(10)))
    x <- dataset_connector("x", x_c)
  })

  expect_silent({
    x_copy <- x$clone(deep = TRUE)
  })

  expect_silent({
    load_dataset(x)
  })

  expect_true(is_pulled(x))
  expect_false(is_pulled(x_copy))

  # check one of the private fields of R6 class
  expect_false(rlang::is_reference(x$get_pull_callable(), x_copy$get_pull_callable()))

  ## RelationalData
  expect_silent({
    x1 <- dataset("x1", data.frame(col1 = seq_len(10)))
    x2 <- dataset("x2", data.frame(col2 = seq_len(10)))
    x <- RelationalData$new(x1, x2)
  })

  expect_silent({
    x_copy <- x$clone(deep = TRUE)
  })

  # check one of the private fields of list of R6 class object
  expect_false(rlang::is_reference(x$get_items()[[1]], x_copy$get_items()[[1]]))
  # check one of the private fields of R6 class
  expect_false(rlang::is_reference(x$get_join_keys(), x_copy$get_join_keys()))

  ## RelationalDataConnector
  expect_silent({
    x1_c <- CallableFunction$new(data.frame)
    x1_c$set_args(list(n = seq_len(10)))
    x1 <- dataset_connector("x1", x1_c)

    x2_c <- CallableFunction$new(data.frame)
    x2_c$set_args(list(n = seq_len(20)))
    x2 <- dataset_connector("x2", x2_c)

    x <- teal:::RelationalData$new(x1, x2)
  })

  expect_silent({
    x_copy <- x$clone(deep = TRUE)
  })

  expect_silent({
    lapply(x$get_items(), load_dataset)
  })

  expect_true(is_pulled(x))
  expect_false(is_pulled(x_copy))

  # check one of the private fields of list of R6 class object
  expect_false(rlang::is_reference(x$get_items()[[1]], x_copy$get_items()[[1]]))
  # check one of the private fields of R6 class
  expect_false(rlang::is_reference(x$get_join_keys(), x_copy$get_join_keys()))
})

testthat::test_that("The hashes of Datasets objects are correct after mutating the RelationalData object", {
  mutated_iris <- iris
  mutated_iris$test <- 1
  mutated_iris_hash <- digest::digest(mutated_iris, algo = "md5")
  rd <- teal_data(dataset("iris", iris))
  mutate_data(rd, code = "iris$test <- 1")
  rd$execute_mutate()
  testthat::expect_equal(rd$get_dataset("iris")$get_hash(), mutated_iris_hash)
})

# Multiple rcd_data connectors ----
testthat::test_that("Multiple rcd_data connectors wrapped in cdisc_data", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, N = 1)
  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  advs <- rcd_cdisc_dataset_connector("ADVS", radvs, ADSL = adsl)
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl)
  adsl_adae <- rcd_data(adsl, adae)
  advs_adtte <- rcd_data(advs, adtte)
  data <- cdisc_data(adsl_adae, advs_adtte)

  items <- data$get_items()
  testthat::expect_true(inherits(data, "RelationalData"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))
  testthat::expect_true(all(vapply(data$get_connectors(), inherits, logical(1), "RelationalDataConnector")))

  testthat::expect_equal(unname(get_dataname(data)), c("ADSL", "ADAE", "ADVS", "ADTTE"))

  testthat::expect_equal(items$ADSL$get_code(), "radsl(N = 1)")
  testthat::expect_equal(items$ADAE$get_code(), "radae(ADSL = ADSL)")
  testthat::expect_equal(items$ADVS$get_code(), "radvs(ADSL = ADSL)")
  testthat::expect_equal(items$ADTTE$get_code(), "radtte(ADSL = ADSL)")

  testthat::expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)"
  )
  testthat::expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADAE <- radae(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(data, "ADVS"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADVS <- radvs(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(data, "ADTTE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADTTE <- radtte(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(data),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADAE <- radae(ADSL = ADSL)\nADVS <- radvs(ADSL = ADSL)\nADTTE <- radtte(ADSL = ADSL)" # nolint
  )
})

# RelationalData with single dataset and connector ----
testthat::test_that("RelationalData with single dataset and connector", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adsl_data <- rcd_data(adsl)

  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    x = radtte(cached = TRUE),
    code = "ADTTE <- radtte(cached = TRUE)"
  )

  adae <- rcd_cdisc_dataset_connector("ADAE", radae, cached = TRUE)
  adae$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "ADSL seed", min = 0, value = 2),
      optionalSliderInput(
        inputId = ns("max_n_aes"),
        label = "Maximum number of AEs per patient",
        min = 0,
        max = 10,
        value = 10,
        step = 1)
    )
  }
  )

  data <- cdisc_data(adsl_data, adtte, adae)
  items <- data$get_items()
  testthat::expect_length(items, 3)
  testthat::expect_true(inherits(data, "RelationalData"))
  testthat::expect_true(inherits(items$ADSL, "DatasetConnector"))
  testthat::expect_true(inherits(items$ADTTE, "Dataset"))
  testthat::expect_true(inherits(items$ADAE, "DatasetConnector"))

  connectors <- data$get_connectors()
  testthat::expect_length(connectors, 2)
  testthat::expect_true(
    inherits(connectors[[1]], "RelationalDataConnector") &&
      inherits(connectors[[2]], "DatasetConnector")
  )

  testthat::expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl(cached = TRUE)")
  testthat::expect_equal(items$ADAE$get_pull_callable()$get_call(), "radae(cached = TRUE)")
  testthat::expect_identical(adtte$get_raw_data, items$ADTTE$get_raw_data)

  # simulate pull with a click of the submit button
  for (connector in data$get_connectors()) {
    connector$pull()
  }

  testthat::expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)"
  )
  testthat::expect_equal(
    get_code(data, "ADTTE"),
    "library(package = \"random.cdisc.data\")\nADTTE <- radtte(cached = TRUE)"
  )
  testthat::expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADAE <- radae(cached = TRUE)"
  )
  testthat::expect_equal(
    get_code(data), "library(package = \"random.cdisc.data\")\nADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)\nADAE <- radae(cached = TRUE)" # nolint
  )
})

# RelationalData with mutliple datasets and connectors ----
testthat::test_that("RelationalData with mutliple datasets and connectors", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, N = 1)
  adsl_data <- rcd_data(adsl)

  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    x = radtte(cached = TRUE),
    code = "ADTTE <- radtte(cached = TRUE)"
  )

  adae <- rcd_cdisc_dataset_connector("ADAE", radae, ADSL = adsl)
  # add custom input
  adae$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "ADSL seed", min = 0, value = 2),
      optionalSliderInput(inputId = ns("max_n_aes"),
                          label = "Maximum number of AEs per patient",
                          min = 0,
                          max = 5,
                          value = 3,
                          step = 1)
      )
    }
  )

  advs <- rcd_cdisc_dataset_connector("ADVS", radvs, ADSL = adsl)
  advs$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "ADSL seed", min = 0, value = 4),
      optionalSliderInput(inputId = ns("max_n_aes"),
                          label = "Number of weeks or cycles",
                          min = 0,
                          max = 10,
                          value = 5,
                          step = 1)
      )
    }
  )

  adlb <- rcd_cdisc_dataset_connector("ADLB", radlb, ADSL = adsl)

  advs_adlb_data <- rcd_data(advs, adlb)

  temp_file <- tempfile()
  saveRDS(radrs(cached = TRUE), file = temp_file)
  adrs <- rds_cdisc_dataset_connector(dataname = "ADRS", file = temp_file)

  adsamp <- script_cdisc_dataset_connector(
    dataname = "ADSAMP",
    keys = get_cdisc_keys("ADVS"),
    file = "delayed_data_script/asdamp_with_adsl.R",
    ADSL = adsl,
    ADVS = advs
  )

  data <- cdisc_data(adsl_data, adtte, adae, advs_adlb_data, adrs, adsamp)

  testthat::expect_true(inherits(data, "RelationalData"))
  items <- data$get_items()
  testthat::expect_true(all(vapply(items[-2], inherits, logical(1), "DatasetConnector")))
  testthat::expect_true(inherits(items$ADTTE, "Dataset"))

  testthat::expect_equal(items$ADSL$get_pull_callable()$get_call(), "radsl(N = 1)")
  testthat::expect_equal(items$ADAE$get_pull_callable()$get_call(), "radae(ADSL = ADSL)")
  testthat::expect_equal(items$ADVS$get_pull_callable()$get_call(), "radvs(ADSL = ADSL)")
  testthat::expect_equal(items$ADLB$get_pull_callable()$get_call(), "radlb(ADSL = ADSL)")
  testthat::expect_equal(
    items$ADSAMP$get_pull_callable()$get_call(),
    "source(file = \"delayed_data_script/asdamp_with_adsl.R\", local = TRUE)$value"
  )
  testthat::expect_identical(adtte$get_raw_data, items$ADTTE$get_raw_data)

  testthat::expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)"
  )
  testthat::expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADAE <- radae(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(data, "ADVS"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADVS <- radvs(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(data, "ADLB"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADLB <- radlb(ADSL = ADSL)"
  )
  testthat::expect_equal(
    get_code(data, "ADSAMP"),
    "library(package = \"random.cdisc.data\")\nADSL <- radsl(N = 1)\nADVS <- radvs(ADSL = ADSL)\nADSAMP <- source(file = \"delayed_data_script/asdamp_with_adsl.R\", local = TRUE)$value" # nolint
  )
  testthat::expect_equal(
    get_code(data, "ADTTE"),
    "library(package = \"random.cdisc.data\")\nADTTE <- radtte(cached = TRUE)"
  )

  # can the shiny app be initialized without error?
  mods <- teal:::get_dummy_modules()
  testthat::expect_message(
    init(data = data, modules = mods)
  )
})
