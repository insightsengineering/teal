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
testthat::test_that("Multiple connectors wrapped in cdisc_data", {

  example_data_connector <- function(...) {
    connectors <- list(...)
    open_fun <- callable_function(library)
    open_fun$set_args(list(package = "teal"))
    con <- DataConnection$new(open_fun = open_fun)
    RelationalDataConnector$new(connection = con, connectors = connectors)
  }

  adsl <- scda_cdisc_dataset_connector("ADSL", "adsl")
  adae <- scda_cdisc_dataset_connector("ADAE", "adae")
  advs <- scda_cdisc_dataset_connector("ADVS", "advs")
  adsl_2 <- code_cdisc_dataset_connector("ADSL_2", code = "ADSL",
    keys = get_cdisc_keys("ADSL"), ADSL = adsl)
  adsl_adae <- example_data_connector(adsl, adae)
  advs_adsl_2 <- example_data_connector(advs, adsl_2)
  data <- cdisc_data(adsl_adae, advs_adsl_2)

  items <- data$get_items()
  testthat::expect_true(inherits(data, "RelationalData"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "DatasetConnector")))
  testthat::expect_true(all(vapply(data$get_connectors(), inherits, logical(1), "RelationalDataConnector")))

  testthat::expect_equal(unname(get_dataname(data)), c("ADSL", "ADAE", "ADVS", "ADSL_2"))

  testthat::expect_equal(items$ADSL$get_code(),
    "ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")")
  testthat::expect_equal(items$ADAE$get_code(),
    "ADAE <- synthetic_cdisc_dataset(dataset_name = \"adae\", name = \"latest\")")
  testthat::expect_equal(items$ADVS$get_code(),
    "ADVS <- synthetic_cdisc_dataset(dataset_name = \"advs\", name = \"latest\")")
  testthat::expect_equal(items$ADSL_2$get_code(),
    "ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\nADSL_2 <- ADSL")

  testthat::expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"teal\")\nADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"teal\")\nADAE <- synthetic_cdisc_dataset(dataset_name = \"adae\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data, "ADVS"),
    "library(package = \"teal\")\nADVS <- synthetic_cdisc_dataset(dataset_name = \"advs\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data, "ADSL_2"),
    "library(package = \"teal\")\nADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\nADSL_2 <- ADSL" # nolint
  )
  testthat::expect_equal(
    get_code(data),
    paste0("library(package = \"teal\")\nADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\n",
      "ADAE <- synthetic_cdisc_dataset(dataset_name = \"adae\", name = \"latest\")\n",
      "ADVS <- synthetic_cdisc_dataset(dataset_name = \"advs\", name = \"latest\")\nADSL_2 <- ADSL"
    )
  )
})

# RelationalData with single dataset and connector ----
testthat::test_that("RelationalData with single dataset and connector", {
  example_data_connector <- function(...) {
    connectors <- list(...)
    open_fun <- callable_function(library)
    open_fun$set_args(list(package = "teal"))
    con <- DataConnection$new(open_fun = open_fun)
    RelationalDataConnector$new(connection = con, connectors = connectors)
  }

  adsl <- scda_cdisc_dataset_connector("ADSL", "adsl")
  adsl_data <- example_data_connector(adsl)

  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    x = synthetic_cdisc_dataset(dataset_name = "adtte", name = "latest") ,
    code = "ADTTE <- synthetic_cdisc_dataset(dataset_name = \"adtte\", name = \"latest\")"
  )

  adae <- scda_cdisc_dataset_connector("ADAE", "adae")
  adae$set_ui_input(function(ns) {
    list(
      textInput(inputId = ns("name"), label = "scda name", value = "latest")
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

  testthat::expect_equal(items$ADSL$get_pull_callable()$get_call(), "synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")")
  testthat::expect_equal(items$ADAE$get_pull_callable()$get_call(), "synthetic_cdisc_dataset(dataset_name = \"adae\", name = \"latest\")")
  testthat::expect_identical(adtte$get_raw_data, items$ADTTE$get_raw_data)

  # simulate pull with a click of the submit button
  for (connector in data$get_connectors()) {
    connector$pull()
  }

  testthat::expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"teal\")\nADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data, "ADTTE"),
    "library(package = \"teal\")\nADTTE <- synthetic_cdisc_dataset(dataset_name = \"adtte\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data, "ADAE"),
    "library(package = \"teal\")\nADAE <- synthetic_cdisc_dataset(dataset_name = \"adae\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data),
    paste0("library(package = \"teal\")\n",
           "ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\n",
           "ADTTE <- synthetic_cdisc_dataset(dataset_name = \"adtte\", name = \"latest\")\n",
           "ADAE <- synthetic_cdisc_dataset(dataset_name = \"adae\", name = \"latest\")"
    )
  )
})

# RelationalData with mutliple datasets and connectors ----
testthat::test_that("RelationalData with mutliple datasets and connectors", {

  example_data_connector <- function(...) {
    connectors <- list(...)
    open_fun <- callable_function(library)
    open_fun$set_args(list(package = "teal"))
    con <- teal:::DataConnection$new(open_fun = open_fun)
    x <- teal:::RelationalDataConnector$new(connection = con, connectors = connectors)
    x$set_ui(
      function(id, connection, connectors) {
        ns <- NS(id)
        tagList(
          connection$get_open_ui(ns("open_connection")),
          do.call(
            what = "tagList",
            args = lapply(
              connectors,
              function(connector) {
                div(
                  connector$get_ui(
                    id = ns(connector$get_dataname())
                  ),
                  br()
                )
              }
            )
          )
        )
      }
    )
    return(x)
  }

  adsl <- scda_cdisc_dataset_connector("ADSL", "adsl")
  adsl_data <- example_data_connector(adsl)

  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    x = synthetic_cdisc_dataset(dataset_name = "adtte", name = "latest"),
    code = "ADTTE <- synthetic_cdisc_dataset(dataset_name = \"adtte\", name = \"latest\")"
  )

  adsl_2 <- code_cdisc_dataset_connector("ADSL_2", "ADSL", keys = get_cdisc_keys("ADSL"), ADSL = adsl)
  # add custom input
  adsl_2$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "Example UI", min = 0, value = 2)
      )
    }
  )

  advs <- scda_cdisc_dataset_connector("ADVS", "advs")
  advs$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "Example UI", min = 0, value = 4)
    )}
  )

  adlb <- scda_cdisc_dataset_connector("ADLB", "adlb")

  advs_adlb_data <- example_data_connector(advs, adlb)

  temp_file <- tempfile()
  saveRDS(synthetic_cdisc_dataset(dataset_name = "adrs", name = "latest"), file = temp_file)
  adrs <- rds_cdisc_dataset_connector(dataname = "ADRS", file = temp_file)

  adsamp <- script_cdisc_dataset_connector(
    dataname = "ADSAMP",
    keys = get_cdisc_keys("ADVS"),
    file = "delayed_data_script/asdamp_with_adsl.R",
    ADSL = adsl,
    ADVS = advs
  )

  data <- cdisc_data(adsl_data, adtte, adsl_2, advs_adlb_data, adrs, adsamp)

  testthat::expect_true(inherits(data, "RelationalData"))
  items <- data$get_items()
  testthat::expect_true(all(vapply(items[-2], inherits, logical(1), "DatasetConnector")))
  testthat::expect_true(inherits(items$ADTTE, "Dataset"))

  testthat::expect_equal(items$ADSL$get_pull_callable()$get_call(), "synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")")
  testthat::expect_equal(items$ADSL_2$get_pull_callable()$get_call(), "ADSL")
  testthat::expect_equal(items$ADVS$get_pull_callable()$get_call(), "synthetic_cdisc_dataset(dataset_name = \"advs\", name = \"latest\")")
  testthat::expect_equal(items$ADLB$get_pull_callable()$get_call(), "synthetic_cdisc_dataset(dataset_name = \"adlb\", name = \"latest\")")
  testthat::expect_equal(
    items$ADSAMP$get_pull_callable()$get_call(),
    "source(file = \"delayed_data_script/asdamp_with_adsl.R\", local = TRUE)$value"
  )
  testthat::expect_identical(adtte$get_raw_data, items$ADTTE$get_raw_data)

  testthat::expect_equal(
    get_code(data, "ADSL"),
    "library(package = \"teal\")\nADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data, "ADSL_2"),
    "library(package = \"teal\")\nADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\nADSL_2 <- ADSL"
  )
  testthat::expect_equal(
    get_code(data, "ADVS"),
    "library(package = \"teal\")\nADVS <- synthetic_cdisc_dataset(dataset_name = \"advs\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data, "ADLB"),
    "library(package = \"teal\")\nADLB <- synthetic_cdisc_dataset(dataset_name = \"adlb\", name = \"latest\")"
  )
  testthat::expect_equal(
    get_code(data, "ADSAMP"),
    paste0("library(package = \"teal\")\n",
      "ADSL <- synthetic_cdisc_dataset(dataset_name = \"adsl\", name = \"latest\")\n",
      "ADVS <- synthetic_cdisc_dataset(dataset_name = \"advs\", name = \"latest\")\n",
      "ADSAMP <- source(file = \"delayed_data_script/asdamp_with_adsl.R\", local = TRUE)$value"
    )
  )
  testthat::expect_equal(
    get_code(data, "ADTTE"),
    "library(package = \"teal\")\nADTTE <- synthetic_cdisc_dataset(dataset_name = \"adtte\", name = \"latest\")"
  )

  # can the shiny app be initialized without error?
  mods <- teal:::get_dummy_modules()
  testthat::expect_message(
    init(data = data, modules = mods)
  )
})

testthat::test_that("RelationalData$print prints out expected output on basic input", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"), object = list(1:3, letters[1:3]))))
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))"
  )
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    x = adtte_raw,
    code = "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))"
  )
  data <- cdisc_data(adsl, adtte, check = TRUE)

  out <- capture.output(print(data))
  testthat::expect_equal(
    out,
    c("A CDISCData object containing 2 Dataset/DatasetConnector object(s) as element(s):",
      "--> Element 1:",
      "A Dataset object containing the following data.frame (3 rows and 2 columns):",
      "  STUDYID USUBJID",
      "1       1       a",
      "2       2       b",
      "3       3       c",
      "--> Element 2:",
      "A Dataset object containing the following data.frame (1 rows and 3 columns):",
      "  STUDYID USUBJID PARAMCD",
      "1 STUDYID USUBJID PARAMCD"
    )
  )

})
