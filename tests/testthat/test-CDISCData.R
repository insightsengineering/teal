library(scda)

ADSL <- ARG1 <- ARG2 <- synthetic_cdisc_data("rcd_2021_05_05")$adsl # nolint
ADTTE <- synthetic_cdisc_data("rcd_2021_05_05")$adtte # nolint
ADRS <- synthetic_cdisc_data("rcd_2021_05_05")$adrs # nolint

# 1. single dataset / dataset code -------------------------------
testthat::test_that("single dataset / dataset code", {
  adsl <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adsl_dataset <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl,
    code = "as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  testthat::expect_silent(
    data <- cdisc_data(adsl_dataset, check = TRUE)
  )

  testthat::expect_true(adsl_dataset$check())
  testthat::expect_true(data$check())
  testthat::expect_identical(get_code(data), "as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))")
  testthat::expect_identical(get_code(data), get_code(adsl_dataset))

  # MUTATE
  testthat::expect_silent(
    data <- cdisc_data(
      cdisc_dataset("ADSL", adsl),
      code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      check = TRUE
    ) %>% mutate_data(code = "ADSL <- dplyr::filter(ADSL, USUBJID == 'F')")
  )

  testthat::expect_true(data$check())
  testthat::expect_identical(
    get_code(data),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"F\")",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  testthat::expect_identical(
    data$get_code_class(FALSE)$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"F\")",
      sep = "\n"
    )
  )
  testthat::expect_reference(
    data$get_dataset("ADSL")$get_raw_data(),
    adsl
  )

  testthat::expect_true(data$check())
  data$execute_mutate()
  testthat::expect_false(data$check())
  data$check_metadata()

  new_env <- new.env()
  eval(parse(text = data$get_code("ADSL")), envir = new_env)
  testthat::expect_identical(
    get(x = "ADSL", envir = new_env),
    data$get_dataset("ADSL")$get_raw_data()
  )
})

# 2. two datasets / datasets code -------------------------------
testthat::test_that("two datasets / datasets code", {
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

  testthat::expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE)
  )

  testthat::expect_true(adsl$check())
  testthat::expect_true(adtte$check())
  testthat::expect_true(data$check())

  testthat::expect_identical(
    get_code(adsl),
    "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))"
  )
  testthat::expect_identical(
    get_code(adtte),
    "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))"
  )
  testthat::expect_identical(
    get_code(data),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      sep = "\n"
    )
  )

  # MUTATE
  testthat::expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE)  %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL <- dplyr::filter(ADSL, USUBJID == 'a')") %>%
      mutate_dataset(
        dataname = "ADTTE",
        code = "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
        vars = list(ADSL = adsl)
      ) %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )

  testthat::expect_true(data$check())
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADSL"),
    "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))"
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADTTE"),
    "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))"
  )

  testthat::expect_identical(
    data$get_code_class(FALSE)$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADSL"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    get_code(data, "ADTTE"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    vapply(get_raw_data(data), nrow, numeric(1)),
    c(ADSL = 3, ADTTE = 1)
  )

  data$execute_mutate()
  testthat::expect_identical(
    vapply(get_raw_data(data), nrow, numeric(1)),
    c(ADSL = 1, ADTTE = 1)
  )
})

testthat::test_that("Duplicated code from datasets is shown", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adae_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))
  some_var <- "TEST"
  adsl_raw$test <- some_var

  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "
      ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))
      some_var <- 'TEST'
      ADSL$test <- some_var"
  )

  adae <- cdisc_dataset(
    dataname = "ADAE",
    x = adae_raw,
    code = "
      ADAE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADAE\"))))"
  )

  data <- cdisc_data(adsl, adae)

  testthat::expect_equal(
    data$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "some_var <- \"TEST\"",
      "ADSL$test <- some_var",
      "ADAE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADAE\"))))",
      sep = "\n"
    )
  )
})

# 3. two datasets / global code -------------------------------
testthat::test_that("two datasets / datasets code", {
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

  testthat::expect_error(
    cdisc_data(
      adsl,
      adtte,
      code = "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nADTTE <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte", #nolint
      check = TRUE
    )
  )

  testthat::expect_silent(
    data <- cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      cdisc_dataset("ADTTE", adtte_raw),
      code = paste(
        "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
        "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
        sep = "\n"
      ),
      check = TRUE
    )
  )

  testthat::expect_identical(
    get_code(data),
    paste(
        "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
        "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
        sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADSL"),
    get_code(data),
  )
  testthat::expect_identical(
    get_code(data, "ADTTE"),
    get_code(data)
  )
  testthat::expect_error(cdisc_dataset("ADSL", adsl_raw)$check(), "code is empty")
  testthat::expect_error(cdisc_dataset("ADTTE", adtte_raw)$check(), "code is empty")
  testthat::expect_true(data$check())

  # MUTATE
  adsl <- cdisc_dataset(dataname = "ADSL", x = adsl_raw)
  adtte <- cdisc_dataset(dataname = "ADTTE", x = adtte_raw)

  testthat::expect_silent(
    data <- cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      cdisc_dataset("ADTTE", adtte_raw),
      code = paste(
        "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
        "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
        sep = "\n"
      ),
      check = TRUE
    ) %>%
      mutate_data(code = "ADSL <- dplyr::filter(ADSL, USUBJID == 'a')") %>%
      mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )


  testthat::expect_error(adsl$check(), "code is empty")
  testthat::expect_error(adtte$check(), "code is empty")
  testthat::expect_true(data$check())

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADTTE"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    get_code(data, "ADSL"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )

  testthat::expect_reference(
    data$get_dataset("ADSL")$get_raw_data(),
    adsl_raw
  )
  testthat::expect_reference(
    data$get_dataset("ADTTE")$get_raw_data(),
    adtte_raw
  )
  data$execute_mutate()

  new_env <- new.env()
  eval(parse(text = data$get_code()), envir = new_env)
  testthat::expect_identical(
    get(x = "ADSL", envir = new_env),
    data$get_dataset("ADSL")$get_raw_data()
  )
  testthat::expect_identical(
    get(x = "ADTTE", envir = new_env),
    data$get_dataset("ADTTE")$get_raw_data()
  )
})

# 4. dataset + connector / code for dataset -------------------------------
testthat::test_that("dataset + connector / global code", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"), object = list(1:3, letters[1:3]))))
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))"
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))

  testthat::expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE)
  )

  testthat::expect_identical(
    get_code(data),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    get_code(data, "ADSL"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    get_code(data, "ADTTE"),
    paste(
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )

  testthat::expect_true(data$check())
  testthat::expect_silent(load_dataset(adtte))
  testthat::expect_true(data$check())

  # MUTATE
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))"
  )
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADSL"), vars = list(x = adsl))

  testthat::expect_silent(
    data <- cdisc_data(
      adsl,
      adtte,
      check = TRUE
    ) %>%
      mutate_data(code = "ADSL <- dplyr::filter(ADSL, USUBJID == 'a')") %>%
      mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )

  testthat::expect_identical(
    get_code(data),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADTTE"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADSL"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADSL"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADTTE"),
    paste(
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADTTE"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
      sep = "\n"
    )
  )

  testthat::expect_true(data$check())
  load_dataset(adtte)
  testthat::expect_true(data$check())
})

# 5.dataset + connector / global code
testthat::test_that("two datasets / datasets code", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"), object = list(1:3, letters[1:3]))))
  adsl <- cdisc_dataset("ADSL", adsl_raw)
  adrs_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADRS"))))
  adrs <- cdisc_dataset("ADRS", adrs_raw)
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADSL"), vars = list(x = adsl))
  adlb_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADLB"))))
    }
  )
  adlb <- cdisc_dataset_connector("ADLB", adlb_cf, keys = get_cdisc_keys("ADLB"), vars = list(x = adsl))

  testthat::expect_identical(
    get_code(adtte),
    paste(
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )

  data <- cdisc_data(
    adsl,
    adlb,
    adtte,
    adrs,
    code = paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADRS <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADRS\"))))",
      sep = "\n"
    ),
    check = TRUE
  )

  testthat::expect_identical(
    get_code(data),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADRS <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADRS\"))))",
      "x <- ADSL",
      "ADLB <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADLB\"))))",
      "})()",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADSL"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADRS <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADRS\"))))",
      "x <- ADSL",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADTTE"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADRS <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADRS\"))))",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADRS <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADRS\"))))",
      "x <- ADSL",
      "ADLB <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADLB\"))))",
      "})()",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADSL"),
      paste(
        "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
        "ADRS <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADRS\"))))",
        "x <- ADSL",
        sep = "\n"
      )
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADTTE"),
    "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nADRS <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adrs\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()" #nolint
  )

  testthat::expect_identical(
    data$get_code_class(FALSE)$get_code(),
    "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nADRS <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adrs\nx <- ADSL\nADLB <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adlb\n})()\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()" # nolint
  )

  load_dataset(adtte)
  load_dataset(adlb)
  testthat::expect_silent(data$check())

  # MUTATE
  adsl <- cdisc_dataset("ADSL", ADSL)
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADSL"), vars = list(x = adsl))
  data <- cdisc_data(adsl, adtte, code = "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl", check = TRUE) %>%
    mutate_data(code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
    mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
    mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")

  testthat::expect_identical(
    get_code(data),
    "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nx <- ADSL\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)\nADSL$x <- 1" #nolint
  )

  testthat::expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nx <- ADSL\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADSL$x <- 1" #nolint
  )

  testthat::expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nx <- ADSL\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)" #nolint
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nx <- ADSL\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()" #nolint
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADSL"),
    "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nx <- ADSL"
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADTTE"),
    "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()" #nolint
  )

  load_dataset(adtte)
  testthat::expect_true(data$check()) # TRUE
  data$execute_mutate()
  testthat::expect_identical(
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 231L, ADTTE = 924L)
  )
})

# 5. only connectors ------
testthat::test_that("only connectors", {
  adsl_cf <- callable_function(
    function() {
      synthetic_cdisc_data("latest")$adsl
    }
  )
  adsl <- cdisc_dataset_connector(
    dataname = "ADSL",
    pull_callable = adsl_cf,
    keys = get_cdisc_keys("ADSL")
  )
  adtte_cf <- callable_function(
    function() {
      synthetic_cdisc_data("rcd_2021_05_05")$adtte
    }
  )
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADSL"), vars = list(x = adsl))

  testthat::expect_error(
    cdisc_data(adsl, adtte, code = "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl", check = TRUE),
    "Connectors are reproducible by default and setting 'code' argument might break it"
  )

  testthat::expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE) %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
      mutate_dataset(
        dataname = "ADTTE",
        code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
        vars = list(ADSL = adsl)) %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- (function() {\n    synthetic_cdisc_data(\"latest\")$adsl\n})()\nx <- ADSL\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()" #nolint
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code("ADSL"),
    "ADSL <- (function() {\n    synthetic_cdisc_data(\"latest\")$adsl\n})()\nx <- ADSL"
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code("ADTTE"),
    "ADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()"
  )

  testthat::expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- (function() {\n    synthetic_cdisc_data(\"latest\")$adsl\n})()\nx <- ADSL\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADSL$x <- 1" #nolint
  )
  testthat::expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- (function() {\n    synthetic_cdisc_data(\"latest\")$adsl\n})()\nx <- ADSL\nADTTE <- (function() {\n    synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte\n})()\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)" # nolint
  )

  load_dataset(adsl)
  load_dataset(adtte)
  testthat::expect_true(
    data$check()
  )
})

# 6. mutate -----
testthat::test_that("only connectors", {
  adsl_cf <- callable_function(
    function() {
      synthetic_cdisc_data("latest")$adsl
    }
  )
  adsl <- cdisc_dataset_connector(
    dataname = "ADSL",
    pull_callable = adsl_cf,
    keys = get_cdisc_keys("ADSL")
  )
  adtte_cf <- callable_function(
    function() {
      synthetic_cdisc_data("rcd_2021_05_05")$adtte
    }
  )
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADSL"), vars = list(x = adsl))

  testthat::expect_error(
    cdisc_data(adsl, adtte, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl", check = TRUE),
    "Connectors are reproducible by default and setting 'code' argument might break it"
  )


})
testthat::test_that("Basic example cdisc dataset", {
  simple_cdisc_dataset <- cdisc_dataset("ADSL", ADSL)

  testthat::expect_identical(ADSL, simple_cdisc_dataset$data)
  testthat::expect_identical("ADSL", simple_cdisc_dataset$get_dataname())
  testthat::expect_true(class(simple_cdisc_dataset)[1] == "CDISCDataset")
})

testthat::test_that("Basic example - without code and check", {
  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE))
  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
    cdisc_dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
    cdisc_dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")), code = "", check = FALSE))
})

testthat::test_that("Basic example - check overall code", {
  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      cdisc_dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
      cdisc_dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
      code = "ADSL <- ARG1 <- ARG2 <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl;",
      check = TRUE
    )
  )

  testthat::expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"),
      cdisc_dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
      cdisc_dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
      code = "ARG1 <- ARG2 <- cadsl;",
      check = TRUE
    ),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )

  testthat::expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"),
      cdisc_dataset(
        dataname = "ARG1",
        x = dplyr::mutate(ADSL, x1 = 1),
        keys = get_cdisc_keys("ADSL"),
        code = "ARG1 <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"
      ),
      cdisc_dataset(
        "ARG2", ADSL, keys = get_cdisc_keys("ADSL"),
        code = "ARG2 <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"
      ),
      check = TRUE
    ),
    "Reproducibility check failed."
  )
})

testthat::test_that("Basic example - dataset depending on other dataset", {
  testthat::expect_true(is.data.frame(ADSL))
  testthat::expect_true(is.data.frame(ARG1))
  testthat::expect_true(is.data.frame(ARG2))

  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"),
      check = TRUE
    )
  )
  testthat::expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"),
      cdisc_dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL"), code = "ARG1 <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"), #nolint
      cdisc_dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL"), code = "ARG2 <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"), #nolint
      code = "ADSL <- ARG1 <- ARG2 <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl;",
      check = TRUE
    ),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )

  arg2 <- cdisc_dataset(
    dataname = "ARG2", x = ARG2, keys = get_cdisc_keys("ADSL"),
    code = "ARG2 <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"
  )

  arg1 <- cdisc_dataset(
    dataname = "ARG1",
    x = ARG1,
    keys = get_cdisc_keys("ADSL"),
    code = "ARG1 <- ARG2",
    vars = list(ARG2 = arg2)
  )

  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL, code = "ADSL <- ARG2", vars = list(ARG2 = arg2))

  testthat::expect_silent(cd <- cdisc_data(arg2, arg1, adsl, check = TRUE))

  testthat::expect_true(arg1$check())
  testthat::expect_true(arg2$check())
  testthat::expect_true(adsl$check())
  testthat::expect_true(cd$check())
})

testthat::test_that("Basic example - failing dataset code", {
  testthat::expect_silent(
    dataset <- cdisc_dataset("ADSL", ADSL, code = "ADSL <- data.frame(a = 1, b = 2)")
  )
  testthat::expect_false(dataset$check())
})

testthat::test_that("Basic example - missing code for dataset", {
  testthat::expect_true(is.data.frame(ADSL))
  testthat::expect_true(is.data.frame(ARG1))
  testthat::expect_true(is.data.frame(ARG2))

  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      code = c("ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"),
      check = TRUE
    )
  )

  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      code = c("ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl"),
      check = FALSE
    )
  )
})

testthat::test_that("Basic example - with line break code and check", {
  testthat::expect_true(is.data.frame(ADSL))
  testthat::expect_true(is.data.frame(ARG1))
  testthat::expect_true(is.data.frame(ARG2))

  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           code = "ADSL <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adsl\n ADSL$x1 <- 1", check = FALSE)
                )
})

testthat::test_that("Naming list elements", {
  testthat::expect_identical(names(get_datasets(cdisc_data(cdisc_dataset("ADSL", ADSL)))), "ADSL")
  testthat::expect_identical(
    names(get_datasets(cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      cdisc_dataset("ADTTE", ADTTE),
      cdisc_dataset("ADRS", ADRS)))),
    c("ADSL", "ADTTE", "ADRS"))
})

testthat::test_that("List values", {
  test_relational_data_equal <- function(data1, data2) {
    testthat::expect_equal(data1$get_items(), data2$get_items())
    testthat::expect_equal(data1$get_join_keys(), data2$get_join_keys())
    testthat::expect_equal(data1$get_ui("test"), data2$get_ui("test"))
  }

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL))

  adsl_yaml <- yaml::yaml.load_file(system.file("metadata/ADSL.yml", package = "random.cdisc.data", mustWork = TRUE))
  adtte_yaml <- yaml::yaml.load_file(system.file("metadata/ADTTE.yml", package = "random.cdisc.data", mustWork = TRUE))

  datasets <- list(cdisc_dataset(
    dataname = "ADSL",
    x = ADSL,
    keys = c("STUDYID", "USUBJID"),
    parent = character(0),
    label = adsl_yaml$domain$label
  ))

  result_to_compare <- do.call("cdisc_data", datasets)

  test_relational_data_equal(result, result_to_compare)

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), cdisc_dataset("ADTTE", ADTTE))

  datasets <- list(
    cdisc_dataset(
      dataname = "ADSL",
      x = ADSL,
      keys = c("STUDYID", "USUBJID"),
      parent = character(0),
      label = adsl_yaml$domain$label
    ),
    cdisc_dataset(
      dataname = "ADTTE",
      x = ADTTE,
      keys = c("STUDYID", "USUBJID", "PARAMCD"),
      parent = "ADSL",
      label = adtte_yaml$domain$label
    ))

  result_to_compare <- do.call("cdisc_data", datasets)

  test_relational_data_equal(result, result_to_compare)
})

testthat::test_that("Keys in cached datasets", {
  testthat::expect_true(all(get_cdisc_keys("ADSL") %in% names(random.cdisc.data::cadsl)))

  testthat::expect_true(all(get_cdisc_keys("ADAE") %in% names(random.cdisc.data::cadae)))

  testthat::expect_true(all(get_cdisc_keys("ADTTE") %in% names(random.cdisc.data::cadtte)))

  testthat::expect_true(all(get_cdisc_keys("ADCM") %in% names(random.cdisc.data::cadcm)))

  testthat::expect_true(all(get_cdisc_keys("ADLB") %in% names(random.cdisc.data::cadlb)))

  testthat::expect_true(all(get_cdisc_keys("ADRS") %in% names(random.cdisc.data::cadrs)))

  testthat::expect_true(all(get_cdisc_keys("ADVS") %in% names(random.cdisc.data::cadvs)))
})

testthat::test_that("Empty code", {
  # missing code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), check = FALSE)
  testthat::expect_identical(get_code(result), "")

  # empty code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE)
  testthat::expect_identical(get_code(result), "")

  # NULL code
  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL), code = NULL, check = FALSE))
})

testthat::test_that("Error - objects differs", {
  testthat::expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL, code = "ADSL <- 2"), check = TRUE),
    "Code from ADSL need to return a data.frame"
  )

  testthat::expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL, code = "ADSL <- data.frame()"), check = TRUE),
    "Reproducibility check failed."
  )

  testthat::expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL, code = "ADSL <- mtcars;"), check = TRUE),
    "Reproducibility check failed."
  )
})

testthat::test_that("Error - ADSL is missing in cdisc_data", {
  testthat::expect_error({
    x <- cdisc_data(
      cdisc_dataset("ADTTE", ADTTE),
      code = "ADTTE <- synthetic_cdisc_data(\"rcd_2021_05_05\")$adtte", check = FALSE
    )
    x$check_metadata()
  },
  "ADSL dataset is missing."
  )
})

testthat::test_that("Error - duplicated names", {
  testthat::expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      cdisc_dataset("ADSL", ADSL),
      code = "",
      check = FALSE
    ),
    "Datasets names should be unique"
  )
})

testthat::test_that("Error - dataset is not of correct class", {
  testthat::expect_error(
    cdisc_data(ARG1 = 1, code = "", check = FALSE),
    "All elements should be of Dataset(Connector) or RelationalDataConnector class",
    fixed = TRUE
  )
})

testthat::test_that("Check the keys", {
  testthat::expect_error(
    teal_data(dataset(dataname = "ADSL", x = ADSL, keys = "non_existing_column")),
    "The join key specification requires dataset ADSL to contain the following columns: non_existing_column"
  )

  data2 <- cdisc_data(dataset("ADSL", ADSL), dataset("ADTTE", ADTTE))
  testthat::expect_identical(
    data2$get_dataset("ADSL")$get_keys(),
    character(0)
  )
  testthat::expect_identical(
    data2$get_dataset("ADTTE")$get_keys(),
    character(0)
  )

  # we can have a empty keys - then we don't check them
  testthat::expect_silent(data2$check_metadata())

  ds <- cdisc_dataset("ADSL", ADSL, keys = c("SEX"))
  testthat::expect_error(
    ds$check_keys(),
    "Duplicate primary key values found in the dataset 'ADSL'"
  )

  testthat::expect_error(
    cdisc_data(ds),
    "Duplicate primary key values found in the dataset 'ADSL'"
  )
})

# 7. invalid arguments -----
testthat::test_that("Cannot create RelationData if arguments include RelationalData object", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"), object = list(1:3, letters[1:3]))))
  c_data <- cdisc_data(
    cdisc_dataset("ADSL", adsl_raw)
  )

  testthat::expect_error(cdisc_data(c_data))
  testthat::expect_error(cdisc_data(cdisc_dataset("ADSL", ADSL), c_data))
})
