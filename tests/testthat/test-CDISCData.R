adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
adae_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))
adrs_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADRS"))))

# 1. single dataset / dataset code -------------------------------
testthat::test_that("single dataset / dataset code", {
  adsl_dataset <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
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
      cdisc_dataset("ADSL", adsl_raw),
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
    get_raw_data(data$get_dataset("ADSL")),
    adsl_raw
  )

  testthat::expect_true(data$check())
  data$execute_mutate()
  testthat::expect_false(data$check())
  data$check_metadata()

  new_env <- new.env()
  eval(parse(text = get_code(data, dataname ="ADSL")), envir = new_env)
  testthat::expect_identical(
    get(x = "ADSL", envir = new_env),
    get_raw_data(data$get_dataset("ADSL"))
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
    get_code(data),
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

  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    x = adtte_raw,
    code = "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))"
  )

  testthat::expect_error(
    cdisc_data(
      adsl,
      adtte,
      code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl\nADTTE <- synthetic_cdisc_data(\"latest\")$adtte", #nolint
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
    get_raw_data(data$get_dataset("ADSL")),
    adsl_raw
  )
  testthat::expect_reference(
    get_raw_data(data$get_dataset("ADTTE")),
    adtte_raw
  )
  data$execute_mutate()

  new_env <- new.env()
  eval(parse(text = get_code(data)), envir = new_env)
  testthat::expect_identical(
    get(x = "ADSL", envir = new_env),
    get_raw_data(data$get_dataset("ADSL"))
  )
  testthat::expect_identical(
    get(x = "ADTTE", envir = new_env),
    get_raw_data(data$get_dataset("ADTTE"))
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
    data$get_code_class(FALSE)$get_code(),
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

  load_dataset(adtte)
  load_dataset(adlb)
  testthat::expect_silent(data$check())

  # MUTATE
  adsl <- cdisc_dataset("ADSL", adsl_raw)
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))
  data <- cdisc_data(
    adsl,
    adtte,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
    check = TRUE
  ) %>%
    mutate_data(code = "ADSL <- dplyr::filter(ADSL, USUBJID == 'a')") %>%
    mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, !(USUBJID %in% ADSL$USUBJID))") %>%
    mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")

  testthat::expect_identical(
    get_code(data),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, !(USUBJID %in% ADSL$USUBJID))",
      "ADSL$x <- 1",
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
    get_code(data, "ADTTE"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, !(USUBJID %in% ADSL$USUBJID))",
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
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )

  load_dataset(adtte)
  testthat::expect_true(data$check()) # TRUE
  data$execute_mutate()
  testthat::expect_identical(
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 1L, ADTTE = 1L)
  )
})

# 5. only connectors ------
testthat::test_that("only connectors", {
  adsl_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
    }
  )
  adsl <- cdisc_dataset_connector(
    dataname = "ADSL",
    pull_callable = adsl_cf,
    keys = get_cdisc_keys("ADSL")
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))

  testthat::expect_error(
    cdisc_data(adsl, adtte, code = "test", check = TRUE),
    "Connectors are reproducible by default and setting 'code' argument might break it"
  )

  testthat::expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE) %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$test <- 1") %>%
      mutate_dataset(
        dataname = "ADTTE",
        code = "ADTTE$test <- ADSL$test",
        vars = list(ADSL = adsl)) %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    paste(
      "ADSL <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "})()",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code("ADSL"),
    paste(
      "ADSL <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "})()",
      "x <- ADSL",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code("ADTTE"),
    paste(
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    get_code(data, "ADSL"),
    paste(
      "ADSL <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "})()",
      "x <- ADSL",
      "ADSL$test <- 1",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    get_code(data, "ADTTE"),
    paste(
      "ADSL <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      "})()",
      "x <- ADSL",
      "ADTTE <- (function() {",
      "    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "})()",
      "ADSL$test <- 1",
      "ADTTE$test <- ADSL$test",
      sep = "\n"
    )
  )

  load_dataset(adsl)
  load_dataset(adtte)
  testthat::expect_true(data$check())
})

testthat::test_that("Basic example - without code and check", {
  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", adsl_raw), code = "", check = FALSE))
  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", adsl_raw),
    cdisc_dataset("ARG1", adsl_raw, keys = get_cdisc_keys("ADSL")),
    cdisc_dataset("ARG2", adsl_raw, keys = get_cdisc_keys("ADSL")), code = "", check = FALSE))
})

testthat::test_that("Basic example - check overall code", {
  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      cdisc_dataset("ARG1", adsl_raw, keys = get_cdisc_keys("ADSL")),
      cdisc_dataset("ARG2", adsl_raw, keys = get_cdisc_keys("ADSL")),
      code = "ADSL <- ARG1 <- ARG2 <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      check = TRUE
    )
  )

  testthat::expect_error(
    cdisc_data(
      cdisc_dataset(
        "ADSL",
        adsl_raw,
        code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
      ),
      cdisc_dataset("ARG1", adsl_raw, keys = get_cdisc_keys("ADSL")),
      cdisc_dataset("ARG2", adsl_raw, keys = get_cdisc_keys("ADSL")),
      code = "test",
      check = TRUE
    ),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )

  testthat::expect_error(
    cdisc_data(
      cdisc_dataset(
        "ADSL",
        adsl_raw,
        code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
      ),
      cdisc_dataset(
        dataname = "ARG1",
        x = dplyr::mutate(adsl_raw, x1 = 1),
        keys = get_cdisc_keys("ADSL"),
        code = "ARG1 <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
      ),
      cdisc_dataset(
        "ARG2",
        adsl_raw,
        keys = get_cdisc_keys("ADSL"),
        code = "ARG2 <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
      ),
      check = TRUE
    ),
    "Reproducibility check failed."
  )
})

testthat::test_that("Basic example - dataset depending on other dataset", {
  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset(
        "ADSL",
        adsl_raw,
        code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"),
      check = TRUE
    )
  )

  arg2 <- cdisc_dataset(
    dataname = "ARG2", x = adsl_raw, keys = get_cdisc_keys("ADSL"),
    code = "ARG2 <- ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )

  arg1 <- cdisc_dataset(
    dataname = "ARG1",
    x = adsl_raw,
    keys = get_cdisc_keys("ADSL"),
    code = "ARG1 <- ARG2",
    vars = list(ARG2 = arg2)
  )

  adsl <- cdisc_dataset(dataname = "ADSL", x = adsl_raw, code = "ADSL <- ARG2", vars = list(ARG2 = arg2))

  testthat::expect_silent(cd <- cdisc_data(arg2, arg1, adsl, check = TRUE))

  testthat::expect_true(arg1$check())
  testthat::expect_true(arg2$check())
  testthat::expect_true(adsl$check())
  testthat::expect_true(cd$check())
})

testthat::test_that("Basic example - failing dataset code", {
  testthat::expect_silent(
    dataset <- cdisc_dataset("ADSL", head(iris), code = "ADSL <- data.frame(a = 1, b = 2)")
  )
  testthat::expect_false(dataset$check())
})

testthat::test_that("Basic example - missing code for dataset", {
  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      code = c("ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"),
      check = TRUE
    )
  )

  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      code = c("ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"),
      check = FALSE
    )
  )
})

testthat::test_that("Code argument of the constructor can have line breaks", {
  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      code = "head(iris)\n 7", check = FALSE
    )
  )
})

testthat::test_that("Naming list elements", {
  testthat::expect_identical(names(get_datasets(cdisc_data(cdisc_dataset("ADSL", adsl_raw)))), "ADSL")
  testthat::expect_identical(
    names(get_datasets(cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      cdisc_dataset("ADTTE", adtte_raw),
      cdisc_dataset("ADRS", adrs_raw)
    ))),
    c("ADSL", "ADTTE", "ADRS")
  )
})

testthat::test_that("List values", {
  test_relational_data_equal <- function(data1, data2) {
    testthat::expect_equal(data1$get_items(), data2$get_items())
    testthat::expect_equal(data1$get_join_keys(), data2$get_join_keys())
    testthat::expect_equal(data1$get_ui("test"), data2$get_ui("test"))
  }

  result <- cdisc_data(cdisc_dataset("ADSL", adsl_raw, label = "test_label"))

  datasets <- list(cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
    keys = c("STUDYID", "USUBJID"),
    parent = character(0),
    label = "test_label"
  ))

  result_to_compare <- do.call("cdisc_data", datasets)

  test_relational_data_equal(result, result_to_compare)

  result <- cdisc_data(cdisc_dataset("ADSL", adsl_raw), cdisc_dataset("ADTTE", adtte_raw))

  datasets <- list(
    cdisc_dataset(
      dataname = "ADSL",
      x = adsl_raw,
      keys = c("STUDYID", "USUBJID"),
      parent = character(0),
      label = character(0)
    ),
    cdisc_dataset(
      dataname = "ADTTE",
      x = adtte_raw,
      keys = c("STUDYID", "USUBJID", "PARAMCD"),
      parent = "ADSL",
      label = character(0)
    )
  )

  result_to_compare <- do.call("cdisc_data", datasets)

  test_relational_data_equal(result, result_to_compare)
})

testthat::test_that("get_cdisc_keys returns column names present in the cached datasets", {
  scda_data <- scda::synthetic_cdisc_data("latest")

  testthat::expect_true(all(get_cdisc_keys("ADSL") %in% names(scda_data$adsl)))

  testthat::expect_true(all(get_cdisc_keys("ADAE") %in% names(scda_data$adae)))

  testthat::expect_true(all(get_cdisc_keys("ADTTE") %in% names(scda_data$adtte)))

  testthat::expect_true(all(get_cdisc_keys("ADCM") %in% names(scda_data$adcm)))

  testthat::expect_true(all(get_cdisc_keys("ADLB") %in% names(scda_data$adlb)))

  testthat::expect_true(all(get_cdisc_keys("ADRS") %in% names(scda_data$adrs)))

  testthat::expect_true(all(get_cdisc_keys("ADVS") %in% names(scda_data$advs)))
})

testthat::test_that("Empty code", {
  # missing code
  result <- cdisc_data(cdisc_dataset("ADSL", adsl_raw), check = FALSE)
  testthat::expect_identical(get_code(result), "")

  # empty code
  result <- cdisc_data(cdisc_dataset("ADSL", adsl_raw), code = "", check = FALSE)
  testthat::expect_identical(get_code(result), "")

  # NULL code
  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", adsl_raw), code = NULL, check = FALSE))
})

testthat::test_that("Error - objects differs", {
  testthat::expect_error(
    cdisc_data(cdisc_dataset("ADSL", adsl_raw, code = "ADSL <- 2"), check = TRUE),
    "Code from ADSL need to return a data.frame"
  )

  testthat::expect_error(
    cdisc_data(cdisc_dataset("ADSL", adsl_raw, code = "ADSL <- data.frame()"), check = TRUE),
    "Reproducibility check failed."
  )

  testthat::expect_error(
    cdisc_data(cdisc_dataset("ADSL", adsl_raw, code = "ADSL <- mtcars;"), check = TRUE),
    "Reproducibility check failed."
  )
})

testthat::test_that("Error - ADSL is missing in cdisc_data", {
  testthat::expect_error({
    x <- cdisc_data(
      cdisc_dataset("ADTTE", adtte_raw),
      code = "ADTTE <- synthetic_cdisc_data(\"latest\")$adtte", check = FALSE
    )
    x$check_metadata()
  },
  "ADSL dataset is missing."
  )
})

testthat::test_that("Error - duplicated names", {
  testthat::expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      cdisc_dataset("ADSL", adsl_raw),
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
    teal_data(dataset(dataname = "ADSL", x = adsl_raw, keys = "non_existing_column")),
    "The join key specification requires dataset ADSL to contain the following columns: non_existing_column"
  )

  data2 <- cdisc_data(dataset("ADSL", adsl_raw), dataset("ADTTE", adtte_raw))
  testthat::expect_identical(
    data2$get_dataset("ADSL")$get_keys(),
    character(0)
  )
  testthat::expect_identical(
    data2$get_dataset("ADTTE")$get_keys(),
    character(0)
  )

  # we can have empty keys - then we don't check them
  testthat::expect_silent(data2$check_metadata())

  adsl_extended <- rbind(adsl_raw, get_cdisc_keys("ADSL"))
  ds <- cdisc_dataset("ADSL", adsl_extended, keys = get_cdisc_keys("ADSL")[1])
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
  c_data <- cdisc_data(
    cdisc_dataset("ADSL", adsl_raw)
  )

  testthat::expect_error(cdisc_data(c_data))
  testthat::expect_error(cdisc_data(cdisc_dataset("ADSL", adsl_raw), c_data))
})
