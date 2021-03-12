library(random.cdisc.data)

ADSL <- ARG1 <- ARG2 <- radsl(cached = TRUE) # nolint
ADTTE <- radtte(cached = TRUE) # nolint
ADRS <- radrs(cached = TRUE) # nolint

# 1. single dataset / dataset code -------------------------------
test_that("single dataset / dataset code", {
  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL, code = "ADSL <- radsl(cached = TRUE)")
  expect_silent(
    data <- cdisc_data(adsl, check = TRUE)
  )

  expect_true(adsl$check())
  expect_true(data$check())
  expect_identical(get_code(data), "ADSL <- radsl(cached = TRUE)")
  expect_identical(get_code(data), get_code(adsl))

  # MUTATE
  expect_silent(
    data <- cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      code = "ADSL <- radsl(cached = TRUE)",
      check = TRUE
    ) %>% mutate_data(code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')")
  )

  expect_true(data$check())
  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")"
  )

  expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- radsl(cached = TRUE)"
  )
  expect_identical(
    data$get_code_class(FALSE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")"
  )
  expect_reference(
    data$get_dataset("ADSL")$get_raw_data(),
    ADSL
  )

  expect_true(data$check())
  data$execute_mutate()
  expect_false(data$check())
  data$check_metadata()

  new_env <- new.env()
  eval(parse(text = data$get_code("ADSL")), envir = new_env)
  expect_identical(
    get(x = "ADSL", envir = new_env),
    data$get_dataset("ADSL")$get_raw_data()
  )
})

# 2. two datasets / datasets code -------------------------------
test_that("two datasets / datasets code", {
  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL, code = "ADSL <- radsl(cached = TRUE)")
  adtte <- cdisc_dataset(dataname = "ADTTE", x = ADTTE, code = "ADTTE <- radtte(cached = TRUE)")

  expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE)
  )

  expect_true(adsl$check())
  expect_true(adtte$check())
  expect_true(data$check())


  expect_identical(
    get_code(adsl),
    "ADSL <- radsl(cached = TRUE)"
  )
  expect_identical(
    get_code(adtte),
    "ADTTE <- radtte(cached = TRUE)"
  )
  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)"
  )

  # MUTATE
  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL, code = "ADSL <- radsl(cached = TRUE)")
  adtte <- cdisc_dataset(dataname = "ADTTE", x = ADTTE, code = "ADTTE <- radtte(cached = TRUE)")

  expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE)  %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
      mutate_dataset(
        dataname = "ADTTE",
        code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
        vars = list(ADSL = adsl)
      ) %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )

  expect_true(data$check())
  expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)"
  )

  expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADSL"),
    "ADSL <- radsl(cached = TRUE)"
  )
  expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADTTE"),
    "ADTTE <- radtte(cached = TRUE)"
  )

  expect_identical(
    data$get_code_class(FALSE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)\nADSL$x <- 1" #nolint
  )

  expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- radsl(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADSL$x <- 1"
  )
  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)" #nolint
  )

  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)\nADSL$x <- 1" #nolint
  )

  expect_identical(
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 400L, ADTTE = 1600L)
  )

  data$execute_mutate()
  expect_identical(
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 231L, ADTTE = 924L)
  )
})

test_that("Duplicated code from datasets is shown", {
  some_var <- TRUE
  adae <- radae(cached = some_var)
  adsl <- radsl(cached = some_var)
  some_var <- "TEST"
  adsl$test <- some_var

  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl,
    code = "some_var <- TRUE
      ADSL <- radsl(cached = some_var)
      some_var <- 'TEST'
      ADSL$test <- some_var"
  )

  adae <- cdisc_dataset(
    dataname = "ADAE",
    x = adae,
    code = "some_var <- TRUE
      ADAE <- radae(cached = some_var)"
  )

  data <- cdisc_data(adsl, adae, check = TRUE)

  expect_equal(
    data$get_code(),
    "some_var <- TRUE\nADSL <- radsl(cached = some_var)\nsome_var <- \"TEST\"\nADSL$test <- some_var\nsome_var <- TRUE\nADAE <- radae(cached = some_var)" # nolint
  )
})

# 3. two datasets / global code -------------------------------
test_that("two datasets / datasets code", {
  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL)
  adtte <- cdisc_dataset(dataname = "ADTTE", x = ADTTE)

  expect_error(
    cdisc_data(
      cdisc_dataset(dataname = "ADSL", x = ADSL, code = "ADSL <- radsl(cached = TRUE)"),
      cdisc_dataset(dataname = "ADTTE", x = ADTTE, code = "ADTTE <- radtte(cached = TRUE)"),
      code = "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)",
      check = TRUE
    )
  )

  expect_silent(
    data <- cdisc_data(
      adsl,
      adtte,
      code = "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)",
      check = TRUE
    )
  )

  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)"
  )


  expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)"
  )
  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)"
  )
  expect_error(adsl$check(), "code is empty")
  expect_error(adtte$check(), "code is empty")
  expect_true(data$check())

  # MUTATE
  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL)
  adtte <- cdisc_dataset(dataname = "ADTTE", x = ADTTE)

  expect_silent(
    data <- cdisc_data(
      adsl,
      adtte,
      code = "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)",
      check = TRUE) %>%
      mutate_data(code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
      mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )


  expect_error(adsl$check(), "code is empty")
  expect_error(adtte$check(), "code is empty")
  expect_true(data$check())

  expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)"
  )

  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)\nADSL$x <- 1" #nolint
  )

  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)" #nolint
  )
  expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADSL$x <- 1" #nolint
  )

  expect_reference(
    data$get_dataset("ADSL")$get_raw_data(),
    ADSL
  )
  expect_reference(
    data$get_dataset("ADTTE")$get_raw_data(),
    ADTTE
  )
  data$execute_mutate()

  new_env <- new.env()
  eval(parse(text = data$get_code()), envir = new_env)
  expect_identical(
    get(x = "ADSL", envir = new_env),
    data$get_dataset("ADSL")$get_raw_data()
  )
  expect_identical(
    get(x = "ADTTE", envir = new_env),
    data$get_dataset("ADTTE")$get_raw_data()
  )
})

# 4. dataset + connector / code for dataset -------------------------------
test_that("dataset + connector / global code", {
  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL, code = "ADSL <- radsl(cached = TRUE)")
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, cached = TRUE, ADSL = adsl)

  expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE)
  )

  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )
  expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- radsl(cached = TRUE)"
  )
  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )


  expect_true(data$check())
  expect_silent(load_dataset(adtte))
  expect_true(data$check())

  # MUTATE
  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL, code = "ADSL <- radsl(cached = TRUE)")
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl, cached = TRUE)

  data <- cdisc_data(adsl, adtte, check = TRUE) %>%
    mutate_data(code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
    mutate_dataset(
      dataname = "ADTTE",
      code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
      vars = list(ADSL = adsl)
    ) %>%
    mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")

  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)\nADSL$x <- 1" #nolint
  )
  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)" #nolint
  )
  expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADSL$x <- 1" #nolint
  )
  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)\nADSL$x <- 1" #nolint
  )

  expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )
  expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADSL"),
    "ADSL <- radsl(cached = TRUE)"
  )
  expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )

  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)" #nolint
  )

  expect_true(data$check())
  load_dataset(adtte)
  expect_true(data$check())
})

# 5.dataset + connector / global code
test_that("two datasets / datasets code", {
  adsl <- cdisc_dataset("ADSL", ADSL)
  adrs <- cdisc_dataset("ADRS", ADRS)
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl, cached = TRUE)
  adlb <- rcd_cdisc_dataset_connector("ADLB", radlb, ADSL = adsl, cached = TRUE)

  expect_identical(
    get_code(adtte),
    "ADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )

  data <- cdisc_data(
    adsl,
    adlb,
    adtte,
    adrs,
    code = "ADSL <- radsl(cached = TRUE)\nADRS <- radrs(cached = TRUE)",
    check = TRUE)

  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADRS <- radrs(cached = TRUE)\nADLB <- radlb(ADSL = ADSL, cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)" # nolint
  )

  expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- radsl(cached = TRUE)\nADRS <- radrs(cached = TRUE)"
  )

  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADRS <- radrs(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)" # nolint
  )

  expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADRS <- radrs(cached = TRUE)\nADLB <- radlb(ADSL = ADSL, cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)" # nolint
  )

  expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADSL"),
    "ADSL <- radsl(cached = TRUE)\nADRS <- radrs(cached = TRUE)"
  )

  expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADRS <- radrs(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )

  expect_identical(
    data$get_code_class(FALSE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADRS <- radrs(cached = TRUE)\nADLB <- radlb(ADSL = ADSL, cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)" # nolint
  )

  load_dataset(adtte)
  load_dataset(adlb)
  expect_silent(data$check())

  # MUTATE
  adsl <- cdisc_dataset("ADSL", ADSL)
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl, cached = TRUE)
  data <- cdisc_data(adsl, adtte, code = "ADSL <- radsl(cached = TRUE)", check = TRUE) %>%
    mutate_data(code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
    mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
    mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")

  expect_identical(
    get_code(data),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)\nADSL$x <- 1" #nolint
  )

  expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADSL$x <- 1" #nolint
  )

  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)" #nolint
  )

  expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )
  expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADSL"),
    "ADSL <- radsl(cached = TRUE)"
  )
  expect_identical(
    data$get_code_class(TRUE)$get_code(dataname = "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )

  load_dataset(adtte)
  expect_true(data$check()) # TRUE
  data$execute_mutate()
  expect_identical(
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 231L, ADTTE = 924L)
  )
})

# 5. only connectors ------
test_that("only connectors", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl, cached = TRUE)
  expect_error(
    cdisc_data(adsl, adtte, code = "ADSL <- radsl(cached = TRUE)", check = TRUE),
    "Connectors are reproducible by default and setting 'code' argument might break it"
  )

  expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE) %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
      mutate_dataset(
        dataname = "ADTTE",
        code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
        vars = list(ADSL = adsl)) %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )

  expect_identical(
    data$get_code_class(TRUE)$get_code(),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )
  expect_identical(
    data$get_code_class(TRUE)$get_code("ADSL"),
    "ADSL <- radsl(cached = TRUE)"
  )
  expect_identical(
    data$get_code_class(TRUE)$get_code("ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)"
  )

  expect_identical(
    get_code(data, "ADSL"),
    "ADSL <- radsl(cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADSL$x <- 1"
  )
  expect_identical(
    get_code(data, "ADTTE"),
    "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(ADSL = ADSL, cached = TRUE)\nADSL <- dplyr::filter(ADSL, SEX == \"F\")\nADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)" # nolint
  )

  load_dataset(adsl)
  load_dataset(adtte)
  expect_true(
    data$check()
  )
})

# 6. mutate -----
test_that("only connectors", {
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl, cached = TRUE)
  expect_error(
    cdisc_data(adsl, adtte, code = "ADSL <- radsl(cached = TRUE)", check = TRUE),
    "Connectors are reproducible by default and setting 'code' argument might break it"
  )


})
test_that("Basic example cdisc dataset", {
  simple_cdisc_dataset <- cdisc_dataset("ADSL", ADSL)

  expect_identical(ADSL, simple_cdisc_dataset$data)
  expect_identical("ADSL", simple_cdisc_dataset$get_dataname())
  expect_true(class(simple_cdisc_dataset)[1] == "CDISCDataset")
})

test_that("Basic example - without code and check", {
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE))
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
    cdisc_dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
    cdisc_dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")), code = "", check = FALSE))
})

test_that("Basic example - check overall code", {
  expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      cdisc_dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
      cdisc_dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
      code = "ADSL <- ARG1 <- ARG2 <- cadsl;",
      check = TRUE
    )
  )

  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
      cdisc_dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
      cdisc_dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
      code = "ARG1 <- ARG2 <- cadsl;",
      check = TRUE
    ),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )

  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
      cdisc_dataset(
        dataname = "ARG1",
        x = dplyr::mutate(ADSL, x1 = 1),
        keys = get_cdisc_keys("ADSL"),
        code = "ARG1 <- radsl(cached = TRUE)"
      ),
      cdisc_dataset("ARG2", ADSL, keys = get_cdisc_keys("ADSL"), code = "ARG2 <- radsl(cached = TRUE)"),
      check = TRUE
    ),
    "Reproducibility check failed."
  )
})

test_that("Basic example - dataset depending on other dataset", {
  expect_true(is.data.frame(ADSL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))

  expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- cadsl"),
      check = TRUE
    )
  )
  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
      cdisc_dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL"), code = "ARG1 <- radsl(cached = TRUE)"),
      cdisc_dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL"), code = "ARG2 <- radsl(cached = TRUE)"),
      code = "ADSL <- ARG1 <- ARG2 <- cadsl;",
      check = TRUE
    ),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )

  arg2 <- cdisc_dataset(dataname = "ARG2", x = ARG2, keys = get_cdisc_keys("ADSL"), code = "ARG2 <- cadsl")

  arg1 <- cdisc_dataset(
    dataname = "ARG1",
    x = ARG1,
    keys = get_cdisc_keys("ADSL"),
    code = "ARG1 <- ARG2",
    vars = list(ARG2 = arg2))

  adsl <- cdisc_dataset(dataname = "ADSL", x = ADSL, code = "ADSL <- ARG2", vars = list(ARG2 = arg2))

  expect_silent(cd <- cdisc_data(arg2, arg1, adsl, check = TRUE))

  expect_true(arg1$check())
  expect_true(arg2$check())
  expect_true(adsl$check())
  expect_true(cd$check())
})

test_that("Basic example - failing dataset code", {
  expect_silent(
    dataset <- cdisc_dataset("ADSL", ADSL, code = "ADSL <- data.frame(a = 1, b = 2)")
  )
  expect_false(dataset$check())
})

test_that("Basic example - missing code for dataset", {
  expect_true(is.data.frame(ADSL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))

  expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      code = c("ADSL <- cadsl"),
      check = TRUE
    )
  )

  expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      code = c("ADSL <- cadsl"),
      check = FALSE
    )
  )
})

test_that("Basic example - with line break code and check", {
  expect_true(is.data.frame(ADSL))
  expect_true(is.data.frame(ARG1))
  expect_true(is.data.frame(ARG2))

  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL), code = "ADSL <- cadsl\n ADSL$x1 <- 1", check = FALSE))
})

test_that("Naming list elements", {

  expect_identical(names(get_datasets(cdisc_data(cdisc_dataset("ADSL", ADSL)))), "ADSL")
  expect_identical(
    names(get_datasets(cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      cdisc_dataset("ADTTE", ADTTE),
      cdisc_dataset("ADRS", ADRS)))),
    c("ADSL", "ADTTE", "ADRS"))
})

test_that("List values", {

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

  expect_equal(result, result_to_compare)


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

  expect_equal(result, result_to_compare)
})

test_that("Keys in cached datasets", {
  expect_true(all(get_cdisc_keys("ADSL") %in% names(random.cdisc.data::cadsl)))

  expect_true(all(get_cdisc_keys("ADAE") %in% names(random.cdisc.data::cadae)))

  expect_true(all(get_cdisc_keys("ADTTE") %in% names(random.cdisc.data::cadtte)))

  expect_true(all(get_cdisc_keys("ADCM") %in% names(random.cdisc.data::cadcm)))

  expect_true(all(get_cdisc_keys("ADLB") %in% names(random.cdisc.data::cadlb)))

  expect_true(all(get_cdisc_keys("ADRS") %in% names(random.cdisc.data::cadrs)))

  expect_true(all(get_cdisc_keys("ADVS") %in% names(random.cdisc.data::cadvs)))
})

test_that("Empty code", {

  # missing code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), check = FALSE)
  expect_identical(get_code(result), "")

  # empty code
  result <- cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE)
  expect_identical(get_code(result), "")

  # NULL code
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL), code = NULL, check = FALSE))
})

test_that("Error - objects differs", {
  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL, code = "ADSL <- 2"), check = TRUE),
    "Code from ADSL need to return a data.frame"
  )

  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL, code = "ADSL <- data.frame()"), check = TRUE),
    "Reproducibility check failed."
  )

  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL, code = "ADSL <- mtcars;"), check = TRUE),
    "Reproducibility check failed."
  )
})

test_that("Error - ADSL is missing in cdisc_data", {
  expect_error({
    x <- cdisc_data(cdisc_dataset("ADTTE", ADTTE), code = "ADTTE <- cadtte", check = FALSE)
    x$check_metadata()
  },
  "ADSL dataset is missing."
  )
})

test_that("Error - duplicated names", {
  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      cdisc_dataset("ADSL", ADSL),
      code = "",
      check = FALSE
    ),
    "Datasets names should be unique"
  )
})

test_that("Error - dataset is not of correct class", {
  expect_error(
    cdisc_data(ARG1 = 1, code = "", check = FALSE),
    "All elements should be of Dataset(Connector) or RelationalDataConnector class",
    fixed = TRUE
  )
})

test_that("Check the keys", {
  data1 <- teal_data(dataset(dataname = "ADSL", x = ADSL, keys = "non_existing_column"))
  expect_error(
    data1$check_metadata(),
    "The join key specification requires dataset ADSL to contain the following columns: non_existing_column"
  )

  data2 <- cdisc_data(dataset("ADSL", ADSL), dataset("ADTTE", ADTTE))
  expect_identical(
    data2$get_dataset("ADSL")$get_keys(),
    character(0)
  )
  expect_identical(
    data2$get_dataset("ADTTE")$get_keys(),
    character(0)
  )

  # we can have a empty keys - then we don't check them
  expect_silent(data2$check_metadata())

  ds <- cdisc_dataset("ADSL", ADSL, keys = c("SEX"))
  expect_error(
    ds$check_keys(),
    "Duplicate primary key values found in the dataset 'ADSL'"
  )

  data <- cdisc_data(ds)
  expect_error(
    data$check_metadata(),
    "Duplicate primary key values found in the dataset 'ADSL'"
  )
})

# 7. invalid arguments -----

test_that("Cannot create RelationData if arguments include RelationalData object", {

  c_data <- cdisc_data(
    cdisc_dataset("ADSL", ADSL)
  )

  expect_error(cdisc_data(c_data))
  expect_error(cdisc_data(cdisc_dataset("ADSL", ADSL), c_data))

})
