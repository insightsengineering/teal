context("cdisc_data")

library(random.cdisc.data)
ADSL <- ARG1 <- ARG2 <- radsl(cached = TRUE) # nolint
ADTTE <- radtte(cached = TRUE) # nolint
ADRS <- radrs(cached = TRUE) # nolint

# 1. single dataset / dataset code -------------------------------
test_that("single dataset / dataset code", {
  adsl <- cdisc_dataset(dataname = "ADSL", data = ADSL, code = "ADSL <- radsl(cached = TRUE)")
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
})

# 2. two datasets / datasets code -------------------------------
test_that("two datasets / datasets code", {
  adsl <- cdisc_dataset(dataname = "ADSL", data = ADSL, code = "ADSL <- radsl(cached = TRUE)")
  adtte <- cdisc_dataset(dataname = "ADTTE", data = ADTTE, code = "ADTTE <- radtte(cached = TRUE)")

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
  adsl <- cdisc_dataset(dataname = "ADSL", data = ADSL, code = "ADSL <- radsl(cached = TRUE)")
  adtte <- cdisc_dataset(dataname = "ADTTE", data = ADTTE, code = "ADTTE <- radtte(cached = TRUE)")

  expect_silent(
    data <- cdisc_data(adsl, adtte, check = TRUE)  %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
      mutate_dataset(dataname = "ADTTE",
                     code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
                     vars = list(ADSL = adsl)) %>%
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
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 231L, ADTTE = 693L)
  )

})

# 3. two datasets / global code -------------------------------
test_that("two datasets / datasets code", {
  adsl <- cdisc_dataset(dataname = "ADSL", data = ADSL)
  adtte <- cdisc_dataset(dataname = "ADTTE", data = ADTTE)

  expect_error(
    cdisc_data(
      cdisc_dataset(dataname = "ADSL", data = ADSL, code = "ADSL <- radsl(cached = TRUE)"),
      cdisc_dataset(dataname = "ADTTE", data = ADTTE, code = "ADTTE <- radtte(cached = TRUE)"),
      code = "ADSL <- radsl(cached = TRUE)
              ADTTE <- radtte(cached = TRUE)",
      check = TRUE
    )
  )

  expect_silent(
    data <- cdisc_data(
      adsl,
      adtte,
      code = "ADSL <- radsl(cached = TRUE)
              ADTTE <- radtte(cached = TRUE)",
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
  adsl <- cdisc_dataset(dataname = "ADSL", data = ADSL)
  adtte <- cdisc_dataset(dataname = "ADTTE", data = ADTTE)

  expect_silent(
    data <- cdisc_data(adsl, adtte,
                       code = "ADSL <- radsl(cached = TRUE)\nADTTE <- radtte(cached = TRUE)",
                       check = TRUE) %>%
      mutate_data(code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
      mutate_dataset(dataname = "ADTTE",
                     code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
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

  expect_identical(
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 231L, ADTTE = 693L)
  )
})

# 4. dataset + connector / code for dataset -------------------------------
test_that("dataset + connector / global code", {
  adsl <- cdisc_dataset(dataname = "ADSL", data = ADSL, code = "ADSL <- radsl(cached = TRUE)")
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


  expect_error(data$check(), "Cannot check the raw data of 'ADTTE' until it is pulled.")
  expect_silent(load_dataset(adtte))
  expect_true(data$check())

  # MUTATE
  adsl <- cdisc_dataset(dataname = "ADSL", data = ADSL, code = "ADSL <- radsl(cached = TRUE)")
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl, cached = TRUE)

  data <- cdisc_data(adsl, adtte, check = TRUE) %>%
    mutate_data(code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
    mutate_dataset(dataname = "ADTTE",
                   code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
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

  expect_error(data$check(), "Cannot check the raw data of 'ADTTE' until it is pulled.")
  expect_silent(load_datasets(adtte))
  expect_true(data$check())

  expect_identical(
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 231L, ADTTE = 693L)
  )
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

  data <- cdisc_data(adsl, adlb, adtte, adrs,
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

  expect_error(data$check())
  load_dataset(adtte)
  load_dataset(adlb)
  expect_silent(data$check())

  # MUTATE
  adsl <- cdisc_dataset("ADSL", ADSL)
  adtte <- rcd_cdisc_dataset_connector("ADTTE", radtte, ADSL = adsl, cached = TRUE)
  data <- cdisc_data(adsl, adtte, code = "ADSL <- radsl(cached = TRUE)", check = TRUE) %>%
    mutate_data(code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
    mutate_dataset(dataname = "ADTTE",
                   code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
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


  expect_error(data$check(), "'ADTTE' has not been pulled yet")
  load_dataset(adtte)
  expect_true(data$check()) # TRUE
  expect_identical(
    vapply(get_raw_data(data), nrow, integer(1)),
    c(ADSL = 231L, ADTTE = 693L)
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
      mutate_dataset(dataname = "ADSL",
                     code = "ADSL <- dplyr::filter(ADSL, SEX == 'F')") %>%
      mutate_dataset(dataname = "ADTTE",
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

  expect_error(
    data$check(),
    "Cannot check the raw data of 'ADSL' until it is pulled."
  )
  load_dataset(adsl)

  expect_error(
    data$check(),
    "Cannot check the raw data of 'ADTTE' until it is pulled."
  )
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
  expect_identical(ADSL, cdisc_dataset("ADSL", ADSL)$data)
  expect_identical("ADSL", cdisc_dataset("ADSL", ADSL)$get_dataname())
  expect_true(any(class(cdisc_dataset("ADSL", ADSL)) == c("RelationalDataset")))
})

test_that("Basic example - without code and check", {
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL), code = "", check = FALSE))
  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
                           dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")), code = "", check = FALSE))
})

test_that("Basic example - check overall code", {
  expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
      dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
      code = "ADSL <- ARG1 <- ARG2 <- cadsl;",
      check = TRUE
    )
  )

  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
      dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL")),
      dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL")),
      code = "ARG1 <- ARG2 <- cadsl;",
      check = TRUE
    ),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )

  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
      dataset(dataname = "ARG1",
              data = dplyr::mutate(ADSL, x1 = 1),
              keys = get_cdisc_keys("ADSL"), code = "ARG1 <- radsl(cached = TRUE)"),
      dataset("ARG2", ADSL, keys = get_cdisc_keys("ADSL"), code = "ARG2 <- radsl(cached = TRUE)"),
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
      dataset("ARG1", ARG1, keys = get_cdisc_keys("ADSL"), code = "ARG1 <- radsl(cached = TRUE)"),
      dataset("ARG2", ARG2, keys = get_cdisc_keys("ADSL"), code = "ARG2 <- radsl(cached = TRUE)"),
      code = "ADSL <- ARG1 <- ARG2 <- cadsl;",
      check = TRUE
    ),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )

  arg2 <- dataset(dataname = "ARG2",
                  data = ARG2,
                  keys = get_cdisc_keys("ADSL"),
                  code = "ARG2 <- cadsl")

  arg1 <- dataset(dataname = "ARG1",
                  data = ARG1,
                  keys = get_cdisc_keys("ADSL"),
                  code = "ARG1 <- ARG2",
                  vars = list(ARG2 = arg2))

  adsl <- cdisc_dataset(dataname = "ADSL",
                        data = ADSL,
                        code = "ADSL <- ARG2",
                        vars = list(ARG2 = arg2))

  expect_silent(
    cd <- cdisc_data(arg2, arg1, adsl, check = TRUE)
  )

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

  expect_silent(cdisc_data(cdisc_dataset("ADSL", ADSL),
                           code = "ADSL <- cadsl\n ADSL$x1 <- 1",
                           check = FALSE))
})

test_that("Naming list elements", {

  expect_identical(names(get_datasets(cdisc_data(cdisc_dataset("ADSL", ADSL)))), "ADSL")
  expect_identical(names(get_datasets(cdisc_data(cdisc_dataset("ADSL", ADSL),
                                                 cdisc_dataset("ADTTE", ADTTE),
                                                 cdisc_dataset("ADRS", ADRS)))),
                   c("ADSL", "ADTTE", "ADRS"))
})

test_that("List values", {

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL))

  adsl_yaml <- yaml::yaml.load_file(system.file("metadata/ADSL.yml", package = "random.cdisc.data", mustWork = TRUE))
  adtte_yaml <- yaml::yaml.load_file(system.file("metadata/ADTTE.yml", package = "random.cdisc.data", mustWork = TRUE))

  datasets <- list(relational_dataset(
    dataname = "ADSL",
    x = ADSL,
    keys = keys(
      primary = c("STUDYID", "USUBJID"),
      foreign = NULL,
      parent = NULL
    ),
    label = adsl_yaml$domain$label
  ))

  result_to_compare <- do.call("cdisc_data", datasets)

  expect_equal(result, result_to_compare)

  result <- cdisc_data(cdisc_dataset("ADSL", ADSL),
                       cdisc_dataset("ADTTE", ADTTE))

  datasets <- list(
    relational_dataset(
      dataname = "ADSL",
      x = ADSL,
      keys = keys(
        primary = c("STUDYID", "USUBJID"),
        foreign = NULL,
        parent = NULL
      ),
      label = adsl_yaml$domain$label
    ),
    relational_dataset(
      dataname = "ADTTE",
      x = ADTTE,
      keys = keys(
        primary = c("STUDYID", "USUBJID", "PARAMCD"),
        foreign = c("STUDYID", "USUBJID"),
        parent = "ADSL"
      ),
      label = adtte_yaml$domain$label
    ))

  result_to_compare <- do.call("cdisc_data", datasets)

  expect_equal(result, result_to_compare)
})

test_that("Keys in cached datasets", {
  expect_true(all(get_cdisc_keys("ADSL")$primary %in% names(random.cdisc.data::cadsl)))

  expect_true(all(get_cdisc_keys("ADAE")$primary %in% names(random.cdisc.data::cadae)))
  expect_true(all(get_cdisc_keys("ADAE")$foreign %in% names(random.cdisc.data::cadae)))

  expect_true(all(get_cdisc_keys("ADTTE")$primary %in% names(random.cdisc.data::cadtte)))
  expect_true(all(get_cdisc_keys("ADTTE")$foreign %in% names(random.cdisc.data::cadtte)))

  expect_true(all(get_cdisc_keys("ADCM")$primary %in% names(random.cdisc.data::cadcm)))
  expect_true(all(get_cdisc_keys("ADCM")$foreign %in% names(random.cdisc.data::cadcm)))

  expect_true(all(get_cdisc_keys("ADLB")$primary %in% names(random.cdisc.data::cadlb)))
  expect_true(all(get_cdisc_keys("ADLB")$foreign %in% names(random.cdisc.data::cadlb)))

  expect_true(all(get_cdisc_keys("ADRS")$primary %in% names(random.cdisc.data::cadrs)))
  expect_true(all(get_cdisc_keys("ADRS")$foreign %in% names(random.cdisc.data::cadrs)))

  expect_true(all(get_cdisc_keys("ADVS")$primary %in% names(random.cdisc.data::cadvs)))
  expect_true(all(get_cdisc_keys("ADVS")$foreign %in% names(random.cdisc.data::cadvs)))
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
    cdisc_data(cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(N = 10);"), check = TRUE),
    "Reproducibility check failed."
  )
})

test_that("Error - ADSL is missing in cdisc_data", {
  expect_error(
    cdisc_data(cdisc_dataset("ADTTE", ADTTE), code = "ADTTE <- cadtte", check = FALSE),
    "ADSL argument is missing."
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
    "Found duplicated dataset names."
  )
})

test_that("Error - dataset is not of correct class", {
  expect_error(
    cdisc_data(ARG1 = 1, code = "", check = FALSE),
    "All arguments should be of RelationalData(set) or RelationalData(set)Connector class",
    fixed = TRUE
  )
})

test_that("Empty keys for single and multiple datasets", {
  expect_silent(cdisc_data(dataset("ADSL", ADSL)))

  expect_silent(cdisc_data(dataset("ADSL", ADSL), dataset("ADTTE", ADTTE)))
})

test_that("Error - primary keys are not unique for the dataset", {
  expect_error(
    cdisc_data(cdisc_dataset("ADSL", ADSL,
                             keys = keys(primary = c("SEX"),
                                         foreign = NULL,
                                         parent = NULL))),
    "ADSL: Keys don't uniquely distinguish the rows,  i.e. some rows share the same keys")
})

test_that("Error - parent is defined without foreign key", {
  expect_error(cdisc_data(cdisc_dataset("ADTTE", ADTTE, keys = keys(primary = c("STUDYID", "USUBJID"),
                                                                    foreign = NULL,
                                                                    parent = "ADSL"))
  ), "ADTTE: Please specify both foreign keys and a parent!")

  expect_error(
    cdisc_data(cdisc_dataset("ADTTE", ADTTE,
                             keys = keys(primary = c("STUDYID", "USUBJID"),
                                         foreign = c("STUDYID", "USUBJID"),
                                         parent = NULL)
    )), "ADTTE: Please specify both foreign keys and a parent!")

})

test_that("Warning - Different keys names but same length", {
  ADTTE <- dplyr::rename(ADTTE, ADSL_STUDYID = STUDYID, ADSL_USUBJID = USUBJID) # nolint
  expect_warning(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ADTTE", ADTTE,
              keys = keys(primary = c("ADSL_STUDYID", "ADSL_USUBJID", "PARAMCD"),
                          foreign = c("ADSL_STUDYID", "ADSL_USUBJID"),
                          parent = "ADSL"))
    ),
    "Following foreign keys are not identical to the primary keys"
  )
})

test_that("Error - length of child keys > length of parent keys", {
  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ADTTE", ADTTE,
              keys = keys(primary = c("STUDYID", "USUBJID", "PARAMCD"),
                          foreign = c("STUDYID", "USUBJID", "PARAMCD"),
                          parent = "ADSL"))
    ),
    "Number of foreign keys can't be larger than"
  )

  ADTTE <- dplyr::rename(ADTTE, ADSL_STUDYID = STUDYID, ADSL_USUBJID = USUBJID) # nolint
  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ADTTE", ADTTE,
              keys = keys(primary = c("ADSL_STUDYID", "ADSL_USUBJID", "PARAMCD"),
                          foreign = c("ADSL_STUDYID", "ADSL_USUBJID", "PARAMCD"),
                          parent = "ADSL"))
    ),
    "Number of foreign keys can't be larger than"
  )
})

test_that("Error - items dataset to wide (without parent)", {
  COUNTRIES <- data.frame(COUNTRY = unique(ADSL$COUNTRY), VALUE = rnorm(length(unique(ADSL$COUNTRY)))) # nolint

  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("COUNTRIES", COUNTRIES,
              keys = keys(primary = c("COUNTRY"),
                          foreign = c("COUNTRY"),
                          parent = "ADSL"))
    ),
    "Foreign keys are don't match all parent keys and both have different length"
  )

  expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL,
                    keys = keys(primary = c("STUDYID", "USUBJID", "COUNTRY"), foreign = NULL, parent = NULL)),
      dataset(dataname = "COUNTRIES",
              data = COUNTRIES,
              keys = keys(primary = c("COUNTRY"),
                          foreign = c("COUNTRY"),
                          parent = "ADSL"))
    )
  )

})

test_that("Error - Two root datasets with different keys", {
  ADSL2 <- ADSL %>% dplyr::rename(ADSL_STUDYID = STUDYID) # nolint

  expect_error(
    cdisc_data(
      cdisc_dataset("ADSL", ADSL),
      dataset("ADSL2", ADSL2,
              keys = keys(primary = c("ADSL_STUDYID", "USUBJID"),
                          foreign = NULL,
                          parent = NULL)),
      cdisc_dataset("ADTTE", ADTTE)
    ),
    "Root dataset keys doesn't match ADSL primary keys"
  )
})
