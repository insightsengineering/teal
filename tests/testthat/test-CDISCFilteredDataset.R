testthat::test_that("The constructor accepts a CDISCTealDataset and an empty list", {
  testthat::expect_error(CDISCFilteredDataset$new(
    dataset = CDISCTealDataset$new(
      "iris",
      head(iris),
      parent = character(0),
      keys = c("Petal.Length")
    )
  ), NA)
})

testthat::test_that("get_call returns a list of calls", {
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCTealDataset$new(
      "iris",
      head(iris),
      parent = character(0),
      keys = c("Petal.Length")
    )
  )
  testthat::expect_true(is_class_list("language")(filtered_dataset$get_call()))
})

testthat::test_that("get_call returns a list call for a CDISCTealDataset with a parent", {
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCTealDataset$new(
      "iris",
      head(iris),
      parent = "test",
      keys = c("Petal.Length")
    )
  )
  testthat::expect_true(is_class_list("language")(filtered_dataset$get_call()))
})

testthat::test_that("get_filter_overview_info returns right array for CDISCFilteredDataset without filtering", {
  adsl <- as.data.frame(as.list(setNames(nm = c(get_cdisc_keys("ADSL")))))
  adsl$sex <- c("F")
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCTealDataset$new(
      "ADSL",
      adsl,
      parent = character(0),
      keys = get_cdisc_keys("ADSL")
    )
  )

  testthat::expect_equal(
    isolate(filtered_dataset$get_filter_overview_info()),
    matrix(list("1/1", "1/1"), nrow = 1, dimnames = list(c("ADSL"), c("Obs", "Subjects")))
  )
})

testthat::test_that("get_filter_overview_info returns right array for CDISCFilteredDataset with filtering", {
  adsl <- as.data.frame(as.list(setNames(nm = c(get_cdisc_keys("ADSL")))))
  adsl$sex <- c("F")
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCTealDataset$new(
      "ADSL",
      adsl,
      parent = character(0),
      keys = get_cdisc_keys("ADSL")
    )
  )
  filter_state_adsl <- ChoicesFilterState$new(c("F", "M"), varname = "sex")
  filter_state_adsl$set_selected("M")

  queue <- filtered_dataset$get_filter_states(1)
  queue$queue_push(filter_state_adsl, queue_index = 1L, element_id = "sex")

  testthat::expect_equal(
    isolate(filtered_dataset$get_filter_overview_info()),
    matrix(list("0/1", "0/1"), nrow = 1, dimnames = list(c("ADSL"), c("Obs", "Subjects")))
  )
})

testthat::test_that("CDISCFilteredDataset$set_filter_state adds desired filter in FilterStates", {
  adsl_df <- as.data.frame(as.list(setNames(nm = c(get_cdisc_keys("ADSL")))))
  adsl <- rbind(adsl_df, adsl_df)
  adsl$sex <- c("F", "M")
  adsl$sex <- factor(adsl$sex, levels = c("M", "F"))
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCTealDataset$new(
      "ADSL",
      adsl,
      parent = character(0),
      keys = get_cdisc_keys("ADSL")
    )
  )
  fs <- list(sex = "M")
  filtered_dataset$set_filter_state(state = fs)

  testthat::expect_equal(
    isolate(filtered_dataset$get_call()),
    list(
      filter = quote(
        ADSL_FILTERED <- dplyr::filter( # nolint
          ADSL,
          sex == "M"
        )
      )
    )
  )
})

testthat::test_that("CDISCFilteredDataset$remove_filter_state removes desired filter in FilterStates", {
  adsl_df <- as.data.frame(as.list(setNames(nm = c(get_cdisc_keys("ADSL")))))
  adsl <- rbind(adsl_df, adsl_df)
  adsl$sex <- c("F", "M")
  adsl$sex <- factor(adsl$sex, levels = c("M", "F"))
  filtered_dataset <- CDISCFilteredDataset$new(
    dataset = CDISCTealDataset$new(
      "ADSL",
      adsl,
      parent = character(0),
      keys = get_cdisc_keys("ADSL")
    )
  )
  fs <- list(sex = "M")
  filtered_dataset$set_filter_state(state = fs)
  filtered_dataset$remove_filter_state("sex")

  testthat::expect_equal(
    isolate(filtered_dataset$get_call()),
    list(
      filter = quote(
        ADSL_FILTERED <- ADSL # nolint
      )
    )
  )
})
