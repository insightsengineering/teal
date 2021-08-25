library(random.cdisc.data)

# Single rcd dataset connector ----
testthat::test_that("Single rcd dataset connector", {
  # create object
  adsl <- rcd_cdisc_dataset_connector("ADSL", radsl, cached = TRUE)
  default_ui <- adsl$get_ui("main-app")
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
  set_ui <- adsl$get_ui("main-app")
  testthat::expect_false(isTRUE(all.equal(default_ui, set_ui)))



  # check UI
  testthat::expect_equal(
    as.character(set_ui),
    as.character(
      tags$div(
        tags$div(
          id = "main-app-inputs",
          h4("Dataset Connector for ", code("ADSL")),
          numericInput(inputId = "main-app-seed", label = "ADSL seed", min = 0, value = 1),
          optionalSliderInput(inputId = "main-app-study_duration",
                              label = "Duration of study in years",
                              min = 0,
                              max = 5,
                              value = 2,
                              step = 1)
        )
      )
    )
  )

  testthat::expect_error(get_raw_data(adsl), regexp = "'ADSL' has not been pulled yet")
  adsl$pull()

  testthat::expect_s3_class(get_raw_data(adsl), "data.frame")

  # check reproducible code
  testthat::expect_equal(
    get_code(adsl), "ADSL <- radsl(cached = TRUE)"
  )
})
