library(scda)

# Single scda dataset connector ----
testthat::test_that("Single scda dataset connector", {
  # create object
  adsl <- scda_cdisc_dataset_connector("ADSL", "adsl")
  default_ui <- adsl$get_ui("main-app")
  adsl$set_ui_input(function(ns) {
    list(textInput(inputId = ns("name"), label = "scda name", value = "latest"))
  })
  set_ui <- adsl$get_ui("main-app")
  testthat::expect_false(isTRUE(all.equal(default_ui, set_ui)))

  # check UI
  testthat::expect_equal(
    as.character(set_ui),
    as.character(
      tags$div(
        tags$div(
          id = "main-app-inputs",
          h4("TealDataset Connector for ", code("ADSL")),
          textInput(inputId = "main-app-name", label = "scda name", value = "latest")
        )
      )
    )
  )

  testthat::expect_error(adsl$get_raw_data(), regexp = "'ADSL' has not been pulled yet")
  adsl$pull()

  testthat::expect_s3_class(adsl$get_raw_data(), "data.frame")

  # check reproducible code
  testthat::expect_equal(adsl$get_code(), 'ADSL <- synthetic_cdisc_dataset(dataset_name = "adsl", name = "latest")')
})
