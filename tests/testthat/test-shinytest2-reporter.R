testthat::skip_if_not_installed("shinytest2")
testthat::skip_if_not_installed("rvest")

testthat::test_that("e2e: reporter tab is visible when reporter is specified (default)", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Module with Reporter")
    )
  )

  testthat::expect_true(app$is_visible(selector = "#teal-reporter_menu_container"))
  app$stop()
})

testthat::test_that("e2e: reporter tab is visible when the teal ui creation is delayed", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    list(
      ui = bslib::page_fluid(
        uiOutput("teal_as_shiny_module")
      ),
      server = function(input, output, session) {
        mods <- modules(
          example_module()
        )
        output$teal_as_shiny_module <- renderUI({
          ui_teal("teal", mods)
        })
        srv_teal("teal", data = teal_data(iris = iris), modules = mods)
      }
    )
  )

  testthat::expect_true(app$is_visible(selector = "#teal-reporter_menu_container"))
  app$stop()
})

testthat::test_that("e2e: reporter card can be customized", {
  skip("TODO")
})

testthat::test_that("e2e: reporter tab is not created if app has no reporter", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = module(),
      reporter = NULL
    )
  )
  testthat::expect_null(app$get_html("#teal-reporter_menu_container"))
  app$stop()
})

testthat::test_that("e2e: adding a report card in a module adds it in the report previewer tab", {
  skip_if_too_deep(5)
  app <- TealAppDriver$new(
    init(
      data = simple_teal_data(),
      modules = example_module(label = "Module with Reporter")
    )
  )

  # Add new card with label and comment
  app$click(app$namespaces()$base_id("add_reporter_wrapper-reporter_add-add_report_card_button"))

  app$set_input(
    app$namespaces()$base_id("add_reporter_wrapper-reporter_add-label"),
    "Card name"
  )
  app$click(app$namespaces()$base_id("add_reporter_wrapper-reporter_add-add_card_ok"))

  # Check whether card was added
  app$run_js("document.querySelector('#teal-preview_report-preview_button').click();") # skipping menu hovering
  app$wait_for_idle()
  accordion_selector <- "#teal-preview_report-preview_content-reporter_cards"
  testthat::expect_match(app$get_text(selector = paste(accordion_selector, ".accordion-title")), "Card name")
  app$stop()
})
