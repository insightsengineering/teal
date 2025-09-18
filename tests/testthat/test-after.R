test_that("after returns a module", {
  modified_example <-  after(example_module(),
                             ui = function(id, elem) {
                               ns <- NS(id)
                               check_box <- checkboxInput(ns("src"), "Include R Code in the report", TRUE)
                               htmltools::tagAppendChild(elem, check_box, .cssSelector = ".standard-layout .sidebar .sidebar-content")
                             },
                             server = function(input, output, session, data) {
                               teal_card(data) <- c(teal_card(data), teal.reporter::teal_card("Modification"))
                               if (!input$`wrapper-src`) {
                                 teal_card(data) <- Filter(function(x) !inherits(x, "code_chunk"), teal_card(data))
                               }
                               data
                             }
  )

  expect_s3_class(modified_example, "teal_module")
})

