## ---- eval=FALSE---------------------------------------------------------
#  library(teal)
#  
#  ASL <- teal::generate_sample_data("asl")
#  ARS <- teal::generate_sample_data("ars")
#  
#  x <- teal::init(
#    data = list(asl = ASL, ars = ARS),
#    tabs = tabs(
#      tab_item(
#        "data source",
#        server = function(input, output, session, datasets) {},
#        ui = function(id) div(p("data source")),
#        filters = NULL,
#        server_args = list(datasets = "teal_datasets")
#      ),
#      data_table_item(),
#      variable_browser_item(),
#      tabs_item(
#        "analysis items",
#        tabs = tabs(
#          tab_item(
#            "spaghetti plot",
#            server = function(input, output, session, datasets) {},
#            ui = function(id) div(p("spaghetti plot")),
#            filters = 'ars',
#            server_args = list(datasets = "teal_datasets")
#          ),
#          tab_item(
#            "survival curves",
#            server = function(input, output, session) {},
#            ui = function(id) div(p("Kaplan Meier Curve")),
#            filters = "ate"
#          )
#        )
#      )
#    )
#  )
#  
#  shinyApp(ui = x$ui, server = x$server)

## ---- eval=FALSE---------------------------------------------------------
#  library(teal)
#  help(package = "teal")

## ---- eval = FALSE-------------------------------------------------------
#  tab_item(
#    label = "Frequency Table",
#    ui = function(id) {
#      ns <- NS(id)
#      tableOutput(ns("freq_table"))
#    },
#    server = function(input, output, session, datasets) {
#      output$freq_table <- renderTable({
#        asl <- datasets$get_data("asl", reactive = TRUE, filtered=TRUE)
#        table(asl$SEX)
#      })
#    },
#    server_args = list(datasets = "teal_datasets")
#  )

## ---- eval=FALSE---------------------------------------------------------
#  ASL_FILTERED <- subset(ASL, ASL.STUDYID %in% c('BP29392', 'WP29945') & ASL.AGE > 15 & ASL.AGE < 35)
#  ARS_FILTERED_ALONE <- subset(ARS, ARS.ADY > 0 & ARS.ADY < 500)
#  
#  ARS_FILTERED <- merge(x = ASL_FILTERED,
#                        y = ARS_FILTERED_ALONE,
#                        by = c("USUBJID", "STUDYID"), all.x = FALSE, all.y = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  tags$div(tags$h1("My New Analysis Web-app"), tags$p(class="pull-left", "Logo"))

## ---- eval=FALSE---------------------------------------------------------
#  .libPaths(c(normalizePath("./libs"), .libPaths()))

