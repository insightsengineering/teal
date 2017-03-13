ui_page_variable_browser <- function(id, datasets) {

  ns <- NS(id)

  div(
    class="row",
    div(
      class="col-md-6",
      # variable browser
      div(class="well", style="background: transparent;",
          do.call(
            tabsetPanel,
            c(id = ns("tsp"),
              do.call(tagList, setNames(lapply(datasets$datanames(), function(domain) {
                ui_id <- paste0('variable_browser_', domain)
                tabPanel(toupper(domain), div(
                  style="margin-top: 15px;",
                  DT::dataTableOutput(ns(ui_id), width="100%")))
              }), NULL)
              )
            )
          )
      )
    ),
    div(
      class="col-md-6",
      div(
        class="well",
        style="padding-bottom: 0px",
        plotOutput(ns("variable_plot"), height="500px"),
        div(
          class="clearfix",
          style="margin-top: 15px;",
          div(
            class="pull-left",
            radioButtons(ns("raw_or_filtered"), NULL, choices = c("unfiltered data"='raw', "filtered data"='filtered'), inline = TRUE)
          ),
          actionLink(ns("add_filter_variable"), "add as filter variable", class="pull-right")
        )
        #           style="margin-top: 15px; margin-bottom: -15px;",
      )
    )
  )
}
