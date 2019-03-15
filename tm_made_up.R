tm_made_up <- function(
    label = "Regression Analysis",
    response,
    regressor,
    facetting,
    pre_output = NULL,
    post_output = NULL){
  
  args <- as.list(environment())
  
  teal::module(
      label = label,
      server = srv_made_up,
      ui = ui_made_up,
      ui_args = args,
      server_args = list(regressor = regressor, response = response, facetting = facetting),
      filters = dataname
  )
}

ui_made_up <- function(id, ...){
  a <- list(...)
  
  ns <- NS(id)
  
  standard_layout(
      output = teal.devel::white_small_well(
          # This shall be wrapped in a teal::plot
          plotOutput(ns("myplot"))
      ),
      encoding = div(
          data_extract_input(
              id = ns("regressor"),
              label = "regressor Variable",
              value = a$regressor
          ),
          data_extract_input(
              id = ns("response"),
              label = "Response Variable",
              value = a$response
          ),
          data_extract_input(
              id = ns("facetting"),
              label = "Facetting Variable",
              value = a$facetting
          )
      
      )
  )# standard_layout
}

srv_made_up <- function(input, output, session, datasets, response, regressor, facetting) {
  
  
  # data_extractor, "response",
  # dataname + filtering (yes/no) + Names(Filtering-selected) + Names(Columns-Selected) 
  
  response_column   <- callModule(data_extractor, id="response", datasets, response)
  regressor_column  <- callModule(data_extractor, id="regressor", datasets, regressor)
  facetting_column  <- callModule(data_extractor, id="facetting", datasets, facetting)
  
  data_merged <- reactive({
        data_merger(datasets, regressor_column(), response_column())
      })
  
  output$myplot <- renderPlot(
      {
        plot(data = data_merged(), 
            x = regressor_column(),
            y = response_column())
      }
  )
  
  
}