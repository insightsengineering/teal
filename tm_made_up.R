tm_made_up <- function(
    label = "Regression Analysis",
    dataname = NULL,
    response,
    regressor,
    facetting=NULL,
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
  arguments <- list(...)
  
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
              value = arguments$regressor
          ),
          data_extract_input(
              id = ns("response"),
              label = "Response Variable",
              value = arguments$response
          ),
          data_extract_input(
              id = ns("facetting"),
              label = "Facetting Variable",
              value = arguments$facetting
          )
      
      )
  )# standard_layout
}

srv_made_up <- function(input, output, session, datasets, response, regressor, facetting) {
  
  
  # data_extractor, "response",
  # dataname + filtering (yes/no) + Names(Filtering-selected) + Names(Columns-Selected) 
  
  response_data   <- callModule(data_extractor, id="response", datasets = datasets, constant_values = response)
  regressor_data  <- callModule(data_extractor, id="regressor", datasets = datasets, constant_values = regressor)
  facetting_data  <- callModule(data_extractor, id="facetting", datasets = datasets, constant_values = facetting)
  
  data_merged <- reactive({
        data_merger(datasets, regressor_data(), response_data())
      })
  
  output$myplot <- renderPlot(
      {
        plot(data = data_merged(), 
            x = get_selected_columns(regressor_data()),
            y = get_selected_columns(response_data())
        )
      }
  )
  
  
}