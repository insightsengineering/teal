

build_extract_inputs <- function(inputs) {
  do.call(div, inputs)
}

data_extract_input <- function(id = NULL, label = NULL, value = data_extract()){
  
  ns <- NS(id)

  # List of elements
  if(is.null(value$dataname)){
    output_panel <- lapply(value, function(x) {
      dataname = x$dataname
      conditionalPanel(
          condition = paste0("input['",ns("ds"), "'] == '",x$dataname,"'"),
          data_extract_input_single(id = ns(dataname), value = x)
      )
    })#lapply
    datanames <- unlist(lapply(value,function(x) x$dataname))
  }else{
    output_panel <- list(data_extract_input_single(value$dataname, value))
    
    datanames <- value$dataname
  }
  
  datanames %<>% setNames(datanames)
  
  div(
      tags$label(label),
      optionalSelectInput(
          inputId = ns("ds"),
          label = "Dataset",
          choices = datanames,
          selected = datanames[1],
          multiple = FALSE
      ),
      build_extract_inputs(output_panel)    
  )
}

data_extract_input_single <- function(id = NULL, value = data_extract(), filtering_sep = " - "){
  
  ns <- NS(id)
  
  stopifnot(is(value,"data_extract"))

  div(
      if(!is.null(value$filtering)){
        shiny::tagList(
            
          optionalSelectInput(
              inputId = ns("filter"),
              label = "Filter",
              choices = construct_choices(value$filtering$choices, filtering_sep),
              selected = construct_choices(value$filtering$selected, filtering_sep),
              multiple = value$filtering$multiple
          ),
          
          hidden(shiny::selectInput(inputId = ns("vars")," ",
                choices = value$filtering$vars,
                selected = value$filtering$vars)),
         )
      }else{
        hidden(shiny::checkboxInput(inputId=ns("filter")," ",value=FALSE))
      },
      if(value$vars$show){
        optionalSelectInput(
            inputId = ns("column"),
            label = if(is.null(values$vars$label)){
              "Column"
            }else{
              values$vars$label
            },
            choices = value$vars$choices,
            selected = value$vars$selected,
            multiple = value$vars$multiple
        )
      }else{
        helpText("Column:", tags$code(value$vars$selected))
      }
  )
}

get_data_with_keys <- function(datasets, dataname){
  
  data <- datasets$get_data(dataname, reactive = TRUE, filtered = FALSE)
  
  keys_stored <- attr(data$keys)
  
  data <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
  
  attr(data,"keys") <- keys_stored
  
  return(data)
}

data_extractor <- function(input, output, session, datasets, constant_values){
  
  # Filtering-sep / Filtering-vars / Filtering-choices (inkl mapping) / Columns-choices
  response_extract <- keys_filter_from_sep_backward(constant_values)
  
  data <- reactive({
        
    ns_data <- function(x)paste0(input$ds,"-",x)
    
    data <- get_data_with_keys(datasets = datasets, dataname = input$ds)
        
    if(is.logical(input[[ns_data("filter")]])){
      filters <- NULL      
    }else{
      
      filtering_names <- input[[ns_data("filter")]]
      
      filtering_list <- response_extract$filtering_list
      
      filters <- construct_filters(filtering_names, filtering_list)
      
    }
    
    data_filter_select(
        data = data,
        filters = filters,
        columns = input[[ns_data("column")]]
        )
  })
  
  # Merge mit anderem dataset
  return(data)
}

#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#------------------ Module Code --------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
