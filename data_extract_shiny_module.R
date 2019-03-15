

build_extract_inputs <- function(inputs) {
  do.call(div, inputs)
}

filter2choices <- function(list_of_filters, filtering_sep){

  choices <- unlist(
      lapply(list_of_filters,function(x)paste0(x, collapse=filtering_sep)))
  choices %<>% setNames(choices)
  
}

choices2filter <- function(filter_choices, list_of_filters, filtering_sep, variable_names){
  
  if(is.null(filter_choices) || is.null(list_of_filters)){
    return(NULL)
  }else{
    
    names(list_of_filters) <- filter2choices(list_of_filters, filtering_sep = filtering_sep)
    
    list_of_filters <- list_of_filters[filter_choices]
    out <- lapply(seq_along(variable_names),function(x){
          unlist(lapply(list_of_filters, function(filter_vector){
                out[[variable_names[x]]] <- c(out[[variable_names[x]]], filter_vector[x])
              })
      )
    })

    names(out) <- variable_names
    return(out)
  }
  
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
      if(!is.null(value$keys_filtering)){
        shiny::tagList(
            
          optionalSelectInput(
              inputId = ns("filter"),
              label = "Filter",
              choices = filter2choices(value$keys_filtering$choices, filtering_sep),
              selected = filter2choices(value$keys_filtering$selected, filtering_sep),
              multiple = value$keys_filtering$multiple
          )
         )
      }else{
        hidden(shiny::checkboxInput(inputId=ns("filter")," ",value=FALSE))
      },
      if(value$columns$show){
        optionalSelectInput(
            inputId = ns("column"),
            label = if(is.null(values$columns$label)){
              "Column"
            }else{
              values$columns$label
            },
            choices = value$columns$choices,
            selected = value$columns$selected,
            multiple = value$columns$multiple
        )
      }else{
        helpText("Column:", tags$code(value$columns$selected))
      }
  )
}

get_data_with_keys <- function(datasets, dataname){
  
  data <- datasets$get_data(dataname, reactive = TRUE, filtered = FALSE)
  
  keys_stored <- attr(data, "keys")
  
  data <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
  
  attr(data,"keys") <- keys_stored
  
  return(data)
}

data_extractor <- function(input, output, session, datasets, constant_values){
  
  # Filtering-sep / Filtering-vars / Filtering-choices (inkl mapping) / Columns-choices
  
  data <- reactive({
        
    ns_data <- function(x)paste0(input$ds,"-",x)
    
    data <- get_data_with_keys(datasets = datasets, dataname = input$ds)
        
    constant_values <- constant_values[[
        which(
            unlist(
                lapply(constant_values, function(x){x$dataname == input$ds})
            )
        )
    ]]
    
    if(is.logical(input[[ns_data("filter")]])){
      filters <- NULL      
    }else{
      
      filtering_names <- input[[ns_data("filter")]]
      
      filtering_list <- constant_values$keys_filtering$choices
      
      filters <- choices2filter(
          filter_choices = filtering_names,
          list_of_filters = filtering_list,
          filtering_sep = " - ",
          variable_names = constant_values$keys_filtering$vars
          )
      
    }
    
    data_filter_select(
        data = data,
        filters = filters,
        columns = input[[ns_data("column")]],
        dataname = input$ds
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
