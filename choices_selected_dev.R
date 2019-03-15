

choices_selected <- function(choices, selected, multiple) {
  stopifnot(length(choices)>=1)
  stopifnot(length(selected)>=1)
  stopifnot(is.logical(multiple))
  stopifnot(all(selected %in% choices))
  list(choices=choices, selected=selected, multiple=multiple)
}

column_combinations <- function(vars, choices, selected, multiple) {
  stopifnot(is.atomic(vars))
  stopifnot(all(vapply(choices, length, 0) == length(vars)))
  c(vars=list(vars), choices_selected(choices, selected, multiple))
}

column_combinations_from_sep <- function(vars, sep, cs = choices_selected()) {

  split_by_sep <- function(txt) strsplit(txt, sep, fixed=TRUE)[[1]]
  
  choices <- lapply(cs$choices, split_by_sep)
  choices %<>% setNames(cs$choices)
  
  selected <- lapply(cs$selected, split_by_sep)
  selected %<>% setNames(cs$selected)
  
  column_combinations(
      vars = vars,
      choices = choices,
      selected = selected,
      multiple = cs$multiple
  )
}

variable_choices <- function(cs, show = FALSE, label = NULL) {
  
  cs$choices %<>% setNames(cs$choices)
  cs$selected %<>% setNames(cs$selected)
  
  list(choices = cs$choices, selected = cs$selected, multiple = cs$multiple, show=show, label=label)
}

data_for_teal <- function(ds, keys, source = NULL){
  attr(ds, "keys") <- keys
  attr(ds, "source") <- source
  return(ds)
}

CDISC_data <- function(...){
  all_keys <- lapply(list(...), function(dataset) attr(dataset,"keys"))
  # Code from Max
  # stopifnot(all(vapply(all_keys, function(keys) identical(keys, all_keys[[1]]), TRUE)))
  
  stopifnot(
    all(
      vapply(
       all_keys, function(keys) length(intersect(keys, all_keys[[1]]))>0,TRUE
     )
   )
  )
  
  return(list(...))	
}

data_extract <- function(dataname = NULL, filtering = NULL, vars = NULL) {
  output <- list(dataname = dataname, filtering = filtering, vars = vars)
  class(output) <- c("list","data_extract")
  return(output)
}

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
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#------------------ App Code -----------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
tryCatch(unloadNamespace("teal.devel"))
tryCatch(unloadNamespace("teal"))
devtools::load_all("../teal")
devtools::load_all("../teal.devel")
library(magrittr)
library(random.cdisc.data)
library(dplyr)


ASL <- radsl(N = 600)

ADTE <- radaette(ASL)

modified_data <- ASL %>% mutate(A = 1) 

adte_filters <- column_combinations_from_sep(
    vars = c("PARAMCD", "AVISIT"), # only key variables are allowed
    sep = " - ",
    # Choices selected can be split
#    cs = choices_selected(
        choices = c("CLV - BASELINE", "CLV - VISIT 1", "LTG - BASELINE"),
        selected = "CLV - BASELINE", 
        multiple = FALSE # if multiple, then a spread is needed
#    )
)

# Instead of variable_choices overwrite choices_selected
# backwards compatible with new arguments show and label
adte_extracted1 <- data_extract(
    dataname = "ADTE", 
    filtering = adte_filters,
    vars = variable_choices(
        cs = choices_selected(
            choices =  c("AVAL", "AVALC"),
            selected = c("AVAL", "AVALC"),
            multiple = FALSE
        ),
        # optional
        # - choices
        # - selected
        # - multiple
        show = FALSE, # Whether the user can select the item (optional)
        label = "" # Label the column select dropdown (optional)
    )
)

adte_extracted <- data_extract(
    dataname = "ADTE", 
    filtering = adte_filters,
    vars = variable_choices(
        cs = choices_selected(
            choices =  c("AVAL"),
            selected = c("AVAL"),
            multiple = FALSE
        ),
        show = FALSE, # Whether the user can select the item
        label = "" # Label the column select dropdown (optional)
    )
)

asl_extracted <- data_extract(
    dataname = "ASL", 
    vars = variable_choices(
        cs = choices_selected(
            choices =  c("SEX", "AGE"),
            selected = c("AGE"),
            multiple = FALSE
        )
    )
)

x <- teal::init(
    data = CDISC_data(
        ASL = data_for_teal(
            modified_data,
            c("USUBJID", "STUDYID"),
            "radsl(N = 600)  %>% dplyr::mutate(A = 1)"),
        ADTE = data_for_teal(
            ADTE,
            c("USUBJID", "STUDYID", "PARAMCD", "AVISIT"),
            "radaette(radsl(N = 600))")
    ),
    modules = root_modules(
        tm_made_up(
            label = "Qplot",
            response = adte_extracted,
            regressor = list(
                adte_extracted1,
                asl_extracted
            )
        )
    )
)

shinyApp(x$ui, x$server)