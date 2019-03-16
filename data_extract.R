

choices_selected <- function(choices, selected, multiple, show = FALSE, label = NULL) {
  stopifnot(length(choices)>=1)
  stopifnot(length(selected)>=1)
  stopifnot(is.logical(multiple))
  stopifnot(all(selected %in% choices))
  
  choices %<>% setNames(choices)
  selected %<>% setNames(selected)
  
  list(choices = choices, selected = selected, multiple = multiple, show=show, label=label)
}

keys_filter <- function(vars, choices, selected, multiple, label) {
  stopifnot(is.atomic(vars))
  stopifnot(all(vapply(choices, length, 0) == length(vars)))
  c(vars=list(vars), choices_selected(choices, selected, multiple, label = label))
}

keys_filter_from_sep <- function(vars, sep, choices, selected, multiple, label = "Filter") {
  
  split_by_sep <- function(txt) strsplit(txt, sep, fixed=TRUE)[[1]]
  
  choices <- lapply(choices, split_by_sep)
  choices %<>% setNames(choices)
  
  selected <- lapply(selected, split_by_sep)
  selected %<>% setNames(selected)

  keys_filter(
      vars = vars,
      choices = choices,
      selected = selected,
      multiple = multiple,
      label = label
  )
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

data_extract <- function(dataname = NULL, keys_filtering = NULL, columns = NULL) {
  output <- list(dataname = dataname, keys_filtering = keys_filtering, columns = columns)
  class(output) <- c("list","data_extract")
  return(output)
}

get_selected_columns <- function(data){
  
  dataname <- attr(data, "dataname")
  
  keys <- attr(data, "keys")
  
  leftover <- setdiff(names(data),keys)
  
  new_names <- paste(leftover, dataname, sep=".")
  names(new_names) <- leftover
  
  return(new_names)
}

set_selected_column_names <- function(data){
  
  overwrite_names <- get_selected_columns(data)
  
  names(data)[which(names(data) %in% names(overwrite_names))] <- overwrite_names
  
  return(data)
  
}
# ---- TODO
extracted_data <- function(...){
  
  datasets <- list(...)
  lapply(datasets, function(x){
        if (is.null(dim(x))){
          stop(paste0("The dataset ", attr(x, "dataname"),"does not contain any data in your setup."))
        }
  })
  all_keys <- lapply(datasets, function(dataset) attr(dataset, "keys"))
  
  if (!all(vapply(all_keys, function(keys) identical(keys, all_keys[[1]]), TRUE))){
    stop("The datasets chosen cannot be merged as the keys are not equal.")
  }
  keys <- all_keys[[1]]
  
  datanames <- lapply(datasets, function(dataset) attr(dataset, "dataname")) %>% unlist()
  
  datasets <- lapply(datasets, set_selected_column_names)
  
  # Please add "Dataname"."ColumnName" to data --> Problem, where to get dataname from ?
  merged_data <- purrr::reduce(
      .x = lapply(datasets, function(dataset) dataset),
      .f = left_join,
      by=keys,
      suffix = paste0(".", datanames)
      )
  if(any(duplicated(datanames))){
    
    merged_data <- merged_data[, !duplicated(colnames(merged_data ))]
    
    # Remove duplicated datanames by regular expression
    names(merged_data) <- stringr::str_replace_all(
        string = names(merged_data),
        pattern = "\\b([^\\.]+)\\.(\\1)\\b",
        replacement = c("\\1" = "\\1")
    )
    
  }
  attr(merged_data, "keys") <- keys
  return(merged_data)    
}

data_filter_select <- function(input_data, filters, columns, dataname = ""){
  
  old_keys <- attr(input_data, "keys")
  new_keys <- setdiff(old_keys, filters$variable_names)
  
  if (is.null(filters$filtering_sep)){
    filters$filtering_sep <- "_"
  }
  
  accepted_combinations <- lapply(filters$filters,
      function(comb) paste(comb, collapse=filters$filtering_sep))
  
  if (!is.null(filters$filters)){
    
    input_data %<>%
        tidyr::unite("tmp_keys_to_remove",
            filters$variable_names,
            sep=filters$filtering_sep) %>%
        dplyr::filter(tmp_keys_to_remove %in% accepted_combinations)
    
  }
  
  if (!is.null(columns)){
  
    input_data %<>%
        select(c(new_keys, unlist(columns), recursive=FALSE))
    
  }
  
  attr(input_data, "keys") <- new_keys
  attr(input_data, "dataname") <- dataname
  
  return(input_data)
}