

choices_selected <- function(choices, selected, multiple) {
  stopifnot(length(choices)>=1)
  stopifnot(length(selected)>=1)
  stopifnot(is.logical(multiple))
  stopifnot(all(selected %in% choices))
  list(choices=choices, selected=selected, multiple=multiple)
}

keys_filter <- function(vars, choices, selected, multiple) {
  stopifnot(is.atomic(vars))
  stopifnot(all(vapply(choices, length, 0) == length(vars)))
  c(vars=list(vars), choices_selected(choices, selected, multiple))
}

keys_filter_from_sep <- function(vars, sep, choices, selected, multiple) {
  
  split_by_sep <- function(txt) strsplit(txt, sep, fixed=TRUE)[[1]]
  
  choices <- lapply(choices, split_by_sep)
  choices %<>% setNames(choices)
  
  selected <- lapply(selected, split_by_sep)
  selected %<>% setNames(selected)
  
  keys_filter(
      vars = vars,
      choices = choices,
      selected = selected,
      multiple = multiple
  )
}

column_filter <- function(cs, show = FALSE, label = NULL) {
  
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

data_extract <- function(dataname = NULL, keys_filtering = NULL, columns = NULL) {
  output <- list(dataname = dataname, keys_filtering = keys_filtering, columns = columns)
  class(output) <- c("list","data_extract")
  return(output)
}

get_selected_columns <- function(data, columnname){
  
  dataname <- attr(data, "dataname")
  
  keys <- attr(data, "keys")
  
  leftover <- setdiff(names(data),"keys")
  
  return(paste0(dataname, leftover, sep="."))
}

# ---- TODO
data_merger <- function(...){
  
  # Please double check keys
  # Please add "Dataname"."ColumnName" to data --> Problem, where to get dataname from ?
  datasets <- list(...)
  keys <- attr(datasets[[1]],"keys")
  merge(..., by = keys)
  
}

# ---- TODO
data_filter_select <- function(data, filters, columns, dataname = ""){
  
  # Please return data with attr(.., "keys") and attr(..., "dataname")
  
  return(data)
}