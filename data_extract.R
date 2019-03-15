

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