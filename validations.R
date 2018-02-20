

validate_has_data <- function(x, min_nrow = NULL) {
  validate(need(!is.null(x) && is.data.frame(x), "no data left"))
  
  if (!is.null(min_nrow)) {
    validate(need(nrow(x) >= min_nrow , paste("need more than", min_nrow, "observations")))
  }
  
}

# Also check if USUBJID, STUDYID matches
validate_one_row_per_id <- function(x, key = c("USUBJID", "STUDYID")) {
  validate(need(!any(duplicated(x[key])) , paste("more then one row per id")))
}