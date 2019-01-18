
check_arm_ref_comp <- function(x, ASL, module) {
  
  msg <- paste("module", module, "argument arm_ref_comp ")
  
  if (!is.null(x)) {
    
    if (!is.list(x)) stop(msg, "needs to be a list or NULL")
    
    vars <- names(x)
    if (is.null(vars) || any(vars == "")) stop(msg, "is not named")
  
    if (!all(vars %in% names(ASL))) stop(msg, "refers to variables that are not in ASL")
    
    Map(function(xi, var) {
      if (!is.list(xi)) stop(msg, "definition for arm variable ", var, " list element needs to be lists with ref and comp elements")
        
      rc <- names(xi)
      if (is.null(rc) || !identical(sort(rc), c('comp', 'ref'))) stop(msg, "definition for arm variable ", var , " nested list needs to have the elements ref and comp")

      arm_levels <- unlist(xi)

      if (!all(arm_levels %in% ASL[[var]])) stop(msg, "definition for arm variable ", var , " refers to arm levels that do not exist in ASL[[",var,"]]")
    }, x, vars)
  }

  invisible(TRUE)
  
}
