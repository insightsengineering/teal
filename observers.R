


arm_ref_comp_observer <- function(session, input, output, id_ref, id_comp, id_arm_var, ASL, arm_var, arm_ref_comp) {
 
  if (is(arm_var, "reactiveVal")) {
    observeEvent(arm_var, {
      
    })    
  }

  
  observe({
    
    arm_var <- input[[id_arm_var]]
    arm <- ASL[[arm_var]]
    arms <- if (is.factor(arm)) levels(arm) else unique(arm)
        
    if (length(arms) > 0) {
      
      if (!is.null(arm_ref_comp[[]]))
      
      ref_arm <- arms[1]
      comp_arm <- setdiff(arms, ref_arm)
      
      
    } else {
      ref_arm <- NULL
      comp_arm <- NULL
    }
    
    updateSelectInput(session, "ref_arm", selected = ref_arm, choices = arms)
    updateSelectInput(session, "comp_arm", selected = comp_arm, choices = arms)
  })
  
}