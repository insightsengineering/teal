
# the arm_ref_comp_observer updates the reference and comparison arms
# when the selected arm variable changes
arm_ref_comp_observer <- function(session, input,
                                  id_ref, id_comp, id_arm_var,
                                  ASL, arm_ref_comp, module) {
  
  check_arm_ref_comp(arm_ref_comp, ASL, module)  ## throws an error if there are issues
  
  observe({
    
    arm_var <- input[[id_arm_var]]
    
    arm <- ASL[[arm_var]]
    arm_levels <- if (is.factor(arm)) levels(arm) else unique(arm)
    
    if (length(arm_levels) > 0) {
      
      default_settings <- arm_ref_comp[[arm_var]]
      
      if (is.null(default_settings)) {
        ref_arm <- arm_levels[1]
        comp_arm <- setdiff(arm_levels, ref_arm)
      } else {
        ref_arm <- default_settings$ref
        comp_arm <- default_settings$comp
      }
    } else {
      ref_arm <- NULL
      comp_arm <- NULL
    }
    
    updateSelectInput(session, id_ref, selected = ref_arm, choices = arm_levels)
    updateSelectInput(session, id_comp, selected = comp_arm, choices = arm_levels)
  })
  
}