#' Perform some basic sanity checks on a fitted model
#' Currently supports lavaan and standard OpenMx models
#' 
#' First, check whether any variance estimates are negative (Heywood case)
#' Second, check whether model converged
#' 
#' Otherwise return TRUE
#' 
#' @param model A lavaan or OpenMx model
#' @param control  A semtree_control object
#'
#' @noRd
checkModel <- function(model, control)
{
  if (is.null(model)) return(FALSE)
  
  if (isTRUE(control$exclude.heywood) && containsHeywoodCases(model))  {
    if (control$verbose) {
      message("Model ignored because of Heywood Case")
    }
    return(FALSE); 
  } 
  
  
  if (inherits(model,"OpenMx")) {
    
    if (model@output$status[[1]] %in% control$exclude.code) {
      message("Model ignored because of excluded status code")
      return(FALSE);
    } 
    

  }
  
  if (control$check.convergence) {
      
      if (!isTRUE(hasConverged(model))) return(FALSE)
    
  }
 
  

  
  return(TRUE);
}
