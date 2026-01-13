#' Perform some basic sanity checks on a fitted model
#' Currently suppots lavaan and standard OpenMx models
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
  if (isTRUE(control$exclude.heywood) && containsHeywoodCases(model))  {
    if (control$verbose) {
      message("Model ignored because of Heywood Case")
    }
    return(FALSE); 
  } 
  
  
  if(inherits(model,"lavaan")){
    if(!model@Fit@converged) {
      if( control$verbose ) {
        message("Model ignored because model did not converge")
      }
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (inherits(model,"OpenMx")) {
	
  if (model@output$status[[1]] %in% control$exclude.code) {
    message("Model ignored because of excluded status code")
    return(FALSE);
  } 
    
  }
  
  return(TRUE);
}
