checkModel <- function(model, control)
{
  if(inherits(model,"lavaan")){
    if(!model@Fit@converged) {
      if( control$verbose ) {
        message("Model ignored because model did not converge")
      }
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (control$exclude.heywood == T && containsHeywoodCases(model))  {
    if (control$verbose) {
      message("Model ignored because of Heywood Case")
    }
    return(FALSE); 
  } 
	
  if (model@output$status[[1]] %in% control$exclude.code) {
    message("Model ignored because of excluded status code")
    return(FALSE);
  } 
  
  return(TRUE);
}

#inherits(model1,"lavaan")
#model1@Fit@converged
