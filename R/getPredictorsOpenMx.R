getPredictorsOpenMx <- function(mxmodel, dataset, covariates)
{
  # specify covariates from model columns
  if (is.null(covariates)) {
    model.ids <- rep(NA, length(mxmodel@manifestVars))
    # find the ids in dataset
    for (i in 1:length(model.ids)) {
      cmp <- mxmodel@manifestVars[i] == names(dataset)
      if (all(!cmp)) {
        ui_stop("Error. Variable ",mxmodel@manifestVars[i], " missing in data set!")
      }
      model.ids[i] <- which(cmp);
    }
    all.ids <- 1:length(names(dataset))
    cvid <- sets::as.set(all.ids)-sets::as.set(model.ids) 
    if (length(cvid)==0) {
      ui_stop("Error. No predictors contained in dataset!")
    }
    covariate.ids <- simplify2array( as.vector(cvid,mode="integer") )
  }
  # resort columns to organize covariates
  else {
    all.ids <- 1:length(names(dataset))
    covariate.ids <- sapply(covariates, function(cv) { which(cv==names(dataset))} )
    
    modid <- sets::as.set(all.ids)-sets::as.set(covariate.ids) 
    if (length(modid)==0) {
      ui_stop("No covariates contained in dataset!")
    }
    
    model.ids <- simplify2array( as.vector(modid, mode="integer") )
  }
  
  return(list(model.ids, covariate.ids))
}