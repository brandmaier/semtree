getPredictorsLavaan <- function(model, dataset, covariates)
{
  # specify covariates from model columns
  if (is.null(covariates)) {    
    model.ids <- rep(NA, length(model@Data@ov.names[[1]]))
    for (i in 1:length(model.ids)) {
      model.ids[i] <- which(model@Data@ov.names[[1]][i] == names(dataset));
    }
    all.ids <- 1:length(names(dataset))
    cvid <- all.ids[!all.ids %in% model.ids]
    if (length(cvid)==0) {
      ui_stop("No covariates contained in dataset!")
    }
    covariate.ids <- simplify2array( as.vector(cvid,mode="integer") )
  }
  # resort columns to organize covariates
  else {
    all.ids <- 1:length(names(dataset))
    covariate.ids <- sapply(covariates, function(cv) { which(cv==names(dataset))} )
    
    modid <- all.ids[!all.ids %in% covariate.ids]
    if (length(modid)==0) {
      ui_stop("No covariates available to split on!")
    }
    
    model.ids <- simplify2array( as.vector(modid,mode="integer") )
  }
  
  return(list(model.ids, covariate.ids))
}